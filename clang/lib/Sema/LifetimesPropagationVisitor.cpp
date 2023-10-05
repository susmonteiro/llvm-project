#include "clang/Sema/LifetimesPropagationVisitor.h"

namespace clang {

void TransferFuncCall(const clang::VarDecl *lhs,
                      unsigned int lhs_num_indirections,
                      unsigned int func_num_indirections,
                      clang::TypeToSet &call_info, const clang::Stmt *loc,
                      PointsToMap &PointsTo,
                      LifetimeAnnotationsAnalysis &state) {
  while (lhs_num_indirections > 0 && func_num_indirections > 0) {
    if (state.IsLifetimeNotset(lhs, lhs_num_indirections)) {
      auto &current_type_call_info = call_info[func_num_indirections];

      if (current_type_call_info.is_local) {
        state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                       func_num_indirections, LOCAL);

      } else if (current_type_call_info.is_static) {
        state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                       func_num_indirections, STATIC);
                                       
      } else {
        for (const auto &[arg, arg_num_indirections] :
             current_type_call_info.info) {
          const auto &arg_points_to = PointsTo.GetExprDecls(arg);
          for (const auto &decl : arg_points_to) {
            state.CreateDependencySimple(lhs, lhs_num_indirections, decl,
                                         arg_num_indirections, loc);
          }
        }
      }
    }
    lhs_num_indirections--;
    func_num_indirections--;
  }
}

void TransferFuncCall(const clang::VarDecl *lhs,
                      const clang::CallExpr *call_expr,
                      unsigned int lhs_num_indirections, const clang::Stmt *loc,
                      PointsToMap &PointsTo,
                      LifetimeAnnotationsAnalysis &state) {
  auto &call_info = PointsTo.GetCallExprInfo(call_expr);
  unsigned int func_num_indirections =
      Lifetime::GetNumIndirections(call_expr->getType().getCanonicalType());
  TransferFuncCall(lhs, lhs_num_indirections, func_num_indirections, call_info,
                   loc, PointsTo, state);
}

void TransferDeadLifetime(const Expr *arg, const Stmt *loc,
                          unsigned int num_indirections, PointsToMap &PointsTo,
                          LifetimeAnnotationsAnalysis &state) {
  if (arg == nullptr) return;
  const clang::VarDecl *arg_decl;
  if (clang::isa<clang::MemberExpr>(arg)) {

    auto &points_to = PointsTo.GetExprDecls(arg);
    arg_decl = *points_to.begin();
    num_indirections = 1;
  } else if (const auto *arg_decl_ref_expr =
                 dyn_cast<clang::DeclRefExpr>(arg)) {
    if (!(arg_decl = dyn_cast<clang::VarDecl>(arg_decl_ref_expr->getDecl()))) {
      return;
    }
  } else {
    return;
  }
  Lifetime &arg_lifetime = state.GetLifetime(arg_decl, num_indirections);
  arg_lifetime.SetDead();
  arg_lifetime.InsertDependencies(DEAD, loc);
}

void TransferMemberExpr(const clang::VarDecl *lhs,
                        const clang::MemberExpr *member_expr,
                        unsigned int lhs_num_indirections,
                        const clang::Stmt *loc, PointsToMap &PointsTo,
                        LifetimeAnnotationsAnalysis &state) {

  const unsigned int rhs_num_indirections = 1;
  auto &rhs_points_to = PointsTo.GetExprDecls(member_expr);

  if (rhs_points_to.size() == 1) {
    for (const auto *rhs_decl : rhs_points_to) {
      state.CreateDependency(lhs, lhs_num_indirections, rhs_decl,
                             rhs_num_indirections, loc);
    }
  } else if (PointsTo.HasCallExprInfo(member_expr)) {
    auto &call_info = PointsTo.GetCallExprInfo(member_expr);
    TransferFuncCall(lhs, lhs_num_indirections, rhs_num_indirections, call_info,
                     loc, PointsTo, state);
  }
}

void TransferRHS(const clang::VarDecl *lhs, const clang::Expr *rhs,
                 unsigned int lhs_num_indirections,
                 unsigned int rhs_num_indirections, const clang::Stmt *loc,
                 PointsToMap &PointsTo, LifetimeAnnotationsAnalysis &state) {
  char maybe_lifetime = PointsTo.GetExprLifetime(rhs);
  if (maybe_lifetime != NOTSET) {
    state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                   rhs_num_indirections, maybe_lifetime);
  } else if (const auto *call_expr = clang::dyn_cast<clang::CallExpr>(rhs)) {
    TransferFuncCall(lhs, call_expr, lhs_num_indirections, loc, PointsTo,
                     state);
  } else if (const auto *member_expr =
                 clang::dyn_cast<clang::MemberExpr>(rhs)) {
    TransferMemberExpr(lhs, member_expr, lhs_num_indirections, loc, PointsTo,
                       state);
  }

  const auto &points_to_expr = PointsTo.GetExprPointsTo(rhs);
  for (const auto &expr : points_to_expr) {
    if (expr != nullptr) {
      char maybe_lifetime = PointsTo.GetExprLifetime(expr);
      if (maybe_lifetime != NOTSET) {
        state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                       rhs_num_indirections, maybe_lifetime);
      } else if (const auto *call_expr =
                     clang::dyn_cast<clang::CallExpr>(expr)) {
        TransferFuncCall(lhs, call_expr, lhs_num_indirections, loc, PointsTo,
                         state);
      } else if (const auto *member_expr =
                     clang::dyn_cast<clang::MemberExpr>(expr)) {
        TransferMemberExpr(lhs, member_expr, lhs_num_indirections, loc,
                           PointsTo, state);
      }
    }
  }

  const auto &points_to_decl = PointsTo.GetExprDecls(rhs);

  for (const auto &decl : points_to_decl) {
    state.CreateDependency(lhs, lhs_num_indirections, decl,
                           rhs_num_indirections, loc);
  }
}

void LifetimesPropagationVisitor::PropagateBinAssign(
    const clang::Expr *lhs, const clang::Expr *rhs, const clang::Expr *expr,
    const clang::BinaryOperator *op) const {

  if (expr == nullptr) return;

  const clang::VarDecl *lhs_decl;
  unsigned int base_num_indirections;
  if (clang::isa<clang::MemberExpr>(expr)) {
    auto &points_to = PointsTo.GetExprDecls(expr);
    lhs_decl = *points_to.begin();
    base_num_indirections = 1;
  } else if (const auto *lhs_decl_ref_expr =
                 dyn_cast<clang::DeclRefExpr>(expr)) {
    if (!(lhs_decl = dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl()))) {
      return;
    }
    base_num_indirections =
        Lifetime::GetNumIndirections(lhs->getType().getCanonicalType());
  } else {
    return;
  }
  TransferRHS(lhs_decl, rhs, base_num_indirections, base_num_indirections, op,
              PointsTo, State);
}

std::optional<std::string> LifetimesPropagationVisitor::VisitArraySubscriptExpr(
    const clang::ArraySubscriptExpr *expr) {
  if (debugEnabled) debugLifetimes("[VisitArraySubscriptExpr]");
  Visit(const_cast<clang::Expr *>(expr->getIdx()));
  
  const auto *base = expr->getBase()->IgnoreParens();
  Visit(const_cast<clang::Expr *>(base));
  PointsTo.InsertPointsTo(expr, base);
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();

  if (!lhs->getType()->isPointerType() && !lhs->getType()->isReferenceType()) {
    return std::nullopt;
  }

  const auto &rhs = op->getRHS()->IgnoreParens();
  Visit(lhs);
  Visit(rhs);

  PointsTo.InsertExprPointsTo(op, lhs);
  PointsTo.InsertExprDecl(op, lhs);

  PointsTo.InsertExprPointsTo(op, rhs);
  PointsTo.InsertCallExprInfo(op, rhs);

  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);

  PropagateBinAssign(lhs, rhs, lhs, op);
  for (const auto &expr : lhs_points_to) {
    PropagateBinAssign(lhs, rhs, expr, op);
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
  if (debugEnabled) debugLifetimes("[VisitCallExpr]");
  // No need to check the arguments lifetimes because there is always a min
  // lifetime between any set of lifetimes

  const clang::FunctionDecl *direct_callee = call->getDirectCallee();
  if (direct_callee) {

    PointsTo.InsertExprPointsTo(call, nullptr);

    clang::QualType func_type = direct_callee->getReturnType().IgnoreParens();
    auto &all_func_info = Analyzer->GetFunctionInfo();

    auto it = all_func_info.find(direct_callee);
    if (it == all_func_info.end()) {
      Analyzer->GetLifetimes(direct_callee);
    }

    auto &func_info = all_func_info[direct_callee];

    // * process dead args
    unsigned int param_idx = 0;
    for (unsigned int arg_idx = 0; arg_idx < direct_callee->getNumParams();
         arg_idx++) {
      const auto *arg = call->getArg(arg_idx);
      clang::QualType arg_type = arg->getType();
      if (!arg_type->isPointerType() && !arg_type->isReferenceType()) {
        continue;
      }
      Visit(const_cast<clang::Expr *>(arg));
      PointsTo.InsertCallExprPointsTo(call, arg);

      const auto &arg_points_to = PointsTo.GetExprPointsTo(arg);

      int num_indirections = Lifetime::GetNumIndirections(arg->getType());
      if (num_indirections < 2) {
        param_idx++;
        continue;
      }

      const auto *param = func_info.GetParam(param_idx);
      clang::QualType param_type = param->getType()->getPointeeType();

      while (--num_indirections > 0) {
        if (param_type.isConstQualified()) {
          param_type->getPointeeType();
          continue;
        }
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(param_idx, num_indirections);
        if (param_lifetime.IsLocal()) {
          TransferDeadLifetime(arg, call, num_indirections, PointsTo, State);
          for (const auto &expr : arg_points_to) {
            TransferDeadLifetime(expr, call, num_indirections, PointsTo, State);
          }
        }
        param_type->getPointeeType();
      }
      param_idx++;
    }

    if (!func_type->isPointerType() && !func_type->isReferenceType()) {
      for (unsigned int arg_idx = 0; arg_idx < direct_callee->getNumParams();
           arg_idx++) {
        const Expr *arg = call->getArg(arg_idx)->IgnoreParens();
        clang::QualType arg_type = arg->getType();
        if (!arg_type->isPointerType() && !arg_type->isReferenceType()) {
          continue;
        }
      }
      return std::nullopt;
    }

    ObjectLifetimes return_ol = func_info.GetReturnLifetime();
    auto &call_info = PointsTo.GetCallExprInfo(call);
    unsigned int func_num_indirections =
        Lifetime::GetNumIndirections(func_type);
    while (func_num_indirections > 0) {
      Lifetime &return_lifetime = return_ol.GetLifetime(func_type);

      auto &current_type_call_info = call_info[func_num_indirections];
      current_type_call_info.call_expr = call;

      if (return_lifetime.IsNotSet()) {
        current_type_call_info.is_local = true;

      } else if (return_lifetime.IsStatic()) {
        current_type_call_info.is_static = true;

      } else {
        unsigned int param_idx = 0;

        for (unsigned int arg_idx = 0; arg_idx < direct_callee->getNumParams();
             arg_idx++) {
          const Expr *arg = call->getArg(arg_idx)->IgnoreParens();
          clang::QualType arg_type = arg->getType();
          if (!arg_type->isPointerType() && !arg_type->isReferenceType()) {
            continue;
          }

          const clang::ParmVarDecl *param = func_info.GetParam(param_idx);
          ObjectLifetimes &param_ol = func_info.GetParamLifetime(param);

          for (Lifetime &param_lifetime : param_ol.GetLifetimes()) {
            if (param_lifetime == return_lifetime) {
              current_type_call_info.info.insert(
                  {arg, param_lifetime.GetNumIndirections()});
            }
          }
          param_idx++;
        }
      }
      func_num_indirections--;
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitCastExpr(
    const clang::CastExpr *cast) {
  if (debugEnabled) debugLifetimes("[VisitCastExpr]");
  switch (cast->getCastKind()) {
    case clang::CK_LValueToRValue:
    // ---
    case clang::CK_BitCast:
    case clang::CK_LValueBitCast:
    case clang::CK_IntegralCast:
    case clang::CK_ArrayToPointerDecay:
    case clang::CK_IntegralToPointer:
    case clang::CK_NoOp:
    case clang::CK_ToVoid:
    case clang::CK_IntegralToFloating: {
      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));
        if (!cast->getType()->isPointerType() &&
            !cast->getType()->isReferenceType()) {
          continue;
        }
        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          PointsTo.InsertPointsTo(cast, child_expr);
        }
      }
      break;
    }
    case clang::CK_NullToPointer:
    case clang::CK_FunctionToPointerDecay:
    case clang::CK_BuiltinFnToFnPtr:
    case clang::CK_UserDefinedConversion:
    case clang::CK_DerivedToBase:
    case clang::CK_UncheckedDerivedToBase:
    case clang::CK_BaseToDerived:
    case clang::CK_Dynamic: {
      break;
    }
    default: {
      if (cast->isGLValue() ||
          cast->getType().getCanonicalType()->isPointerType()) {
        llvm::errs() << "Unknown cast type:\n";
        llvm::report_fatal_error("unknown cast type encountered");
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string>
LifetimesPropagationVisitor::VisitConditionalOperator(
    const clang::ConditionalOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitConditionalOperator]");

  const auto *true_expr = op->getTrueExpr();
  Visit(const_cast<clang::Expr *>(true_expr));
  PointsTo.InsertPointsTo(op, true_expr);

  const auto *false_expr = op->getFalseExpr();
  Visit(const_cast<clang::Expr *>(false_expr));
  PointsTo.InsertPointsTo(op, false_expr);

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  if (debugEnabled) debugLifetimes("[VisitDeclRefExpr]");

  if (!decl_ref->isGLValue() && !decl_ref->getType()->isBuiltinType()) {
    return std::nullopt;
  }

  auto *decl = decl_ref->getDecl();
  if (!clang::isa<clang::VarDecl>(decl) &&
      !clang::isa<clang::FunctionDecl>(decl)) {
    return std::nullopt;
  }

  if (!decl_ref->isGLValue() && !decl_ref->getType()->isBuiltinType()) {
    return std::nullopt;
  }

  PointsTo.InsertExprPointsTo(decl_ref, nullptr);
  if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
    State.CreateVariableIfNotFound(var_decl);
    PointsTo.InsertExprDecl(decl_ref, var_decl);
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  if (debugEnabled) debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      clang::QualType lhs_type = var_decl->getType().getCanonicalType();

      if (var_decl->isStaticLocal()) {
        ObjectLifetimes objectLifetimes(
            true, Lifetime::GetNumIndirections(lhs_type) + 2);
        State.CreateVariable(var_decl, objectLifetimes);
        return std::nullopt;
      }

      if (var_decl->isFunctionPointerType()) {
        ObjectLifetimes objectLifetimes(
            true, Lifetime::GetNumIndirections(lhs_type) + 1);
        State.CreateVariable(var_decl, objectLifetimes);
        return std::nullopt;
      }

      if (!lhs_type->isPointerType() && !lhs_type->isReferenceType()) {
        continue;
      }

      ObjectLifetimes objectLifetimes =
          State.GetVarDeclLifetime(var_decl, State.GetLifetimeFactory());
      State.CreateVariable(var_decl, objectLifetimes);

      if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
        const clang::Expr *init = var_decl->getInit()->IgnoreParens();
        Visit(const_cast<clang::Expr *>(init));
        unsigned int lhs_num_indirections =
            Lifetime::GetNumIndirections(lhs_type);
        TransferRHS(var_decl, init, lhs_num_indirections, lhs_num_indirections,
                    decl_stmt, PointsTo, State);
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitExpr(
    const clang::Expr *expr) {
  if (debugEnabled) debugLifetimes("[VisitExpr]");
  for (const auto &child : expr->children()) {
    if (child == nullptr) continue;
    Visit(const_cast<clang::Stmt *>(child));
    if (const auto *child_expr = clang::dyn_cast<clang::Expr>(child)) {
      PointsTo.InsertPointsTo(expr, child_expr);
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitMemberExpr(
    const clang::MemberExpr *member_expr) {
  if (debugEnabled) debugLifetimes("[VisitMemberExpr]");
  const clang::Expr *base = member_expr->getBase();
  Visit(const_cast<clang::Expr *>(base));
  PointsTo.InsertCallExprInfo(member_expr, base);
  auto &points_to = PointsTo.GetExprDecls(base);

  if (points_to.size() < 1) return std::nullopt;
  PointsTo.InsertExprDecl(member_expr, base);
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  if (debugEnabled) debugLifetimes("[VisitReturnStmt]");

  const auto *return_value = return_stmt->getRetValue();
  if (return_value == nullptr) {
    return std::nullopt;
  }

  Visit(const_cast<clang::Expr *>(return_value));
  clang::QualType return_type = Func->getReturnType().IgnoreParens();
  if (!return_type->isPointerType() || return_type->isReferenceType()) {
    return std::nullopt;
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  if (debugEnabled) debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    if (child == nullptr) continue;
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitUnaryAddrOf(
    const clang::UnaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitUnaryAddrOfOperator]");

  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
    if (const auto *child_expr = dyn_cast<clang::Expr>(child)) {
      PointsTo.InsertPointsTo(op, child_expr);
      if (clang::isa<clang::ArraySubscriptExpr>(child_expr)) {
        return std::nullopt;
      }

      if (const auto &member_expr =
              clang::dyn_cast<clang::MemberExpr>(child_expr)) {
        if (member_expr->isArrow()) {
          return std::nullopt;
        }
      }
    }
  }

  for (const auto *child : PointsTo.GetExprPointsTo(op)) {
    if (child != nullptr && clang::isa<clang::ArraySubscriptExpr>(child)) {
      clang::QualType child_type = child->getType();
      if (child_type->isPointerType() || child_type->isReferenceType()) {
        return std::nullopt;
      }
    }
  }

  for (const auto *decl : PointsTo.GetExprDecls(op)) {
    if (decl->isStaticLocal() || decl->isDefinedOutsideFunctionOrMethod() ||
        State.GetObjectLifetimes(decl).HasLifetimeStatic()) {
      PointsTo.InsertExprLifetime(op, STATIC);
      return std::nullopt;
    }
  }

  PointsTo.InsertExprLifetime(op, LOCAL);

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitUnaryOperator(
    const clang::UnaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitUnaryOperator]");

  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
    if (!op->isGLValue() && !op->getType()->isPointerType() &&
        !op->getType()->isReferenceType() && !op->getType()->isArrayType()) {
      return std::nullopt;
    }

    if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
      PointsTo.InsertPointsTo(op, child_expr);
    }
  }
  return std::nullopt;
}

}  // namespace clang
