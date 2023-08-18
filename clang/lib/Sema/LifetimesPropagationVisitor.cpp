#include "clang/Sema/LifetimesPropagationVisitor.h"

namespace clang {

void TransferFuncCall(const clang::VarDecl *lhs, clang::QualType lhs_type,
                      clang::QualType func_type, clang::TypeToSet &call_info,
                      const clang::Stmt *loc, PointsToMap &PointsTo,
                      LifetimeAnnotationsAnalysis &state) {
  unsigned int lhs_num_indirections = Lifetime::GetNumIndirections(lhs_type);
  unsigned int func_num_indirections = Lifetime::GetNumIndirections(func_type);

  while (lhs_num_indirections > 0 && func_num_indirections > 0) {
    if (state.IsLifetimeNotset(lhs, lhs_num_indirections)) {
      auto &current_type_call_info = call_info[func_num_indirections];
      if (current_type_call_info.is_local) {
        state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                       func_num_indirections, LOCAL);
        state.CreateStmtLifetime(loc, LOCAL);
      } else if (current_type_call_info.is_static) {
        state.CreateLifetimeDependency(lhs, lhs_num_indirections, loc,
                                       func_num_indirections, STATIC);
        // TODO delete stmt lifetimes
        state.CreateStmtLifetime(loc, STATIC);
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
                      clang::QualType lhs_type, const clang::Stmt *loc,
                      PointsToMap &PointsTo,
                      LifetimeAnnotationsAnalysis &state) {
  auto &call_info = PointsTo.GetCallExprInfo(call_expr);
  clang::QualType func_type = call_expr->getType().getCanonicalType();
  TransferFuncCall(lhs, lhs_type, func_type, call_info, loc, PointsTo, state);
}

void TransferDeadLifetime(const Expr *arg, const Stmt *loc,
                          unsigned int num_indirections, PointsToMap &PointsTo,
                          LifetimeAnnotationsAnalysis &state) {
  if (arg == nullptr) return;
  const clang::VarDecl *arg_decl;
  if (clang::isa<clang::MemberExpr>(arg)) {
    auto &points_to = PointsTo.GetExprDecls(arg);
    // TODO delete this
    assert(points_to.size() == 1 && "Handle multiple points to in MemberExpr");
    arg_decl = *points_to.begin();
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
  arg_lifetime.InsertPossibleLifetimes(DEAD, loc);
}

void TransferMemberExpr(const clang::VarDecl *lhs,
                        const clang::MemberExpr *member_expr,
                        clang::QualType lhs_type, clang::QualType rhs_type,
                        const clang::Stmt *loc, PointsToMap &PointsTo,
                        LifetimeAnnotationsAnalysis &state) {
  debugInfo("TransferMemberExpr");
  auto &rhs_points_to = PointsTo.GetExprDecls(member_expr);
  // debugLifetimes("rhs_points_to.size()", rhs_points_to.size());
  if (rhs_points_to.size() == 1) {
    for (const auto *rhs_decl : rhs_points_to) {
      state.CreateDependency(lhs, lhs_type, rhs_decl, rhs_type, loc);
    }
  } else if (PointsTo.HasCallExprInfo(member_expr)) {
    auto &call_info = PointsTo.GetCallExprInfo(member_expr);
    TransferFuncCall(lhs, lhs_type, rhs_type, call_info, loc, PointsTo, state);
  } else {
    // TODO uncomment assert
    assert(false && "Should not reach here");
  }
}

void TransferRHS(const clang::VarDecl *lhs, const clang::Expr *rhs,
                 clang::QualType lhs_type, clang::QualType base_type,
                 const clang::Stmt *loc, PointsToMap &PointsTo,
                 LifetimeAnnotationsAnalysis &state) {
  // debugLifetimes("TransferRHS", lhs->getNameAsString());
  clang::QualType rhs_type = PointsTo.GetExprType(rhs);
  rhs_type = rhs_type.isNull() ? base_type : rhs_type;

  char maybe_lifetime = PointsTo.GetExprLifetime(rhs);
  if (maybe_lifetime != NOTSET) {
    state.CreateLifetimeDependency(lhs, lhs_type, loc, rhs_type,
                                   maybe_lifetime);
    state.CreateStmtLifetime(loc, LOCAL);
  } else if (const auto *call_expr = clang::dyn_cast<clang::CallExpr>(rhs)) {
    TransferFuncCall(lhs, call_expr, lhs_type, loc, PointsTo, state);
  } else if (const auto *member_expr =
                 clang::dyn_cast<clang::MemberExpr>(rhs)) {
    TransferMemberExpr(lhs, member_expr, lhs_type, rhs_type, loc, PointsTo,
                       state);
  }

  const auto &points_to_expr = PointsTo.GetExprPointsTo(rhs);
  for (const auto &expr : points_to_expr) {
    if (expr != nullptr) {
      char maybe_lifetime = PointsTo.GetExprLifetime(expr);
      if (maybe_lifetime != NOTSET) {
        state.CreateLifetimeDependency(lhs, lhs_type, loc, rhs_type,
                                       maybe_lifetime);
        state.CreateStmtLifetime(loc, LOCAL);
      } else if (const auto *call_expr =
                     clang::dyn_cast<clang::CallExpr>(expr)) {
        TransferFuncCall(lhs, call_expr, lhs_type, loc, PointsTo, state);
      } else if (const auto *member_expr =
                     clang::dyn_cast<clang::MemberExpr>(expr)) {
        TransferMemberExpr(lhs, member_expr, lhs_type, rhs_type, loc, PointsTo,
                           state);
      }
    }
  }

  const auto &points_to_decl = PointsTo.GetExprDecls(rhs);

  // TODO check if the callexpr or memberexpr first
  // TODO these should not be taken from GetExprDecls but instead from
  // GetExprPointsTo

  for (const auto &decl : points_to_decl) {
    // TODO most recent decl
    state.CreateDependency(lhs, lhs_type, decl, rhs_type, loc);
  }
}

void LifetimesPropagationVisitor::PropagateBinAssign(
    const clang::Expr *lhs, const clang::Expr *rhs, const clang::Expr *expr,
    const clang::BinaryOperator *op) const {
  debugLifetimes("PropagateBinAssign");
  // TODO delete this
  assert(rhs != nullptr && "RHS cannot be nullptr");
  if (expr == nullptr) return;

  const clang::VarDecl *lhs_decl;
  if (clang::isa<clang::MemberExpr>(expr)) {
    auto &points_to = PointsTo.GetExprDecls(expr);
    // TODO delete this
    assert(points_to.size() == 1 && "Handle multiple points to in MemberExpr");
    lhs_decl = *points_to.begin();
  } else if (const auto *lhs_decl_ref_expr =
                 dyn_cast<clang::DeclRefExpr>(expr)) {
    if (!(lhs_decl = dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl()))) {
      return;
    }
  } else {
    return;
  }
  // TODO maybe don't use types
  clang::QualType base_type = lhs->getType().getCanonicalType();
  TransferRHS(lhs_decl, rhs, base_type, base_type, op, PointsTo, State);
}

std::optional<std::string> LifetimesPropagationVisitor::VisitArraySubscriptExpr(
    const clang::ArraySubscriptExpr *expr) {
  if (debugEnabled) debugLifetimes("[VisitArraySubscriptExpr]");
  Visit(const_cast<clang::Expr *>(expr->getIdx()));
  const auto *base = expr->getBase()->IgnoreParens();
  Visit(const_cast<clang::Expr *>(base));
  PointsTo.InsertExprPointsTo(expr, base);
  PointsTo.InsertExprDecl(expr, base);
  // debugLifetimes("Before insert call expr info (ArraySubscriptExpr)");
  PointsTo.InsertCallExprInfo(expr, base);
  // debugLifetimes("After insert call expr info (ArraySubscriptExpr)");

  clang::QualType found_type = PointsTo.GetExprType(base);
  if (!found_type.isNull()) {
    PointsTo.InsertExprType(expr, found_type);
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();
  const auto &rhs = op->getRHS()->IgnoreParens();

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes. We
  // don't want to change points-to sets in those cases.
  if (!lhs->getType()->isPointerType() && !lhs->getType()->isReferenceType()) {
    Visit(lhs);
    Visit(rhs);
    return std::nullopt;
  }

  Visit(lhs);
  PointsTo.InsertExprPointsTo(op, lhs);
  PointsTo.InsertExprDecl(op, lhs);
  // TODO check if this is needed
  // debugLifetimes("Before insert call expr info (BinAssign lhs)");
  PointsTo.InsertCallExprInfo(op, lhs);
  // debugLifetimes("After insert call expr info (BinAssign lhs)");

  Visit(rhs);
  PointsTo.InsertExprPointsTo(op, rhs);
  // debugLifetimes("Before insert call expr info (BinAssign rhs)");
  PointsTo.InsertCallExprInfo(op, rhs);
  // debugLifetimes("After insert call expr info (BinAssign rhs)");

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
    // debugInfo(direct_callee->getNameAsString());
    PointsTo.InsertExprPointsTo(call, nullptr);

    clang::QualType func_type = direct_callee->getReturnType().IgnoreParens();
    auto &all_func_info = Analyzer->GetFunctionInfo();

    auto it = all_func_info.find(direct_callee);
    if (it == all_func_info.end()) {
      Analyzer->GetLifetimes(direct_callee);
    }

    auto &func_info = all_func_info[direct_callee];

    // * process args
    // debugLight("Processing dead args...");
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
      const auto *param = direct_callee->getParamDecl(param_idx);
      clang::QualType param_type = param->getType()->getPointeeType();
      
      while (--num_indirections > 0) {
        if (param_type.isConstQualified()) {
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
      // debugWarn("Return type is not pointer type");
      for (unsigned int arg_idx = 0; arg_idx < direct_callee->getNumParams();
           arg_idx++) {
        const Expr *arg = call->getArg(arg_idx)->IgnoreParens();
        clang::QualType arg_type = arg->getType();
        if (!arg_type->isPointerType() && !arg_type->isReferenceType()) {
          continue;
        }
        clang::QualType found_type = PointsTo.GetExprType(arg);
        if (!found_type.isNull()) {
          PointsTo.InsertExprType(arg, found_type);
        }
      }
      return std::nullopt;
    }

    ObjectLifetimes return_ol = func_info.GetReturnLifetime();
    // debugInfo("Inserting call into CallExprInfo");
    auto &call_info = PointsTo.GetCallExprInfo(call);
    // debugLifetimes("Has call_info?", PointsTo.HasCallExprInfo(call));
    unsigned int func_num_indirections =
        Lifetime::GetNumIndirections(func_type);
    while (func_num_indirections > 0) {
      // debugLifetimes("Func num indirections", func_num_indirections);
      Lifetime &return_lifetime = return_ol.GetLifetime(func_type);
      assert(!return_lifetime.IsNull());
      // debugLifetimes("Func lifetime", return_lifetime.DebugString());

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
            // TODO == or contains?
            if (param_lifetime == return_lifetime) {
              // debugLifetimes("Param lifetime", param_lifetime.DebugString());
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
    case clang::CK_LValueToRValue: {
      // TODO
      debugLight("> Case LValueToRValue");
      // if (cast->getType()->isPointerType()) {
      // Converting from a glvalue to a prvalue means that we need to
      // perform a dereferencing operation because the objects associated
      // with glvalues and prvalues have different meanings:
      // - A glvalue is associated with the object identified by the
      // glvalue.
      // - A prvalue is only associated with an object if the prvalue is of
      //   pointer type; the object it is associated with is the object the
      //   pointer points to.
      // See also documentation for PointsToMap.

      // ObjectSet points_to = PointsTo.GetPointerPointsToSet(
      //     PointsTo.GetExprObjectSet(cast->getSubExpr()));
      // PointsTo.SetExprObjectSet(cast, points_to);
      // }

      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));
        if (!cast->getType()->isPointerType() &&
            !cast->getType()->isReferenceType()) {
          continue;
        }
        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          PointsTo.InsertExprPointsTo(cast, child_expr);
          PointsTo.InsertExprDecl(cast, child_expr);
          // debugLifetimes("Before insert call expr info (DeclRef)");
          PointsTo.InsertCallExprInfo(cast, child_expr);
          // debugLifetimes("After insert call expr info (DeclRef)");
          clang::QualType found_type = PointsTo.GetExprType(child_expr);
          if (!found_type.isNull()) {
            PointsTo.InsertExprType(cast, found_type);
          }
        }
      }
      break;
    }
    case clang::CK_NullToPointer: {
      // TODO
      debugLight("> Case NullToPointer");

      // PointsTo.SetExprObjectSet(cast, {});
      // break;
    }
    // These casts are just no-ops from a Object point of view.
    case clang::CK_FunctionToPointerDecay:
    case clang::CK_BuiltinFnToFnPtr:
    case clang::CK_UserDefinedConversion:
      // Note on CK_UserDefinedConversion: The actual conversion happens in a
      // CXXMemberCallExpr that is a subexpression of this CastExpr. The
      // CK_UserDefinedConversion is just used to mark the fact that this is a
      // user-defined conversion; it's therefore a no-op for our purposes.
      {
        // TODO
        debugLight("> Case FunctionToPointerDecay");

        // clang::QualType type = cast->getType().getCanonicalType();
        // if (type->isPointerType() || cast->isGLValue()) {
        //   PointsTo.SetExprObjectSet(
        //       cast, PointsTo.GetExprObjectSet(cast->getSubExpr()));
        // }
        break;
      }
    case clang::CK_DerivedToBase:
    case clang::CK_UncheckedDerivedToBase:
    case clang::CK_BaseToDerived:
    case clang::CK_Dynamic: {
      // TODO
      debugLight("> Case SubExpressions");

      // These need to be mapped to what the subexpr points to.
      // (Simple cases just work okay with this; may need to be revisited when
      // we add more inheritance support.)

      // ObjectSet points_to =
      // PointsTo.GetExprObjectSet(cast->getSubExpr());
      // PointsTo.SetExprObjectSet(cast, points_to);
      break;
    }
    case clang::CK_BitCast:
    case clang::CK_LValueBitCast:
    case clang::CK_IntegralCast:
    case clang::CK_ArrayToPointerDecay:
    case clang::CK_IntegralToPointer:
    case clang::CK_NoOp:
    case clang::CK_ToVoid:
    case clang::CK_IntegralToFloating: {
      debugLight("> Case Reinterpret cast");

      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));
        if (!cast->getType()->isPointerType() &&
            !cast->getType()->isReferenceType()) {
          continue;
        }
        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          PointsTo.InsertExprPointsTo(cast, child_expr);
          PointsTo.InsertExprDecl(cast, child_expr);
          // debugLifetimes("Before insert call expr info (CastExpr)");
          PointsTo.InsertCallExprInfo(cast, child_expr);
          // debugLifetimes("After insert call expr info (CastExpr)");

          clang::QualType found_type = PointsTo.GetExprType(child_expr);
          if (!found_type.isNull()) {
            PointsTo.InsertExprType(cast, found_type);
          } else {
            PointsTo.InsertExprType(cast, child_expr->getType());
          }
        }
      }

      break;
    }
    default: {
      debugLight("> Case Unknown cast");
      // cast->dump();
      if (cast->isGLValue() ||
          cast->getType().getCanonicalType()->isPointerType()) {
        llvm::errs() << "Unknown cast type:\n";
        // cast->dump();
        // No-noop casts of pointer types are not handled yet.
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
  const auto *false_expr = op->getFalseExpr();
  Visit(const_cast<clang::Expr *>(true_expr));
  Visit(const_cast<clang::Expr *>(false_expr));
  PointsTo.InsertExprPointsTo(op, true_expr);
  PointsTo.InsertExprPointsTo(op, false_expr);

  // debugLifetimes("Size of points to of op",
  //                PointsTo.GetExprPointsTo(op).size());

  // debugLifetimes("Before insert call expr info (ConditionalOperator true)");
  PointsTo.InsertCallExprInfo(op, true_expr);
  // debugLifetimes("After insert call expr info (ConditionalOperator true)");
  // debugLifetimes("Before insert call expr info (ConditionalOperator false)");
  PointsTo.InsertCallExprInfo(op, false_expr);
  // debugLifetimes("After insert call expr info (ConditionalOperator false)");
  PointsTo.InsertExprDecl(op, true_expr);
  PointsTo.InsertExprDecl(op, false_expr);
  // debugLifetimes("Size of expr decls of op",
  // PointsTo.GetExprDecls(op).size()); debugLifetimes("Size of expr decls of
  // true", PointsTo.GetExprDecls(true_expr).size()); debugLifetimes("Size of
  // expr decls of false", PointsTo.GetExprDecls(false_expr).size());

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  if (debugEnabled) debugLifetimes("[VisitDeclRefExpr]");

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

      if (!lhs_type->isPointerType() && !lhs_type->isReferenceType()) {
        continue;
      }

      ObjectLifetimes objectLifetimes =
          State.GetVarDeclLifetime(var_decl, State.GetLifetimeFactory());
      State.CreateVariable(var_decl, objectLifetimes);

      if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
        const clang::Expr *init = var_decl->getInit()->IgnoreParens();
        Visit(const_cast<clang::Expr *>(init));
        TransferRHS(var_decl, init, lhs_type, lhs_type, decl_stmt, PointsTo,
                    State);
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
      PointsTo.InsertExprPointsTo(expr, child_expr);
      PointsTo.InsertExprDecl(expr, child_expr);
      // debugLifetimes("Before insert call expr info (Expr)");
      PointsTo.InsertCallExprInfo(expr, child_expr);
      // debugLifetimes("After insert call expr info (Expr)");
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitMemberExpr(
    const clang::MemberExpr *member_expr) {
  if (debugEnabled) debugLifetimes("[VisitMemberExpr]");

  const clang::Expr *base = member_expr->getBase();
  Visit(const_cast<clang::Expr *>(base));
  // debugLifetimes("After visit");
  // TODO check if this works from struct->struct->field
  // TODO
  auto &points_to = PointsTo.GetExprDecls(base);
  if (points_to.size() > 1) {
    assert(false && "More than one decl in MemberExpr");
  }

  // TODO change this
  // debugLifetimes("Member - has call info?", PointsTo.HasCallExprInfo(base));
  // debugLifetimes("Before insert call expr info (MemberExpr)");
  bool has_func_call = PointsTo.InsertCallExprInfo(member_expr, base);
  // debugLifetimes("After insert call expr info (MemberExpr)");
  // PointsTo.InsertCallExprInfo(member_expr, base);

  // TODO uncomment assert
  assert(points_to.size() == 1 || has_func_call);

  if (points_to.size() < 1) return std::nullopt;
  PointsTo.InsertExprDecl(member_expr, base);

  // TODO delete this
  PointsTo.InsertExprType(member_expr, base->getType());
  // debugLifetimes("Size of points to [member expr]",
  //                PointsTo.GetExprDecls(member_expr).size());
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

  if (clang::isa<clang::CastExpr>(return_value)) {
    for (const auto &child : return_value->children()) {
      if (child == nullptr) continue;
      if (const auto *sub_cast_expr = clang::dyn_cast<clang::CastExpr>(child)) {
        PointsTo.InsertExprType(return_stmt->getRetValue(),
                                sub_cast_expr->getType());
      }
    }
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
      PointsTo.InsertExprPointsTo(op, child_expr);
      PointsTo.InsertExprDecl(op, child_expr);
      // debugLifetimes("Before insert call expr info (UnaryAddrOf)");
      PointsTo.InsertCallExprInfo(op, child_expr);
      // debugLifetimes("After insert call expr info (UnaryAddrOf)");
      if (clang::isa<clang::ArraySubscriptExpr>(child_expr)) {
        return std::nullopt;
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
      PointsTo.InsertExprPointsTo(op, child_expr);
      PointsTo.InsertExprDecl(op, child_expr);
      // debugLifetimes("Before insert call expr info (UnaryOperator)");
      PointsTo.InsertCallExprInfo(op, child_expr);
      // debugLifetimes("After insert call expr info (UnaryOperator)");
    }
  }
  return std::nullopt;
}

}  // namespace clang
