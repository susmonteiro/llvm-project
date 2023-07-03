#include "clang/Sema/LifetimesPropagationVisitor.h"

namespace clang {

void CreateDependency(const clang::Expr *expr, const clang::VarDecl *lhs,
                      clang::QualType lhs_type, clang::QualType rhs_type,
                      const clang::Stmt *loc,
                      LifetimeAnnotationsAnalysis &state) {
  if (const auto *rhs_ref_decl = clang::dyn_cast<clang::DeclRefExpr>(expr)) {
    if (const auto *rhs_var_decl = clang::dyn_cast<clang::VarDecl>(
            rhs_ref_decl->getDecl()->getCanonicalDecl())) {
      state.CreateDependency(lhs, lhs_type, rhs_var_decl, rhs_type, loc);
    }
  }
}

void TransferRHS(const clang::VarDecl *lhs, const clang::Expr *rhs,
                 clang::QualType lhs_type, clang::QualType base_type,
                 const clang::Stmt *loc, PointsToMap &PointsTo,
                 LifetimeAnnotationsAnalysis &state) {
  const auto &points_to = PointsTo.GetExprPointsTo(rhs);
  clang::QualType rhs_type = PointsTo.GetExprType(rhs);
  rhs_type = rhs_type.isNull() ? base_type : rhs_type;

  CreateDependency(rhs, lhs, lhs_type, rhs_type, loc, state);
  for (const auto &expr : points_to) {
    if (expr != nullptr) {
      CreateDependency(expr, lhs, lhs_type, rhs_type, loc, state);
    }
  }
}

ObjectLifetimes GetVarDeclLifetime(const clang::VarDecl *var_decl,
                                   FunctionLifetimeFactory &lifetime_factory) {
  clang::QualType type = var_decl->getType().IgnoreParens();
  clang::TypeLoc type_loc;
  if (var_decl->getTypeSourceInfo()) {
    type_loc = var_decl->getTypeSourceInfo()->getTypeLoc();
  }
  ObjectLifetimes objectsLifetimes;
  if (llvm::Error err = lifetime_factory.CreateVarLifetimes(type, type_loc)
                            .moveInto(objectsLifetimes)) {
    // TODO error
    return ObjectLifetimes();
    // return std::move(err);
  }
  return objectsLifetimes;
}

void LifetimesPropagationVisitor::PropagateBinAssign(
    const clang::Expr *lhs, const clang::Expr *rhs, const clang::Expr *expr,
    const clang::BinaryOperator *op) const {
  if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) return;
  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr);
  if (const auto *lhs_var_decl =
          dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
    clang::QualType found_type = PointsTo.GetExprType(lhs);
    clang::QualType base_type = lhs->getType().getCanonicalType();
    if (found_type.isNull()) {
      TransferRHS(lhs_var_decl, rhs, base_type, base_type, op, PointsTo, State);
    } else {
      TransferRHS(lhs_var_decl, rhs, found_type, base_type, op, PointsTo,
                  State);
    }
  }
}

std::optional<std::string> LifetimesPropagationVisitor::VisitArraySubscriptExpr(
    const clang::ArraySubscriptExpr *expr) {
  if (debugEnabled) debugLifetimes("[VisitArraySubscriptExpr]");
  Visit(const_cast<clang::Expr *>(expr->getIdx()));
  Visit(const_cast<clang::Expr *>(expr->getBase()));
  PointsTo.InsertExprLifetimes(expr, expr->getBase());
  clang::QualType found_type = PointsTo.GetExprType(expr->getBase());
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
  PointsTo.InsertExprLifetimes(op, lhs);

  Visit(rhs);
  PointsTo.InsertExprLifetimes(op, rhs);

  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);

  if (const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs)) {
    PropagateBinAssign(lhs, rhs, lhs_decl_ref_expr, op);
  } else {
    for (const auto &expr : lhs_points_to) {
      PropagateBinAssign(lhs, rhs, expr, op);
    }
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
    clang::QualType func_type = direct_callee->getReturnType().IgnoreParens();

    auto &all_func_info = Analyzer->GetFunctionInfo();

    auto it = all_func_info.find(direct_callee);
    if (it == all_func_info.end()) {
      Analyzer->GetLifetimes(direct_callee);
    }

    auto &func_info = all_func_info[direct_callee];

    // ignore if return type does not have a Lifetime
    if (!func_type->isPointerType() && !func_type->isReferenceType()) {
      // debugWarn("Return type is not pointer type");
      unsigned int i = -1;
      while (++i < func_info.GetNumParams()) {
        const clang::ParmVarDecl *param = func_info.GetParam(i);
        if (!param->getType()->isPointerType() &&
            !param->getType()->isReferenceType()) {
          // debugWarn("Param type is not pointer type");
          continue;
        }
        const Expr *arg = call->getArg(i)->IgnoreParens();
        Visit(const_cast<clang::Expr *>(arg));
        clang::QualType found_type = PointsTo.GetExprType(arg);
        if (!found_type.isNull()) {
          PointsTo.InsertExprType(arg, found_type);
        }
      }
      return std::nullopt;
    }

    const Lifetime &return_lifetime = func_info.GetReturnLifetime(func_type);

    unsigned int i = -1;
    while (++i < func_info.GetNumParams()) {
      const clang::ParmVarDecl *param = func_info.GetParam(i);
      if (!param->getType()->isPointerType() &&
          !param->getType()->isReferenceType()) {
        // debugWarn("Param type is not pointer type");
        continue;
      }
      const Expr *arg = call->getArg(i)->IgnoreParens();
      Visit(const_cast<clang::Expr *>(arg));
      clang::QualType param_type = PointsTo.GetExprType(arg);
      if (!param_type.isNull()) {
        PointsTo.InsertExprType(arg, param_type);
      } else {
        param_type = param->getType().getCanonicalType();
      }

      const auto &param_lifetime =
          func_info.GetParamLifetime(param, param_type);

      if (param_lifetime == return_lifetime) {
        PointsTo.InsertExprLifetimes(call, arg);
      }
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
      // debugLight("> Case LValueToRValue");
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
          PointsTo.InsertExprLifetimes(cast, child_expr);
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
      // debugLight("> Case NullToPointer");

      // PointsTo.SetExprObjectSet(cast, {});
      // break;
    }
    // These casts are just no-ops from a Object point of view.
    case clang::CK_FunctionToPointerDecay:
    case clang::CK_BuiltinFnToFnPtr:
    case clang::CK_ArrayToPointerDecay:
    case clang::CK_UserDefinedConversion:
      // Note on CK_UserDefinedConversion: The actual conversion happens in a
      // CXXMemberCallExpr that is a subexpression of this CastExpr. The
      // CK_UserDefinedConversion is just used to mark the fact that this is a
      // user-defined conversion; it's therefore a no-op for our purposes.
    case clang::CK_NoOp: {
      // TODO
      // debugLight("> Case No-ops");

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
      // debugLight("> Case SubExpressions");

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
    case clang::CK_IntegralToPointer: {
      // We don't support analyzing functions that perform a reinterpret_cast.

      // TODO
      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));
        if (!cast->getType()->isPointerType() &&
            !cast->getType()->isReferenceType()) {
          continue;
        }
        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          PointsTo.InsertExprLifetimes(cast, child_expr);
          clang::QualType found_type = PointsTo.GetExprType(child_expr);
          if (!found_type.isNull()) {
            PointsTo.InsertExprType(cast, found_type);
          }
        }
      }
      // debugLight("> Case Reinterpret cast");

      // diag_reporter_(
      //     Func->getBeginLoc(),
      //     "cannot infer lifetimes because function uses a type-unsafe
      //     cast", clang::DiagnosticIDs::Warning);
      // diag_reporter_(cast->getBeginLoc(), "type-unsafe cast occurs here",
      //                clang::DiagnosticIDs::Note);
      // return "type-unsafe cast prevents analysis";
      break;
    }
    default: {
      if (cast->isGLValue() ||
          cast->getType().getCanonicalType()->isPointerType()) {
        llvm::errs() << "Unknown cast type:\n";
        cast->dump();
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
  PointsTo.InsertExprLifetimes(op, true_expr);
  PointsTo.InsertExprLifetimes(op, false_expr);
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

  if (decl_ref->isGLValue() || decl_ref->getType()->isBuiltinType())
    return std::nullopt;
  PointsTo.InsertExprLifetimes(decl_ref, nullptr);

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  if (debugEnabled) debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      clang::QualType type = var_decl->getType().getCanonicalType();
      if (!type->isPointerType() && !type->isReferenceType()) {
        continue;
      }

      ObjectLifetimes objectsLifetimes = GetVarDeclLifetime(var_decl, Factory);
      State.CreateVariable(var_decl, objectsLifetimes);

      // Don't need to record initializers because initialization has already
      // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
      // VisitCallExpr().
      if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
        const clang::Expr *init = var_decl->getInit()->IgnoreParens();
        Visit(const_cast<clang::Expr *>(init));
        TransferRHS(var_decl, init, type, type, decl_stmt, PointsTo, State);
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
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitMemberExpr(
    const clang::MemberExpr *member_expr) {
  if (debugEnabled) debugLifetimes("[VisitMemberExpr]");

  const clang::Expr *base = member_expr->getBase();
  Visit(const_cast<clang::Expr *>(base));
  PointsTo.InsertExprLifetimes(member_expr, base);
  PointsTo.InsertExprType(member_expr, base->getType());
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  if (debugEnabled) debugLifetimes("[VisitReturnStmt]");

  const auto *return_value = return_stmt->getRetValue();
  clang::QualType return_type = Func->getReturnType().IgnoreParens();
  if (return_value == nullptr || !return_type->isPointerType() ||
      return_type->isReferenceType()) {
    return std::nullopt;
  }

  Visit(const_cast<clang::Expr *>(return_value));

  for (const auto &child : return_value->children()) {
    if (child == nullptr) continue;
    if (const auto *cast_expr = clang::dyn_cast<clang::CastExpr>(child)) {
      PointsTo.InsertExprType(return_stmt->getRetValue(), cast_expr->getType());
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
      PointsTo.InsertExprLifetimes(op, child_expr);
    }
  }

  for (const auto &child : PointsTo.GetExprPointsTo(op)) {
    if (child == nullptr) continue;
    if (const auto *decl_ref_expr = clang::dyn_cast<DeclRefExpr>(child)) {
      if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl_ref_expr->getDecl()->getCanonicalDecl())) {
        State.GetObjectLifetimes(var_decl).InsertPointeeObject(Lifetime(LOCAL, op->getType().getCanonicalType()));
      }
    }
  }

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
      PointsTo.InsertExprLifetimes(op, child_expr);
    }
  }
  return std::nullopt;
}

}  // namespace clang
