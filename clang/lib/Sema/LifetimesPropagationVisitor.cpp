#include "clang/Sema/LifetimesPropagationVisitor.h"

namespace clang {

void TransferRHS(const clang::NamedDecl *lhs, const clang::Expr *rhs,
                 PointsToMap &PointsTo, LifetimeAnnotationsAnalysis &state) {
  // debugLifetimes("\t[TransferRHS]");
  const auto &points_to = PointsTo.GetExprPointsTo(rhs);
  for (const auto &expr : points_to) {
    if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
      const auto *rhs_ref_decl = clang::dyn_cast<clang::DeclRefExpr>(expr);
      state.CreateDependency(lhs, rhs_ref_decl);
    }
  }
}

Lifetime GetVarDeclLifetime(const clang::VarDecl *var_decl,
                            FunctionLifetimeFactory &lifetime_factory) {
  clang::QualType type = var_decl->getType();
  clang::TypeLoc type_loc;
  if (var_decl->getTypeSourceInfo()) {
    type_loc = var_decl->getTypeSourceInfo()->getTypeLoc();
  }
  Lifetime lifetime;
  if (llvm::Error err = lifetime_factory.CreateVarLifetimes(type, type_loc)
                            .moveInto(lifetime)) {
    // TODO error
    return Lifetime();
    // return std::move(err);
  }
  return lifetime;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitBinaryOperator(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinaryOperator]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS();

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes. We
  // don't want to change points-to sets in those cases.
  // TODO need to check for references?
  if (!lhs->getType()->isPointerType()) {
    debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  Visit(lhs);
  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);
  PointsTo.InsertExprLifetimes(op, lhs);

  const auto &rhs = op->getRHS();
  Visit(rhs);

  const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
  PointsTo.InsertExprLifetimes(op, rhs);

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs);

  if (lhs_decl_ref_expr &&
      State.IsLifetimeNotset(lhs_decl_ref_expr->getDecl())) {
    TransferRHS(lhs_decl_ref_expr->getDecl(), rhs, PointsTo, State);
  } else {
    // TODO
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
  debugLifetimes("[VisitCallExpr]");
  // No need to check the arguments lifetimes because there is always a min lifetime between any set of lifetimes

  const clang::FunctionDecl *direct_callee = call->getDirectCallee();
  if (direct_callee) {
    clang::QualType func_type = direct_callee->getReturnType();
    // ignore if return type does not have a Lifetime
    // TODO references also included?
    if (!func_type->isPointerType()) {
      debugWarn("Return type is not pointer type");
      return std::nullopt;
    }
    
    auto it = FuncInfo.find(direct_callee);
    if (it == FuncInfo.end()) {
      // TODO error
      debugWarn("Did not find function in FuncInfo");
      return std::nullopt;
    }

    const auto &func_info = FuncInfo[direct_callee];
    const Lifetime &return_lifetime = func_info.GetReturnLifetime();

    unsigned int i = -1;
    while (++i < func_info.GetNumParams()) {
      const clang::ParmVarDecl *param = func_info.GetParam(i);
      const auto &param_lifetime = func_info.GetParamLifetime(param);
      if (param_lifetime.has_value() &&
          param_lifetime.value() == return_lifetime) {
        const Expr *arg = call->getArg(i);
        Visit(const_cast<clang::Expr *>(arg));
        PointsTo.InsertExprLifetimes(call, arg);
      }
    }
  } else {
    debugWarn("No direct callee");
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitCastExpr(
    const clang::CastExpr *cast) {
  debugLifetimes("[VisitCastExpr]");
  switch (cast->getCastKind()) {
    case clang::CK_LValueToRValue: {
      // TODO
      debugLight("> Case LValueToRValue");
      if (cast->getType()->isPointerType()) {
        // Converting from a glvalue to a prvalue means that we need to perform
        // a dereferencing operation because the objects associated with
        // glvalues and prvalues have different meanings:
        // - A glvalue is associated with the object identified by the glvalue.
        // - A prvalue is only associated with an object if the prvalue is of
        //   pointer type; the object it is associated with is the object the
        //   pointer points to.
        // See also documentation for PointsToMap.

        // ObjectSet points_to = PointsTo.GetPointerPointsToSet(
        //     PointsTo.GetExprObjectSet(cast->getSubExpr()));
        // PointsTo.SetExprObjectSet(cast, points_to);
      }

      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));

        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          PointsTo.InsertExprLifetimes(cast, child_expr);
        }
      }
      break;
    }
    case clang::CK_NullToPointer: {
      // TODO
      debugLight("> Case NullToPointer");

      // PointsTo.SetExprObjectSet(cast, {});
      break;
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
      debugLight("> Case No-ops");

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
    case clang::CK_IntegralToPointer: {
      // We don't support analyzing functions that perform a reinterpret_cast.

      // TODO
      debugLight("> Case Reinterpret cast");

      // diag_reporter_(
      //     Func->getBeginLoc(),
      //     "cannot infer lifetimes because function uses a type-unsafe cast",
      //     clang::DiagnosticIDs::Warning);
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

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  debugLifetimes("[VisitDeclRefExpr]");

  auto *decl = decl_ref->getDecl();
  if (!clang::isa<clang::VarDecl>(decl) &&
      !clang::isa<clang::FunctionDecl>(decl)) {
    return std::nullopt;
  }

  assert(decl_ref->isGLValue() || decl_ref->getType()->isBuiltinType());

  // clang::QualType type = decl->getType().getCanonicalType();

  // TODO don't insert if it's not either reference or pointer type

  PointsTo.InsertExprLifetimes(decl_ref, nullptr);

  // TODO
  // if (type->isReferenceType()) {
  //   PointsTo.SetExprObjectSet(
  //       decl_ref, PointsTo.GetPointerPointsToSet(object));
  // } else {
  //   PointsTo.SetExprObjectSet(decl_ref, {object});
  // }

  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      if (!var_decl->getType()->isPointerType()) {
        // TODO what happens when it's a reference? `isPointerType()` also
        // catches this?
        debugWarn("Var decl is not pointer type");
        // return std::nullopt;
        continue;
      }

      Lifetime lifetime = GetVarDeclLifetime(var_decl, Factory);
      State.CreateVariable(var_decl, lifetime);

      // Don't need to record initializers because initialization has already
      // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
      // VisitCallExpr().
      if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
        const clang::Expr *init = var_decl->getInit();
        Visit(const_cast<clang::Expr *>(init));
        if (State.IsLifetimeNotset(var_decl))
          TransferRHS(var_decl, init, PointsTo, State);
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
