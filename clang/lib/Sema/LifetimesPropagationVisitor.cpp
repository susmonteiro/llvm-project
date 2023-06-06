#include "clang/Sema/LifetimesPropagationVisitor.h"

namespace clang {

void CreateDependency(const clang::Expr *expr, const clang::NamedDecl *lhs,
                      const clang::Stmt *loc,
                      LifetimeAnnotationsAnalysis &state) {
  if (clang::isa<clang::DeclRefExpr>(expr)) {
    const auto *rhs_ref_decl = clang::dyn_cast<clang::DeclRefExpr>(expr);
    state.CreateDependency(lhs, rhs_ref_decl, loc);
  } else if (clang::isa<clang::UnaryOperator>(expr)) {
    debugInfo("Create dependency between var_decl and unary_operator");
    const auto *rhs_unary_op = clang::dyn_cast<clang::UnaryOperator>(expr);
    state.CreateDependency(lhs, rhs_unary_op, loc);
  }
}

void TransferRHS(const clang::NamedDecl *lhs, const clang::Expr *rhs,
                 const clang::Stmt *loc, PointsToMap &PointsTo,
                 LifetimeAnnotationsAnalysis &state) {
  // debugLifetimes("\t[TransferRHS]");
  const auto &points_to = PointsTo.GetExprPointsTo(rhs);
  CreateDependency(rhs, lhs, loc, state);
  for (const auto &expr : points_to) {
    debugInfo("Found expr in points_to");
    expr->dump();
    if (expr == nullptr) continue;
    CreateDependency(expr, lhs, loc, state);
  }
}

Lifetime GetVarDeclLifetime(const clang::VarDecl *var_decl,
                            FunctionLifetimeFactory &lifetime_factory) {
  clang::QualType type = var_decl->getType().IgnoreParens();
  clang::TypeLoc type_loc;
  if (var_decl->getTypeSourceInfo()) {
    type_loc = var_decl->getTypeSourceInfo()->getTypeLoc();
  }
  ObjectLifetime objectLifetime;
  if (llvm::Error err = lifetime_factory.CreateVarLifetimes(type, type_loc)
                            .moveInto(objectLifetime)) {
    // TODO error
    return Lifetime();
    // return std::move(err);
  }
  debugLifetimes("Variable " + var_decl->getNameAsString() + ":\n", objectLifetime.DebugString());
  // TODO change this
  return objectLifetime.GetVarLifetime();
}

std::optional<std::string> LifetimesPropagationVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes. We
  // don't want to change points-to sets in those cases.
  if (!lhs->getType()->isPointerType() && !lhs->getType()->isReferenceType()) {
    debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  Visit(lhs);
  PointsTo.InsertExprLifetimes(op, lhs);

  const auto &rhs = op->getRHS()->IgnoreParens();
  Visit(rhs);
  PointsTo.InsertExprLifetimes(op, rhs);

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs);

  if (lhs_decl_ref_expr &&
      State.IsLifetimeNotset(lhs_decl_ref_expr->getDecl())) {
    TransferRHS(lhs_decl_ref_expr->getDecl(), rhs, op, PointsTo, State);
  } else {
    // TODO
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
    // ignore if return type does not have a Lifetime
    if (!func_type->isPointerType() && !func_type->isReferenceType()) {
      debugWarn("Return type is not pointer type");
      return std::nullopt;
    }

    auto it = FuncInfo.find(direct_callee);
    if (it == FuncInfo.end()) {
      // TODO error
      debugWarn("Did not find function in FuncInfo");
      return std::nullopt;
    }

    auto &func_info = FuncInfo[direct_callee];
    const Lifetime &return_lifetime = func_info.GetReturnLifetime();

    unsigned int i = -1;
    while (++i < func_info.GetNumParams()) {
      const clang::ParmVarDecl *param = func_info.GetParam(i);
      const auto &param_lifetime = func_info.GetParamLifetime(param);
      if (param_lifetime.has_value() &&
          param_lifetime.value() == return_lifetime) {
        const Expr *arg = call->getArg(i)->IgnoreParens();
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
  if (debugEnabled) debugLifetimes("[VisitCastExpr]");
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
  if (debugEnabled) debugLifetimes("[VisitDeclRefExpr]");

  auto *decl = decl_ref->getDecl();
  if (!clang::isa<clang::VarDecl>(decl) &&
      !clang::isa<clang::FunctionDecl>(decl)) {
    return std::nullopt;
  }

  assert(decl_ref->isGLValue() || decl_ref->getType()->isBuiltinType());

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
  if (debugEnabled) debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      if (!var_decl->getType()->isPointerType() &&
          !var_decl->getType()->isReferenceType()) {
        debugWarn("Var decl is not pointer type");
        continue;
      }

      Lifetime lifetime = GetVarDeclLifetime(var_decl, Factory);
      State.CreateVariable(var_decl, lifetime);

      // Don't need to record initializers because initialization has already
      // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
      // VisitCallExpr().
      if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
        const clang::Expr *init = var_decl->getInit()->IgnoreParens();
        Visit(const_cast<clang::Expr *>(init));
        debugInfo("The vardecl has init");
        if (State.IsLifetimeNotset(var_decl)) {
          debugInfo(
              "The lifetime of vardecl is not set, thus we call TransferRHS");
          TransferRHS(var_decl, init, decl_stmt, PointsTo, State);
        }
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitExpr(
    const clang::Expr *expr) {
  if (debugEnabled) debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  if (debugEnabled) debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesPropagationVisitor::VisitUnaryAddrOf(
    const clang::UnaryOperator *op) {
  // TODO implement
  if (debugEnabled) debugLifetimes("[VisitUnaryAddrOf]");

  if (!op->isGLValue() && !op->getType()->isPointerType() &&
      !op->getType()->isReferenceType() && !op->getType()->isArrayType()) {
    debugWarn("Skipped...");
    return std::nullopt;
  }

  // TODO needed?
  // for (const auto &child : op->children()) {
  //   Visit(const_cast<clang::Stmt *>(child));
  //   debugInfo("Visited addrof child");
  //   if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
  //     debugInfo("Insert addrof in PointsTo because it is expr");
  //     PointsTo.InsertExprLifetimes(op, child_expr);
  //   }
  // }

  PointsTo.InsertExprLifetimes(op, nullptr);
  State.CreateDeclRef(op, Lifetime(LOCAL));
  return std::nullopt;
}

// TODO sometimes this is not being visited
std::optional<std::string> LifetimesPropagationVisitor::VisitUnaryDeref(
    const clang::UnaryOperator *op) {
  // TODO implement
  if (debugEnabled) debugLifetimes("[VisitUnaryDeref]");

  if (!op->isGLValue() && !op->getType()->isPointerType() &&
      !op->getType()->isReferenceType() && !op->getType()->isArrayType()) {
    debugWarn("Skipped...");
    return std::nullopt;
  }

  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
    if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
      debugInfo("Insert deref in PointsTo because it is expr");
      PointsTo.InsertExprLifetimes(op, child_expr);
    }
  }
  return std::nullopt;
}

}  // namespace clang
