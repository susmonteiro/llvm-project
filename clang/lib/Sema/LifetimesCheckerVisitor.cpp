#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Sema/Sema.h"

namespace clang {

std::optional<std::string> LifetimesCheckerVisitor::VisitBinAssign(
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
  const auto &lhs_points_to = points_to_map.GetExprPoints(lhs);
  points_to_map.InsertExprLifetimes(op, lhs);

  const auto &rhs = op->getRHS();
  Visit(rhs);

  const auto &rhs_points_to = points_to_map.GetExprPoints(rhs);
  points_to_map.InsertExprLifetimes(op, rhs);

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs);
  if (lhs_decl_ref_expr) {
    // Check that lhs_lifetime >= rhs_lifetime
    Lifetime &lhs_lifetime = state_.GetLifetime(lhs_decl_ref_expr->getDecl());
    
    for (const auto &expr : rhs_points_to) {
    if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
      const auto *rhs_var = clang::dyn_cast<clang::DeclRefExpr>(expr);
      clang::QualType rhs_var_type = rhs_var->getType();
      if (!rhs_var_type->isPointerType() && !rhs_var_type->isReferenceType()) {
        continue;
      }

      // there can only be one pointer/reference variable
      const auto *lhs_decl = lhs_decl_ref_expr->getDecl();
      const auto *rhs_decl = rhs_var->getDecl();
      Lifetime &rhs_lifetime = state_.GetLifetime(rhs_decl);
      if (lhs_lifetime < rhs_lifetime) { 
        if (rhs_lifetime.IsNotSet()) {
          for (char l : rhs_lifetime.GetShortestLifetimes()) {
            if (l == lhs_lifetime.GetId()) continue;
            S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
              << lhs_lifetime.GetLifetimeName()
              << rhs_lifetime.GetLifetimeName(l)
              << op->getSourceRange();
          }
          // TODO implement the notes in this case
        } else if (lhs_lifetime.IsNotSet()) {
          for (char l : lhs_lifetime.GetShortestLifetimes()) {
            if (l == rhs_lifetime.GetId()) continue;
            S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
              << rhs_lifetime.GetLifetimeName()
              << lhs_lifetime.GetLifetimeName(l)
              << op->getSourceRange();
          }
          // TODO implement the notes in this case
        } else {
          S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
              << lhs_lifetime.GetLifetimeName()
              << rhs_lifetime.GetLifetimeName()
              << op->getSourceRange();
          S.Diag(lhs_decl->getLocation(), diag::note_lifetime_declared_here)
              << lhs_lifetime.GetLifetimeName() << lhs_decl->getSourceRange();
          S.Diag(rhs_decl->getLocation(), diag::note_lifetime_declared_here)
              << rhs_lifetime.GetLifetimeName() << rhs_decl->getSourceRange();
        }
      }
      break;
    }
  }

  }

  // if (lhs_decl_ref_expr &&
  //     state_.IsLifetimeNotset(lhs_decl_ref_expr->getDecl())) {
  //   TransferRHS(lhs_decl_ref_expr->getDecl(), rhs, points_to_map, state_);
  // } else {
  //   // TODO
  // }

  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitCastExpr(
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

        // ObjectSet points_to = points_to_map_.GetPointerPointsToSet(
        //     points_to_map_.GetExprObjectSet(cast->getSubExpr()));
        // points_to_map_.SetExprObjectSet(cast, points_to);
      }

      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));

        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          points_to_map.InsertExprLifetimes(cast, child_expr);
        }
      }
      break;
    }
    case clang::CK_NullToPointer: {
      // TODO
      debugLight("> Case NullToPointer");

      // points_to_map_.SetExprObjectSet(cast, {});
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
      //   points_to_map_.SetExprObjectSet(
      //       cast, points_to_map_.GetExprObjectSet(cast->getSubExpr()));
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
      // points_to_map_.GetExprObjectSet(cast->getSubExpr());
      // points_to_map_.SetExprObjectSet(cast, points_to);
      break;
    }
    case clang::CK_BitCast:
    case clang::CK_LValueBitCast:
    case clang::CK_IntegralToPointer: {
      // We don't support analyzing functions that perform a reinterpret_cast.

      // TODO
      debugLight("> Case Reinterpret cast");

      // diag_reporter_(
      //     func_->getBeginLoc(),
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

std::optional<std::string> LifetimesCheckerVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  debugLifetimes("[VisitDeclRefExpr]");

  auto *decl = decl_ref->getDecl();
  if (!clang::isa<clang::VarDecl>(decl) &&
      !clang::isa<clang::FunctionDecl>(decl)) {
    return std::nullopt;
  }

  assert(decl_ref->isGLValue() || decl_ref->getType()->isBuiltinType());

  // clang::QualType type = decl->getType().getCanonicalType();

  // if (type->isReferenceType()) {
  //   debugLifetimes("It's reference type!");

  // } else {
  //   debugLifetimes("It's not reference type");
  // }

  // TODO don't insert if it's not either reference or pointer type

  points_to_map.InsertExprLifetimes(decl_ref, nullptr);

  // TODO
  // if (type->isReferenceType()) {
  //   points_to_map_.SetExprObjectSet(
  //       decl_ref, points_to_map_.GetPointerPointsToSet(object));
  // } else {
  //   points_to_map_.SetExprObjectSet(decl_ref, {object});
  // }

  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  debugLifetimes("[VisitReturnStmt]");
  debugLifetimes("This is the return lifetime",
                 state_.GetReturnLifetime().DebugString());

  clang::QualType return_type = func_->getReturnType();

  // We only need to handle pointers and references.
  // For record types, initialization of the return value has already been
  // handled in VisitCXXConstructExpr() or VisitInitListExpr(), so nothing
  // to do here.
  if (!return_type->isPointerType() && !return_type->isReferenceType()) {
    return std::nullopt;
  }

  Lifetime &return_lifetime = state_.GetReturnLifetime();
  if (return_lifetime.IsNotSet()) {
    debugWarn("Return does not have a valid lifetime");
    // TODO error
  }

  Visit(const_cast<clang::Expr *>(return_stmt->getRetValue()));
  const auto &return_expr =
      points_to_map.GetExprPoints(return_stmt->getRetValue());

  for (const auto &expr : return_expr) {
    if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
      const auto *var = clang::dyn_cast<clang::DeclRefExpr>(expr);
      clang::QualType var_type = var->getType();
      if (!var_type->isPointerType() && !var_type->isReferenceType()) {
        continue;
      }

      // there can only be one pointer/reference variable
      const auto *var_decl = var->getDecl();
      Lifetime &var_lifetime = state_.GetLifetime(var_decl);
      if (return_lifetime < var_lifetime) { 
        if (var_lifetime.IsNotSet()) {
          for (char l : var_lifetime.GetShortestLifetimes()) {
            if (l == return_lifetime.GetId()) continue;
            S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
              << return_lifetime.GetLifetimeName()
              << var_lifetime.GetLifetimeName(l)
              << return_stmt->getSourceRange();
          }
          // TODO implement the notes in this case
        } else {
          S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
              << return_lifetime.GetLifetimeName()
              << var_lifetime.GetLifetimeName()
              << return_stmt->getSourceRange();
          S.Diag(var_decl->getLocation(), diag::note_lifetime_declared_here)
              << var_lifetime.GetLifetimeName() << var_decl->getSourceRange();
        }
      }
      break;
    }
  }

  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
