#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"

namespace clang {

std::optional<std::string> LifetimesCheckerVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS();

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes. We
  // don't want to change points-to sets in those cases.
  // TODO need to check for references?
  if (!lhs->getType()->isPointerType() && !lhs->getType()->isReferenceType()) {
    debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);
  // TODO remove this
  if (lhs_points_to.empty()) {
    debugWarn("LHS is not in PointsToMap");
    Visit(lhs);
    PointsTo.InsertExprLifetimes(op, lhs);
    const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);
  }

  const auto &rhs = op->getRHS();
  const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
  // TODO remove this
  if (rhs_points_to.empty()) {
    debugWarn("RHS is not in PointsToMap");
    Visit(rhs);
    PointsTo.InsertExprLifetimes(op, rhs);
    const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
  }

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs);
  if (lhs_decl_ref_expr) {
    // Check that lhs_lifetime >= rhs_lifetime
    Lifetime &lhs_lifetime = State.GetLifetime(lhs_decl_ref_expr->getDecl());

    for (const auto &expr : rhs_points_to) {
      if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
        const auto *rhs_var = clang::dyn_cast<clang::DeclRefExpr>(expr);
        clang::QualType rhs_var_type = rhs_var->getType();
        if (!rhs_var_type->isPointerType() &&
            !rhs_var_type->isReferenceType()) {
          continue;
        }

        // there can only be one pointer/reference variable
        const auto *lhs_decl = lhs_decl_ref_expr->getDecl();
        const auto *rhs_decl = rhs_var->getDecl();
        Lifetime &rhs_lifetime = State.GetLifetime(rhs_decl);
        if (rhs_lifetime < lhs_lifetime) {
          if (rhs_lifetime.IsNotSet()) {
            for (const auto &pair : rhs_lifetime.GetShortestLifetimes()) {
              if (pair.first == lhs_lifetime.GetId()) continue;
              S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                  << lhs_lifetime.GetLifetimeName()
                  << rhs_lifetime.GetLifetimeName(pair.first) << op->getSourceRange();
            }
            // TODO implement the notes in this case
          } else if (lhs_lifetime.IsNotSet()) {
            // TODO should this case exist?
            //   for (char l : lhs_lifetime.GetShortestLifetimes()) {
            //     if (l == rhs_lifetime.GetId()) continue;
            //     S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
            //         << rhs_lifetime.GetLifetimeName()
            //         << lhs_lifetime.GetLifetimeName(l) <<
            //         op->getSourceRange();
            //   }
            //   // TODO implement the notes in this case
          } else {
            S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                << lhs_lifetime.GetLifetimeName()
                << rhs_lifetime.GetLifetimeName() << op->getSourceRange();
            S.Diag(lhs_decl->getLocation(), diag::note_lifetime_declared_here)
                << lhs_lifetime.GetLifetimeName() << lhs_decl->getSourceRange();
            S.Diag(rhs_decl->getLocation(), diag::note_lifetime_declared_here)
                << rhs_lifetime.GetLifetimeName() << rhs_decl->getSourceRange();
          }
        }
      }
    }
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  if (debugEnabled) debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      if (!var_decl->getType()->isPointerType() && !var_decl->getType()->isReferenceType()) {
        debugWarn("Var decl is not pointer type");
        continue;
      }
      Lifetime &var_decl_lifetime = State.GetLifetime(var_decl);
      // no initializer, nothing to check
      if (!var_decl->hasInit() || var_decl_lifetime.IsNotSet())
        return std::nullopt;

      const auto &init_points_to =
          PointsTo.GetExprPointsTo(var_decl->getInit());
      // TODO remove this
      if (init_points_to.empty()) {
        debugWarn("Initializer is not in PointsToMap");
      }

      for (const auto &expr : init_points_to) {
        if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
          const auto &init_var = clang::dyn_cast<clang::DeclRefExpr>(expr);
          clang::QualType init_var_type = init_var->getType();
          if (!init_var_type->isPointerType() &&
              !init_var_type->isReferenceType()) {
            continue;
          }

          Lifetime &init_lifetime = State.GetLifetime(init_var->getDecl());
          // TODO check this
          // TODO maybe it's more correct to write ! >= 
          if (init_lifetime < var_decl_lifetime) {
            if (init_lifetime.IsNotSet()) {
              for (const auto &pair : init_lifetime.GetShortestLifetimes()) {
                if (pair.first == var_decl_lifetime.GetId()) continue;
                // TODO getLocation? -> also change for the "else" branch
                S.Diag(var_decl->getInit()->getExprLoc(),
                       diag::warn_assign_lifetimes_differ)
                    << var_decl_lifetime.GetLifetimeName()
                    << init_lifetime.GetLifetimeName(pair.first)
                    << var_decl->getInit()->getSourceRange();
              }
              // TODO implement the notes in this case
            } else {
              // TODO put loc in the '=' sign (also for above)
              // TODO maybe change warning to "declaration" instead of "assign"
              // (also above)
              S.Diag(var_decl->getInitializingDeclaration()->getLocation(),
                     diag::warn_assign_lifetimes_differ)
                  << var_decl_lifetime.GetLifetimeName()
                  << init_lifetime.GetLifetimeName()
                  << var_decl->getInitializingDeclaration()->getSourceRange();
              S.Diag(init_var->getDecl()->getLocation(),
                     diag::note_lifetime_declared_here)
                  << init_lifetime.GetLifetimeName()
                  << init_var->getDecl()->getSourceRange();
            }
          }
        }
      }
    }
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  if (debugEnabled) debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  if (debugEnabled) debugLifetimes("[VisitReturnStmt]");

  clang::QualType return_type = Func->getReturnType();

  // We only need to handle pointers and references.
  // For record types, initialization of the return value has already been
  // handled in VisitCXXConstructExpr() or VisitInitListExpr(), so nothing
  // to do here.
  if (!return_type->isPointerType() && !return_type->isReferenceType()) {
    return std::nullopt;
  }

  Lifetime &return_lifetime = State.GetReturnLifetime();
  if (return_lifetime.IsNotSet()) {
    debugWarn("Return does not have a valid lifetime");
    // TODO error
  }

  const auto &return_expr =
      PointsTo.GetExprPointsTo(return_stmt->getRetValue());
  // TODO remove this
  if (return_expr.empty()) {
    debugWarn("Return expr is not in PointsToMap");
    Visit(const_cast<clang::Expr *>(return_stmt->getRetValue()));
    const auto &lhs_points_to =
        PointsTo.GetExprPointsTo(return_stmt->getRetValue());
  }

  for (const auto &expr : return_expr) {
    if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
      const auto *var = clang::dyn_cast<clang::DeclRefExpr>(expr);
      clang::QualType var_type = var->getType();
      if (!var_type->isPointerType() && !var_type->isReferenceType()) {
        continue;
      }

      // there can only be one pointer/reference variable
      const auto *var_decl = var->getDecl();
      Lifetime &var_lifetime = State.GetLifetime(var_decl);
      if (var_lifetime < return_lifetime) {
        if (var_lifetime.IsNotSet()) {
          for (const auto &pair : var_lifetime.GetShortestLifetimes()) {
            if (pair.first == return_lifetime.GetId()) continue;
            S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
                << return_lifetime.GetLifetimeName()
                << var_lifetime.GetLifetimeName(pair.first)
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
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  if (debugEnabled) debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
