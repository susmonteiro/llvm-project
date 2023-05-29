#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"

namespace clang {

void LifetimesCheckerVisitor::PrintNotes(Lifetime &lifetime,
                                         const clang::NamedDecl *var_decl,
                                         int msg) const {
  char id = lifetime.GetId();
  PrintNotes(lifetime, var_decl, msg, id);
}

void LifetimesCheckerVisitor::PrintNotes(Lifetime &lifetime,
                                         const clang::NamedDecl *var_decl,
                                         int msg, char id) const {
  const auto &maybe_stmts = lifetime.GetStmts(id);
  if (maybe_stmts.has_value()) {
    const auto &stmts = maybe_stmts.value();
    for (const auto &stmt : stmts) {
      // TODO try to print in a better place
      S.Diag(stmt->getBeginLoc(), msg)
          << lifetime.GetLifetimeName(id) << stmt->getSourceRange();
    }
  } else {
    S.Diag(var_decl->getLocation(), msg)
        << lifetime.GetLifetimeName() << var_decl->getSourceRange();
  }
}

std::optional<std::string> LifetimesCheckerVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();

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

  const auto &rhs = op->getRHS()->IgnoreParens();
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
          if (lhs_lifetime.IsNotSet()) {
            debugWarn("LHS LIFETIME IS NOT SET");
            // TODO should this case exist?
            // ! if LHS lifetime is not set then it will have all lifetimes from
            // ! rhs because of the propagation
            //   for (char l : lhs_lifetime.GetShortestLifetimes()) {
            //     if (l == rhs_lifetime.GetId()) continue;
            //     S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
            //         << rhs_lifetime.GetLifetimeName()
            //         << lhs_lifetime.GetLifetimeName(l) <<
            //         op->getSourceRange();
            //   }
            //   // TODO implement the notes in this case
          } else if (rhs_lifetime.IsNotSet()) {
            const auto &rhs_shortest_lifetimes =
                rhs_lifetime.GetShortestLifetimes();
            for (unsigned int i = 0; i < rhs_shortest_lifetimes.size(); i++) {
              if (rhs_shortest_lifetimes[i].empty() ||
                  (char)i == lhs_lifetime.GetId())
                continue;
              S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                  << lhs_lifetime.GetLifetimeName()
                  << rhs_lifetime.GetLifetimeName(i) << op->getSourceRange();
              PrintNotes(lhs_lifetime, lhs_decl,
                         diag::note_lifetime_declared_here, i);
            }
            PrintNotes(lhs_lifetime, lhs_decl,
                       diag::note_lifetime_declared_here);
          } else {
            S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                << lhs_lifetime.GetLifetimeName()
                << rhs_lifetime.GetLifetimeName() << op->getSourceRange();
            PrintNotes(rhs_lifetime, rhs_decl,
                       diag::note_lifetime_declared_here);
            PrintNotes(lhs_lifetime, lhs_decl,
                       diag::note_lifetime_declared_here);
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
      if (!var_decl->getType()->isPointerType() &&
          !var_decl->getType()->isReferenceType()) {
        debugWarn("Var decl is not pointer type");
        continue;
      }
      Lifetime &var_decl_lifetime = State.GetLifetime(var_decl);
      // no initializer, nothing to check
      if (!var_decl->hasInit() || var_decl_lifetime.IsNotSet())
        return std::nullopt;

      const auto &init_points_to =
          PointsTo.GetExprPointsTo(var_decl->getInit()->IgnoreParens());
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
              const auto &init_shortest_lifetimes =
                  init_lifetime.GetShortestLifetimes();
              for (unsigned int i = 0; i < init_shortest_lifetimes.size();
                   i++) {
                if (init_shortest_lifetimes[i].empty() ||
                    (char)i == var_decl_lifetime.GetId())
                  continue;
                S.Diag(var_decl->getInit()->getExprLoc(),
                       diag::warn_assign_lifetimes_differ)
                    << var_decl_lifetime.GetLifetimeName()
                    << init_lifetime.GetLifetimeName(i)
                    << var_decl->getInit()->getSourceRange();
                PrintNotes(init_lifetime, init_var->getDecl(),
                           diag::note_lifetime_declared_here, i);
              }
            } else {
              // TODO put loc in the '=' sign (also for above)
              // TODO maybe change warning to "declaration" instead of "assign"
              // (also above)
              S.Diag(var_decl->getInitializingDeclaration()->getLocation(),
                     diag::warn_assign_lifetimes_differ)
                  << var_decl_lifetime.GetLifetimeName()
                  << init_lifetime.GetLifetimeName()
                  << var_decl->getInitializingDeclaration()->getSourceRange();
              PrintNotes(init_lifetime, init_var->getDecl(),
                         diag::note_lifetime_declared_here);
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

  clang::QualType return_type = Func->getReturnType().IgnoreParens();

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

  const auto &return_value = return_stmt->getRetValue()->IgnoreParens();

  const auto &return_expr = PointsTo.GetExprPointsTo(return_value);
  // TODO remove this
  if (return_expr.empty()) {
    debugWarn("Return expr is not in PointsToMap");
    Visit(const_cast<clang::Expr *>(return_value));
    const auto &lhs_points_to = PointsTo.GetExprPointsTo(return_value);
  }

  for (const auto &expr : return_expr) {
    if (expr != nullptr &&
        clang::isa<clang::DeclRefExpr>(expr->IgnoreParens())) {
      const auto *var =
          clang::dyn_cast<clang::DeclRefExpr>(expr->IgnoreParens());
      clang::QualType var_type = var->getType().IgnoreParens();
      if (!var_type->isPointerType() && !var_type->isReferenceType()) {
        continue;
      }

      // there can only be one pointer/reference variable
      const auto *var_decl = var->getDecl();
      Lifetime &var_lifetime = State.GetLifetime(var_decl);
      if (var_lifetime < return_lifetime) {
        if (var_lifetime.IsNotSet()) {
          const auto &var_shortest_lifetimes =
              var_lifetime.GetShortestLifetimes();
          for (unsigned int i = 0; i < var_shortest_lifetimes.size(); i++) {
            if (var_shortest_lifetimes[i].empty() ||
                (char)i == return_lifetime.GetId())
              continue;
            S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
                << return_lifetime.GetLifetimeName()
                << var_lifetime.GetLifetimeName(i)
                << return_stmt->getSourceRange();
            PrintNotes(var_lifetime, var_decl,
                       diag::note_lifetime_declared_here, (char)i);
          }
        } else {
          S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
              << return_lifetime.GetLifetimeName()
              << var_lifetime.GetLifetimeName()
              << return_stmt->getSourceRange();
          PrintNotes(var_lifetime, var_decl, diag::note_lifetime_declared_here);
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
