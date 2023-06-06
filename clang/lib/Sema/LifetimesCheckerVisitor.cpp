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
                                         clang::SourceLocation loc,
                                         clang::SourceRange range,
                                         int msg) const {
  char id = lifetime.GetId();
  PrintNotes(lifetime, loc, range, msg, id);
}

void LifetimesCheckerVisitor::PrintNotes(Lifetime &lifetime,
                                         const clang::NamedDecl *var_decl,
                                         int msg, char id) const {
  PrintNotes(lifetime, var_decl->getLocation(), var_decl->getSourceRange(), msg,
             id);
}

void LifetimesCheckerVisitor::PrintNotes(Lifetime &lifetime,
                                         clang::SourceLocation loc,
                                         clang::SourceRange range, int msg,
                                         char id) const {
  const auto &maybe_stmts = lifetime.GetStmts(id);
  if (maybe_stmts.has_value()) {
    const auto &stmts = maybe_stmts.value();
    for (const auto &stmt : stmts) {
      // TODO try to print in a better place
      S.Diag(stmt->getBeginLoc(), msg)
          << lifetime.GetLifetimeName(id) << stmt->getSourceRange();
    }
  } else {
    S.Diag(loc, msg) << lifetime.GetLifetimeName() << range;
  }
}

void LifetimesCheckerVisitor::CompareAndCheckLifetimes(
    Lifetime &lhs_lifetime, Lifetime &rhs_lifetime,
    const clang::VarDecl *lhs_var_decl, const clang::ValueDecl *rhs_var_decl,
    int warn, int note) const {
  if (rhs_lifetime < lhs_lifetime) {
    if (rhs_lifetime.IsNotSet()) {
      const auto &init_shortest_lifetimes = rhs_lifetime.GetShortestLifetimes();
      for (unsigned int i = 0; i < init_shortest_lifetimes.size(); i++) {
        if (init_shortest_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(lhs_var_decl->getInit()->getExprLoc(), warn)
            << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName(i)
            << lhs_var_decl->getInit()->getSourceRange();
        PrintNotes(rhs_lifetime, rhs_var_decl, note, i);
      }
    } else {
      // TODO maybe change warning to "declaration" instead of "assign"
      // (also above)
      S.Diag(lhs_var_decl->getInitializingDeclaration()->getLocation(), warn)
          << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName()
          << lhs_var_decl->getInitializingDeclaration()->getSourceRange();
      PrintNotes(rhs_lifetime, rhs_var_decl, note);
    }
  }
}

void LifetimesCheckerVisitor::CompareAndCheckLifetimes(
    Lifetime &lhs_lifetime, Lifetime &rhs_lifetime,
    const clang::VarDecl *lhs_var_decl, clang::SourceLocation loc,
    clang::SourceRange range, int warn, int note) const {
  if (rhs_lifetime < lhs_lifetime) {
    if (rhs_lifetime.IsNotSet()) {
      const auto &init_shortest_lifetimes = rhs_lifetime.GetShortestLifetimes();
      for (unsigned int i = 0; i < init_shortest_lifetimes.size(); i++) {
        if (init_shortest_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(lhs_var_decl->getInit()->getExprLoc(), warn)
            << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName(i)
            << lhs_var_decl->getInit()->getSourceRange();
        PrintNotes(rhs_lifetime, loc, range, note, i);
      }
    } else {
      // TODO maybe change warning to "declaration" instead of "assign"
      // (also above)
      S.Diag(lhs_var_decl->getInitializingDeclaration()->getLocation(), warn)
          << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName()
          << lhs_var_decl->getInitializingDeclaration()->getSourceRange();
      PrintNotes(rhs_lifetime, loc, range, note);
    }
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
  if (const auto *lhs_var_decl = dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
    // Check that lhs_lifetime >= rhs_lifetime
    Lifetime &lhs_lifetime =
        State.GetLifetime(lhs_var_decl, lhs_var_decl->getType());

    for (const auto &expr : rhs_points_to) {
      if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
        const auto *rhs_var = clang::dyn_cast<clang::DeclRefExpr>(expr);
        clang::QualType rhs_var_type = rhs_var->getType();
        if (!rhs_var_type->isPointerType() &&
            !rhs_var_type->isReferenceType()) {
          continue;
        }

        // there can only be one pointer/reference variable
        if (const auto *rhs_var_decl =
                dyn_cast<clang::VarDecl>(rhs_var->getDecl())) {
          Lifetime &rhs_lifetime =
              State.GetLifetime(rhs_var_decl, rhs_var_type);
          if (rhs_lifetime < lhs_lifetime) {
            if (lhs_lifetime.IsNotSet()) {
              debugWarn("LHS LIFETIME IS NOT SET");
              // TODO should this case exist?
              // ! if LHS lifetime is not set then it will have all lifetimes
              // from ! rhs because of the propagation
              //   for (char l : lhs_lifetime.GetShortestLifetimes()) {
              //     if (l == rhs_lifetime.GetId()) continue;
              //     S.Diag(op->getExprLoc(),
              //     diag::warn_assign_lifetimes_differ)
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
                PrintNotes(lhs_lifetime, lhs_var_decl,
                           diag::note_lifetime_declared_here, i);
              }
              PrintNotes(lhs_lifetime, lhs_var_decl,
                         diag::note_lifetime_declared_here);
            } else {
              S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                  << lhs_lifetime.GetLifetimeName()
                  << rhs_lifetime.GetLifetimeName() << op->getSourceRange();
              PrintNotes(rhs_lifetime, rhs_var_decl,
                         diag::note_lifetime_declared_here);
              PrintNotes(lhs_lifetime, lhs_var_decl,
                         diag::note_lifetime_declared_here);
            }
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
      Lifetime &var_decl_lifetime =
          State.GetLifetime(var_decl, var_decl->getType());
      // no initializer, nothing to check
      if (!var_decl->hasInit() || var_decl_lifetime.IsNotSet())
        return std::nullopt;

      const auto *init = var_decl->getInit()->IgnoreParens();

      if (const auto *unary_op =
              dyn_cast<clang::UnaryOperator>(var_decl->getInit())) {
        if (unary_op->getOpcode() == UO_AddrOf) {
          debugInfo("The rhs is unary operator &");
          Lifetime init_lifetime = Lifetime(LOCAL);
          CompareAndCheckLifetimes(var_decl_lifetime, init_lifetime, var_decl,
                                   init->getExprLoc(), init->getSourceRange(),
                                   diag::warn_assign_lifetimes_differ,
                                   diag::note_lifetime_declared_here);
        }
      }

      const auto &init_points_to = PointsTo.GetExprPointsTo(init);
      // TODO remove this
      if (init_points_to.empty()) {
        debugWarn("Initializer is not in PointsToMap");
      }

      for (const auto &expr : init_points_to) {
        debugWarn("Inside loop");
        expr->dump();
        if (expr == nullptr) continue;
        if (clang::isa<clang::DeclRefExpr>(expr)) {
          debugWarn("It is a DeclRefExpr");
          const auto &init_var = clang::dyn_cast<clang::DeclRefExpr>(expr);
          clang::QualType init_var_type = init_var->getType();
          if (!init_var_type->isPointerType() &&
              !init_var_type->isReferenceType()) {
            continue;
          }

          if (const auto *init_var_decl =
                  dyn_cast<clang::VarDecl>(init_var->getDecl())) {
            Lifetime &init_lifetime =
                State.GetLifetime(init_var_decl, init_var_type);

            // TODO check this
            CompareAndCheckLifetimes(var_decl_lifetime, init_lifetime, var_decl,
                                     init_var->getDecl(),
                                     diag::warn_assign_lifetimes_differ,
                                     diag::note_lifetime_declared_here);
          }
        } else if (clang::isa<clang::UnaryOperator>(expr)) {
          debugLifetimes("Is the unary operator pointer type?",
                         expr->getType()->isPointerType() ||
                             expr->getType()->isReferenceType());
          Lifetime init_lifetime = Lifetime(LOCAL);
        } else {
          debugWarn("No option was good");
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

  Lifetime &return_lifetime = State.GetReturnLifetime(return_type);
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
      if (const auto *var_decl = dyn_cast<clang::VarDecl>(var->getDecl())) {
        Lifetime &var_lifetime = State.GetLifetime(var_decl, var_type);
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

std::optional<std::string> LifetimesCheckerVisitor::VisitUnaryAddrOf(
    const clang::UnaryOperator *op) {
  // TODO implement
  if (debugEnabled) debugLifetimes("[VisitUnaryAddrOf]");
  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

// TODO sometimes this is not being visited
std::optional<std::string> LifetimesCheckerVisitor::VisitUnaryDeref(
    const clang::UnaryOperator *op) {
  // TODO implement
  if (debugEnabled) debugLifetimes("[VisitUnaryDeref]");
  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

// TODO delete this
std::optional<std::string> LifetimesCheckerVisitor::VisitUnaryOperator(
    const clang::UnaryOperator *op) {
  // TODO implement
  if (debugEnabled) debugLifetimes("[VisitUnaryAddrOf]");
  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
