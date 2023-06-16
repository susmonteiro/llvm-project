#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"

namespace clang {

PrintNotesFactory LifetimesCheckerVisitorFactory::BinAssignFactory() const {
  return [this](const clang::VarDecl *lhs_var_decl,
                const clang::VarDecl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, clang::Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr && op != nullptr);
    if (lhs_lifetime.IsNotSet()) {
      debugWarn("LHS LIFETIME IS NOT SET");
      // TODO should this case exist?
      // ! if LHS lifetime is not set then it will have all
      // lifetimes from ! rhs because of the propagation
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
      const auto &rhs_shortest_lifetimes = rhs_lifetime.GetShortestLifetimes();
      for (unsigned int i = 0; i < rhs_shortest_lifetimes.size(); i++) {
        if (rhs_shortest_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
            << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName(i)
            << op->getSourceRange();
        PrintNotes(lhs_lifetime, lhs_var_decl,
                   diag::note_lifetime_declared_here, i);
      }
      PrintNotes(lhs_lifetime, lhs_var_decl, diag::note_lifetime_declared_here);
    } else {
      S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
          << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName()
          << op->getSourceRange();
      PrintNotes(rhs_lifetime, rhs_var_decl, diag::note_lifetime_declared_here);
      PrintNotes(lhs_lifetime, lhs_var_decl, diag::note_lifetime_declared_here);
    }
  };
}

PrintNotesFactory LifetimesCheckerVisitorFactory::DeclStmtFactory() const {
  return [this](const clang::VarDecl *lhs_var_decl,
                const clang::VarDecl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr);
    if (rhs_lifetime.IsNotSet()) {
      const auto &init_shortest_lifetimes = rhs_lifetime.GetShortestLifetimes();
      for (unsigned int i = 0; i < init_shortest_lifetimes.size(); i++) {
        if (init_shortest_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;

        S.Diag(lhs_var_decl->getInit()->getExprLoc(),
               diag::warn_decl_lifetimes_differ)
            << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName(i)
            << lhs_var_decl->getInit()->getSourceRange();
        PrintNotes(rhs_lifetime, rhs_var_decl,
                   diag::note_lifetime_declared_here, i);
      }
    } else {
      // TODO maybe change warning to "declaration" instead of "assign"
      // (also above)
      S.Diag(lhs_var_decl->getInitializingDeclaration()->getLocation(),
             diag::warn_decl_lifetimes_differ)
          << lhs_lifetime.GetLifetimeName() << rhs_lifetime.GetLifetimeName()
          << lhs_var_decl->getInitializingDeclaration()->getSourceRange();
      PrintNotes(rhs_lifetime, rhs_var_decl, diag::note_lifetime_declared_here);
    }
  };
}

PrintNotesFactory LifetimesCheckerVisitorFactory::ReturnStmtFactory() const {
  return [this](const clang::VarDecl *_lhs_var_decl,
                const clang::VarDecl *var_decl,
                const clang::BinaryOperator *_op, const clang::Expr *expr,
                const clang::Stmt *return_stmt, Lifetime &return_lifetime,
                Lifetime &var_lifetime) {
    assert(var_decl != nullptr && expr != nullptr && return_stmt != nullptr);
    if (var_lifetime.IsNotSet()) {
      const auto &var_shortest_lifetimes = var_lifetime.GetShortestLifetimes();
      for (unsigned int i = 0; i < var_shortest_lifetimes.size(); i++) {
        if (var_shortest_lifetimes[i].empty() ||
            (char)i == return_lifetime.GetId())
          continue;
        S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
            << return_lifetime.GetLifetimeName()
            << var_lifetime.GetLifetimeName(i) << return_stmt->getSourceRange();
        PrintNotes(var_lifetime, var_decl, diag::note_lifetime_declared_here,
                   (char)i);
      }
    } else {
      S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
          << return_lifetime.GetLifetimeName() << var_lifetime.GetLifetimeName()
          << return_stmt->getSourceRange();
      PrintNotes(var_lifetime, var_decl, diag::note_lifetime_declared_here);
    }
  };
}

void LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                                const clang::VarDecl *var_decl,
                                                int msg) const {
  char id = lifetime.GetId();
  PrintNotes(lifetime, var_decl, msg, id);
}

void LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                                clang::SourceLocation loc,
                                                clang::SourceRange range,
                                                int msg) const {
  char id = lifetime.GetId();
  PrintNotes(lifetime, loc, range, msg, id);
}

void LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                                const clang::VarDecl *var_decl,
                                                int msg, char id) const {
  PrintNotes(lifetime, var_decl->getLocation(), var_decl->getSourceRange(), msg,
             id);
}

void LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                                clang::SourceLocation loc,
                                                clang::SourceRange range,
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
    S.Diag(loc, msg) << lifetime.GetLifetimeName() << range;
  }
}

void LifetimesCheckerVisitor::VerifyBinAssign(
    clang::QualType lhs_type, const clang::Expr *rhs, const clang::Expr *expr,
    const llvm::SmallSet<const clang::Expr *, 2U> &rhs_points_to,
    const clang::BinaryOperator *op, PrintNotesFactory factory) const {
  if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) {
    return;
  }

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr);
  if (const auto *lhs_var_decl =
          dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
    for (const auto &expr : rhs_points_to) {
      CompareAndCheck(lhs_var_decl, lhs_type, expr, rhs, nullptr, op, false,
                      factory);
    }
  }
}

void LifetimesCheckerVisitor::CompareAndCheck(
    const clang::VarDecl *lhs_var_decl, clang::QualType lhs_type,
    const clang::Expr *expr, const clang::Expr *rhs, const clang::Stmt *stmt,
    const clang::BinaryOperator *op, bool return_lifetime,
    PrintNotesFactory factory) const {
  if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
    const auto *rhs_decl_ref_expr = clang::dyn_cast<clang::DeclRefExpr>(expr);
    // there can only be one pointer/reference variable
    if (const auto *rhs_var_decl =
            dyn_cast<clang::VarDecl>(rhs_decl_ref_expr->getDecl())) {
      clang::QualType current_type = lhs_type;
      while (current_type->isPointerType() || current_type->isReferenceType()) {
        Lifetime &lhs_lifetime =
            return_lifetime ? State.GetReturnLifetime(current_type)
                            : State.GetLifetime(lhs_var_decl, current_type);
        Lifetime &rhs_lifetime =
            State.GetLifetimeOrLocal(rhs_var_decl, current_type);
        if (rhs_lifetime < lhs_lifetime) {
          factory(lhs_var_decl, rhs_var_decl, op, expr, stmt, lhs_lifetime,
                  rhs_lifetime);
        }
        current_type = current_type->getPointeeType();
      }
    }
  }
}

std::optional<std::string> LifetimesCheckerVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes.
  // We don't want to change points-to sets in those cases.
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

  clang::QualType lhs_type = lhs->getType().getCanonicalType();

  if (const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs)) {
    VerifyBinAssign(lhs_type, rhs, lhs_decl_ref_expr, rhs_points_to, op,
                    Factory.BinAssignFactory());
  }

  for (const auto &expr : lhs_points_to) {
    VerifyBinAssign(lhs_type, rhs, expr, rhs_points_to, op,
                    Factory.BinAssignFactory());
  }
  return std::nullopt;
}

const clang::VarDecl *LifetimesCheckerVisitor::GetDeclFromArg(
    const clang::Expr *arg) const {
  const auto &arg_points_to = PointsTo.GetExprPointsTo(arg->IgnoreParens());
  for (const auto &expr : arg_points_to) {
    if (expr == nullptr) continue;
    if (const auto *decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr)) {
      return dyn_cast<clang::VarDecl>(decl_ref_expr->getDecl());
    } else if (const auto *member_expr = dyn_cast<clang::MemberExpr>(arg)) {
      return dyn_cast<clang::VarDecl>(member_expr->getMemberDecl());
    }
  }
  debugWarn("Arg is not a declrefexpr or memberexpr");
  return nullptr;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
  if (debugEnabled) debugLifetimes("[VisitCallExpr]");

  const clang::FunctionDecl *direct_callee = call->getDirectCallee();
  if (direct_callee) {
    auto it = FuncInfo.find(direct_callee);
    if (it == FuncInfo.end()) {
      // TODO error
      debugWarn("Did not find function in FuncInfo");
      return std::nullopt;
    }

    auto &func_info = FuncInfo[direct_callee];
    const auto &params_by_type = func_info.GetParamsByType();
    llvm::DenseMap<const clang::ParmVarDecl *,
                   std::pair<clang::QualType, unsigned int>>
        params_set;

    unsigned int num_indirections = params_by_type.size();

    while (num_indirections-- != 0) {
      llvm::DenseMap<char,
                     llvm::DenseSet<std::pair<clang::QualType, unsigned int>>>
          param_lifetimes;
      // get lifetimes for the current indirection level
      for (const auto &pair : params_set) {
        clang::QualType param_type = pair.second.first->getPointeeType();
        params_set[pair.first].first = param_type;
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(pair.first, param_type);
        param_lifetimes[param_lifetime.GetIdNoOffset()].insert(pair.second);
      }
      // check lifetimes of higher indirections
      for (const auto &pair : param_lifetimes) {
        if (pair.second.size() < 2) continue;
        auto it = pair.second.begin();
        const auto *previous_arg = GetDeclFromArg(call->getArg(it->second));
        assert(previous_arg != nullptr);
        Lifetime &previous = State.GetLifetime(previous_arg, (it->first));
        while (++it != pair.second.end()) {
          const auto *current_arg = GetDeclFromArg(call->getArg(it->second));
          assert(current_arg != nullptr);
          Lifetime &current = State.GetLifetime(current_arg, (it->first));
          if (previous != current) {
            // TODO change notes
            S.Diag(call->getExprLoc(), diag::warn_func_params_lifetimes_equal)
                << direct_callee << previous_arg << current_arg
                << call->getSourceRange();
            S.Diag(previous_arg->getLocation(), diag::note_lifetime_of)
                << previous_arg << previous.GetLifetimeName();
            S.Diag(current_arg->getLocation(), diag::note_lifetime_of)
                << current_arg << current.GetLifetimeName();
            return std::nullopt;
          }
        }
      }
      
      // check lifetimes of current indirection level
      if (!param_lifetimes.empty()) {
        for (const auto &param_pair : params_by_type[num_indirections]) {
          Lifetime &current_lifetime = func_info.GetParamLifetime(
              param_pair.first, param_pair.first->getType());

          auto it = param_lifetimes.find(current_lifetime.GetIdNoOffset());
          if (it == param_lifetimes.end()) continue;

          const auto &filtered_params = it->second;
          const auto *current_arg =
              GetDeclFromArg(call->getArg(param_pair.second));
          clang::QualType type = current_arg->getType().getCanonicalType();
          Lifetime &current_arg_lifetime =
              State.GetLifetimeOrLocal(current_arg, type);

          for (const auto &pair : filtered_params) {
            const auto *arg = call->getArg(pair.second);
            const auto *arg_decl = GetDeclFromArg(arg);
            assert(arg_decl != nullptr);
            Lifetime &arg_lifetime = State.GetLifetimeOrLocal(arg_decl, type);
            if (current_arg_lifetime < arg_lifetime) {
              // TODO change this
              S.Diag(call->getExprLoc(),
                     diag::warn_func_params_lifetimes_shorter)
                  << direct_callee << current_arg << arg_decl
                  << call->getSourceRange();
              S.Diag(current_arg->getLocation(), diag::note_lifetime_of)
                  << current_arg << current_arg_lifetime.GetLifetimeName();
              S.Diag(arg_decl->getLocation(), diag::note_lifetime_of)
                  << arg_decl << arg_lifetime.GetLifetimeName();
            }
          }
        }
      }
      // insert params for the next indirection level
      for (const auto &param_pair : params_by_type[num_indirections]) {
        params_set[param_pair.first] = std::pair(
            param_pair.first->getType().getCanonicalType(), param_pair.second);
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
        continue;
      }
      clang::QualType var_decl_type = var_decl->getType().getCanonicalType();
      Lifetime &var_decl_lifetime = State.GetLifetime(var_decl, var_decl_type);
      // no initializer, nothing to check
      if (!var_decl->hasInit() || var_decl_lifetime.IsNotSet())
        return std::nullopt;

      const auto *init = var_decl->getInit()->IgnoreParens();
      const auto &init_points_to = PointsTo.GetExprPointsTo(init);

      // TODO remove this
      if (init_points_to.empty()) {
        debugWarn("Initializer is not in PointsToMap");
        return std::nullopt;
      }

      CompareAndCheck(var_decl, var_decl_type, init, init, nullptr, nullptr,
                      false, Factory.DeclStmtFactory());

      for (const auto &expr : init_points_to) {
        CompareAndCheck(var_decl, var_decl_type, expr, init, nullptr, nullptr,
                        false, Factory.DeclStmtFactory());
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  if (debugEnabled) debugLifetimes("[VisitExpr]");
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

  const auto &return_value = return_stmt->getRetValue()->IgnoreParens();

  // TODO remove this
  if (PointsTo.IsEmpty(return_value)) {
    debugWarn("Return expr is not in PointsToMap");
    Visit(const_cast<clang::Expr *>(return_value));
  }

  CompareAndCheck(nullptr, return_type, return_value, return_value, return_stmt,
                  nullptr, true, Factory.ReturnStmtFactory());

  const auto &return_expr = PointsTo.GetExprPointsTo(return_value);
  for (const auto &expr : return_expr) {
    CompareAndCheck(nullptr, return_type, expr, return_value, return_stmt,
                    nullptr, true, Factory.ReturnStmtFactory());
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
