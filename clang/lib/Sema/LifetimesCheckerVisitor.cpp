#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"

namespace clang {

std::string GenerateArgName(const clang::VarDecl *arg,
                            unsigned int num_indirections) {
  return '\'' + std::string(num_indirections, '*') + arg->getNameAsString() +
         '\'';
}

PrintNotesFactory LifetimesCheckerVisitorFactory::BinAssignFactory() const {
  return [this](const clang::VarDecl *lhs_var_decl,
                const clang::VarDecl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, clang::Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr && op != nullptr);
    assert(lhs_lifetime.IsSet());
    if (rhs_lifetime.IsNotSet()) {
      const auto &rhs_possible_lifetimes = rhs_lifetime.GetPossibleLifetimes();
      for (unsigned int i = 0; i < rhs_possible_lifetimes.size(); i++) {
        if (rhs_possible_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
            << lhs_lifetime.GetLifetimeName() << Lifetime::GetLifetimeName(i)
            << op->getSourceRange();
        PrintNotes(rhs_lifetime, rhs_var_decl,
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
    assert(lhs_lifetime.IsSet());
    if (rhs_lifetime.IsNotSet()) {
      const auto &init_possible_lifetimes = rhs_lifetime.GetPossibleLifetimes();
      for (unsigned int i = 0; i < init_possible_lifetimes.size(); i++) {
        if (init_possible_lifetimes[i].empty() ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(lhs_var_decl->getInit()->getExprLoc(),
               diag::warn_decl_lifetimes_differ)
            << lhs_lifetime.GetLifetimeName() << Lifetime::GetLifetimeName(i)
            << lhs_var_decl->getInit()->getSourceRange();
        PrintNotes(rhs_lifetime, rhs_var_decl,
                   diag::note_lifetime_declared_here, i);
      }
    } else {
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
    assert(return_lifetime.IsSet());
    if (var_lifetime.IsNotSet()) {
      const auto &var_possible_lifetimes = var_lifetime.GetPossibleLifetimes();
      for (unsigned int i = 0; i < var_possible_lifetimes.size(); i++) {
        if (var_possible_lifetimes[i].empty() ||
            (char)i == return_lifetime.GetId())
          continue;
        S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
            << return_lifetime.GetLifetimeName() << Lifetime::GetLifetimeName(i)
            << return_stmt->getSourceRange();
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
      S.Diag(stmt->getBeginLoc(), msg)
          << Lifetime::GetLifetimeName(id) << stmt->getSourceRange();
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
        // TODO is the first part of the condition true?
        if (lhs_lifetime.IsSet() && rhs_lifetime < lhs_lifetime) {
          factory(lhs_var_decl, rhs_var_decl, op, expr, stmt, lhs_lifetime,
                  rhs_lifetime);
        }
        current_type = current_type->getPointeeType();
      }
    }
  }
}

void LifetimesCheckerVisitor::PrintNotes(const clang::VarDecl *var_decl,
                                         Lifetime &lifetime,
                                         unsigned int num_indirections) const {
  char id = lifetime.GetId();
  PrintNotes(var_decl, lifetime, id, num_indirections);
}

void LifetimesCheckerVisitor::PrintNotes(const clang::VarDecl *var_decl,
                                         Lifetime &lifetime, char id,
                                         unsigned int num_indirections) const {
  const auto &maybe_stmts = lifetime.GetStmts(id);
  if (maybe_stmts.has_value()) {
    const auto &stmts = maybe_stmts.value();
    for (const auto &stmt : stmts) {
      S.Diag(stmt->getBeginLoc(), diag::note_lifetime_of)
          << GenerateArgName(var_decl, num_indirections)
          << Lifetime::GetLifetimeName(id) << stmt->getSourceRange();
    }
  } else {
    PrintNotes(var_decl, id, num_indirections);
  }
}

void LifetimesCheckerVisitor::PrintNotes(const clang::VarDecl *var_decl,
                                         char id,
                                         unsigned int num_indirections) const {
  S.Diag(var_decl->getLocation(), diag::note_lifetime_of)
      << GenerateArgName(var_decl, num_indirections)
      << Lifetime::GetLifetimeName(id) << var_decl->getSourceRange();
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
    // debugWarn("LHS of bin_op is not pointer type");
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
  // debugWarn("Arg is not a declrefexpr or memberexpr");
  return nullptr;
}

void LifetimesCheckerVisitor::CallExprChecker(
    const clang::CallExpr *call, const clang::FunctionDecl *direct_callee,
    const clang::VarDecl *first_arg, const clang::VarDecl *second_arg,
    Lifetime &first_lifetime, Lifetime &second_lifetime,
    unsigned int first_num_indirections, unsigned int second_num_indirections,
    int msg) const {
  S.Diag(call->getExprLoc(), msg)
      << direct_callee << GenerateArgName(first_arg, first_num_indirections)
      << GenerateArgName(second_arg, second_num_indirections)
      << call->getSourceRange();
  if (first_lifetime.IsNotSet() && second_lifetime.IsNotSet()) {
    const auto &first_arg_possible_lifetimes =
        first_lifetime.GetPossibleLifetimes();
    const auto &second_arg_possible_lifetimes =
        second_lifetime.GetPossibleLifetimes();
    unsigned int max_size = std::max(first_arg_possible_lifetimes.size(),
                                     second_arg_possible_lifetimes.size());
    for (unsigned int i = 0; i < max_size; i++) {
      if ((i >= second_arg_possible_lifetimes.size() ||
           second_arg_possible_lifetimes[i].empty()) &&
          (i < first_arg_possible_lifetimes.size() &&
           !first_arg_possible_lifetimes[i].empty())) {
        PrintNotes(first_arg, first_lifetime, i, first_num_indirections);
      } else if ((i >= first_arg_possible_lifetimes.size() ||
                  first_arg_possible_lifetimes[i].empty()) &&
                 (i < second_arg_possible_lifetimes.size() &&
                  !second_arg_possible_lifetimes[i].empty())) {
        PrintNotes(second_arg, second_lifetime, i, second_num_indirections);
      }
    }
  } else if (first_lifetime.IsNotSet()) {
    const auto &first_arg_possible_lifetimes =
        first_lifetime.GetPossibleLifetimes();
    for (unsigned int i = 0; i < first_arg_possible_lifetimes.size(); i++) {
      if ((char)i == second_lifetime.GetId() ||
          first_arg_possible_lifetimes[i].empty())
        continue;
      PrintNotes(first_arg, first_lifetime, i, first_num_indirections);
    }
    PrintNotes(second_arg, second_lifetime, second_num_indirections);
  } else if (second_lifetime.IsNotSet()) {
    const auto &second_arg_possible_lifetimes =
        second_lifetime.GetPossibleLifetimes();
    for (unsigned int i = 0; i < second_arg_possible_lifetimes.size(); i++) {
      if ((char)i == first_lifetime.GetId() ||
          second_arg_possible_lifetimes[i].empty())
        continue;
      PrintNotes(second_arg, second_lifetime, i, second_num_indirections);
    }
    PrintNotes(first_arg, first_lifetime, first_num_indirections);
  } else {
    PrintNotes(second_arg, second_lifetime, second_num_indirections);
    PrintNotes(first_arg, first_lifetime, first_num_indirections);
  }
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

    auto &func_info = it->second;
    const auto &params_info_vec = func_info.GetParamsInfo();

    unsigned int num_args = call->getNumArgs();

    llvm::SmallVector<ParamInfo> params_set;
    params_set.resize(num_args);

    unsigned int num_indirections = params_info_vec.size();

    while (num_indirections-- > 0) {
      debugLifetimes("Indirection level ", num_indirections);
      llvm::SmallVector<llvm::SmallVector<ParamInfo>> param_lifetimes;

      // get lifetimes for the current indirection level
      debugLifetimes("Get lifetimes for current indirection level");
      for (const auto &param_info : params_set) {
        if (param_info.type.isNull()) continue;
        clang::QualType param_type = param_info.type->getPointeeType();
        params_set[param_info.index].type = param_type;
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(param_info.param, param_type);
        char lifetime_id = param_lifetime.GetId();
        if ((unsigned int)lifetime_id >= param_lifetimes.size()) {
          param_lifetimes.resize(lifetime_id + 1);
        }
        param_lifetimes[lifetime_id].emplace_back(param_info);
      }

      // check lifetimes of higher indirections
      debugLifetimes("Lifetimes of higher indirections");
      for (const auto &params : param_lifetimes) {
        if (params.size() < 2) continue;

        // ! param_lifetimes should have ParamInfo sorted
        auto it = params.begin();
        const ParamInfo *first_param_info = it;
        const auto *first_arg =
            GetDeclFromArg(call->getArg(first_param_info->index));
        if (first_arg == nullptr) continue;

        Lifetime &first_arg_lifetime = State.GetLifetime(first_arg, (it->type));

        while (++it != params.end()) {
          if (it->index != first_param_info->index) {
            const auto *current_arg = GetDeclFromArg(call->getArg(it->index));
            if (current_arg != nullptr) {
              Lifetime &current_arg_lifetime =
                  State.GetLifetime(current_arg, (it->type));
              if (first_arg_lifetime != current_arg_lifetime) {
                CallExprChecker(
                    call, direct_callee, first_arg, current_arg,
                    first_arg_lifetime, current_arg_lifetime,
                    first_param_info->num_indirections - num_indirections,
                    it->num_indirections - num_indirections,
                    diag::warn_func_params_lifetimes_equal);
                return std::nullopt;
              }
            }
          }
        }
      }

      // check lifetimes of current indirection level
      debugLifetimes("Lifetimes of current level");
      if (!param_lifetimes.empty()) {
        for (const auto &param_info : params_info_vec[num_indirections]) {
          Lifetime &current_lifetime =
              func_info.GetParamLifetime(param_info.param, param_info.type);
          char current_id = current_lifetime.GetId();

          if (param_lifetimes.size() <= (unsigned int)current_id) {
            continue;
          }

          const auto &filtered_params = param_lifetimes[current_id];
          const auto *current_arg =
              GetDeclFromArg(call->getArg(param_info.index));
          if (current_arg == nullptr) continue;
          clang::QualType type = current_arg->getType().getCanonicalType();
          Lifetime &current_arg_lifetime =
              State.GetLifetimeOrLocal(current_arg, type);
          for (const auto &other_param_info : filtered_params) {
            const auto *arg = call->getArg(other_param_info.index);
            const auto *arg_decl = GetDeclFromArg(arg);
            if (arg_decl == nullptr) continue;
            Lifetime &arg_lifetime = State.GetLifetimeOrLocal(arg_decl, type);
            if (current_arg_lifetime < arg_lifetime) {
              CallExprChecker(
                  call, direct_callee, current_arg, arg_decl,
                  current_arg_lifetime, arg_lifetime,
                  param_info.num_indirections - num_indirections,
                  other_param_info.num_indirections - num_indirections,
                  diag::warn_func_params_lifetimes_shorter);
            }
          }
        }
      }

      // insert params for the next indirection level
      debugLifetimes("Insert params for the next indirection level");
      for (const auto &param_info : params_info_vec[num_indirections]) {
        assert(param_info.index < params_set.size());
        params_set[param_info.index] = param_info;
      }

      // check static params
      debugLifetimes("Check static params");
      for (const auto &param_info : params_set) {
        if (param_info.type.isNull()) continue;
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(param_info.param, param_info.type);
        if (param_lifetime.IsStatic()) {
          const auto *arg = call->getArg(param_info.index);
          const auto *arg_decl = GetDeclFromArg(arg);
          if (arg_decl == nullptr) continue;
          Lifetime &arg_lifetime =
              State.GetLifetimeOrLocal(arg_decl, param_info.type);
          if (!arg_lifetime.IsStatic()) {
            if (arg_lifetime.IsNotSet()) {
              const auto &arg_possible_lifetimes =
                  arg_lifetime.GetPossibleLifetimes();
              for (unsigned int i = 0; i < arg_possible_lifetimes.size(); i++) {
                if (arg_possible_lifetimes[i].empty()) continue;
                S.Diag(arg->getExprLoc(), diag::arg_lifetimes_differ)
                    << GenerateArgName(arg_decl, param_info.num_indirections -
                                                     num_indirections)
                    << Lifetime::GetLifetimeName(STATIC)
                    << Lifetime::GetLifetimeName(i) << call->getSourceRange();
                PrintNotes(arg_decl, arg_lifetime, i,
                           param_info.num_indirections - num_indirections);
              }
              PrintNotes(param_info.param, STATIC,
                         param_info.num_indirections - num_indirections);

            } else {
              S.Diag(arg->getExprLoc(), diag::arg_lifetimes_differ)
                  << GenerateArgName(arg_decl, param_info.num_indirections -
                                                   num_indirections)
                  << Lifetime::GetLifetimeName(STATIC)
                  << arg_lifetime.GetLifetimeName() << call->getSourceRange();
              PrintNotes(arg_decl, arg_lifetime,
                         param_info.num_indirections - num_indirections);
            }
            PrintNotes(param_info.param, STATIC,
                       param_info.num_indirections - num_indirections);
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
    if (child == nullptr) continue;
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
