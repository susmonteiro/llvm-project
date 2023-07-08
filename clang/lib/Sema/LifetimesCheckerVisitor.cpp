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

void LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                                const clang::Decl *var_decl,
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
                                                const clang::Decl *var_decl,
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

PrintNotesFactory LifetimesCheckerVisitorFactory::BinAssignFactory() const {
  return [this](const clang::VarDecl *lhs_var_decl,
                const clang::Decl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, clang::Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr && op != nullptr);
    assert(lhs_lifetime.IsSet());
    if (rhs_lifetime.IsNotSet()) {
      unsigned int possible_lifetimes_size =
          rhs_lifetime.GetPossibleLifetimes().size();
      for (unsigned int i = OFFSET; i < possible_lifetimes_size; i++) {
        if (!rhs_lifetime.ContainsShortestLifetime(i) ||
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
                const clang::Decl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr);
    assert(lhs_lifetime.IsSet());
    if (rhs_lifetime.IsNotSet()) {
      unsigned int init_possible_lifetimes_size =
          rhs_lifetime.GetPossibleLifetimes().size();
      for (unsigned int i = 0; i < init_possible_lifetimes_size; i++) {
        if (!rhs_lifetime.ContainsShortestLifetime(i) ||
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
                const clang::Decl *var_decl, const clang::BinaryOperator *_op,
                const clang::Expr *expr, const clang::Stmt *return_stmt,
                Lifetime &return_lifetime, Lifetime &var_lifetime) {
    assert(var_decl != nullptr && expr != nullptr && return_stmt != nullptr);
    assert(return_lifetime.IsSet());
    if (var_lifetime.IsNotSet()) {
      unsigned int var_possible_lifetimes_size =
          var_lifetime.GetPossibleLifetimes().size();
      for (unsigned int i = 0; i < var_possible_lifetimes_size; i++) {
        if (!var_lifetime.ContainsShortestLifetime(i) ||
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

void LifetimesCheckerVisitor::VerifyBinAssign(
    clang::QualType lhs_type, clang::QualType base_type, const clang::Expr *rhs,
    const clang::Expr *expr,
    const llvm::SmallSet<const clang::Expr *, 2U> &rhs_points_to,
    const clang::BinaryOperator *op, PrintNotesFactory factory) const {
  if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) {
    return;
  }
  clang::QualType rhs_type = PointsTo.GetExprType(rhs);
  rhs_type = rhs_type.isNull() ? base_type : rhs_type;

  const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr);
  if (const auto *lhs_var_decl =
          dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
    CompareAndCheck(lhs_var_decl, lhs_type, rhs, rhs, rhs_type, nullptr, op,
                    false, factory);

    for (const auto &expr : rhs_points_to) {
      CompareAndCheck(lhs_var_decl, lhs_type, expr, rhs, rhs_type, nullptr, op,
                      false, factory);
    }
  }
}

void LifetimesCheckerVisitor::VerifyMaxLifetimes(
    const clang::Expr *lhs, const clang::Expr *rhs,
    const clang::BinaryOperator *op,
    const llvm::SmallSet<const clang::Expr *, 2U> &lhs_points_to,
    const llvm::SmallSet<const clang::Expr *, 2U> &rhs_points_to) const {
  assert(clang::isa<clang::UnaryOperator>(lhs));
  const clang::UnaryOperator *deref_op = clang::dyn_cast<UnaryOperator>(lhs);
  if (deref_op->getOpcode() != clang::UO_Deref ||
      op->getType().getCanonicalType() !=
          deref_op->getType().getCanonicalType())
    return;

  for (const auto &expr : lhs_points_to) {
    if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) continue;
    const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr);
    if (const auto *lhs_var_decl =
            dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
      clang::QualType lhs_type = PointsTo.GetExprType(lhs);
      lhs_type =
          lhs_type.isNull() ? lhs->getType().getCanonicalType() : lhs_type;
      Lifetime &lhs_lifetime = State.GetLifetime(lhs_var_decl, lhs_type);
      for (const auto &expr : rhs_points_to) {
        if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) continue;
        const auto *rhs_decl_ref_expr =
            clang::dyn_cast<clang::DeclRefExpr>(expr);
        if (const auto *rhs_var_decl =
                dyn_cast<clang::VarDecl>(rhs_decl_ref_expr->getDecl())) {
          if (!lhs_type->isPointerType() && !lhs_type->isReferenceType())
            continue;
          clang::QualType rhs_type = PointsTo.GetExprType(rhs);
          rhs_type =
              rhs_type.isNull() ? lhs->getType().getCanonicalType() : rhs_type;
          Lifetime &rhs_lifetime =
              State.GetLifetimeOrLocal(rhs_var_decl, rhs_type);
          unsigned int i = LOCAL - 1;
          unsigned int lhs_possible_lifetimes_size =
              lhs_lifetime.GetPossibleLifetimes().size();
          while (++i < lhs_possible_lifetimes_size) {
            if (lhs_lifetime.GetPossibleLifetime(i)->empty()) continue;
            Lifetime specific_lifetime(Lifetime::IdToChar(i));
            if (rhs_lifetime < specific_lifetime) {
              if (rhs_lifetime.IsNotSet()) {
                unsigned int possible_lifetimes_size =
                    rhs_lifetime.GetPossibleLifetimes().size();
                const auto &stmts = lhs_lifetime.GetStmts(i);
                for (unsigned int i = OFFSET; i < possible_lifetimes_size;
                     i++) {
                  if (!rhs_lifetime.ContainsShortestLifetime(i) ||
                      (char)i == specific_lifetime.GetId())
                    continue;
                  S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                      << specific_lifetime.GetLifetimeName()
                      << Lifetime::GetLifetimeName(i) << op->getSourceRange();
                  Factory.PrintNotes(rhs_lifetime, rhs_var_decl,
                                     diag::note_lifetime_declared_here, i);
                }
              } else {
                S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                    << specific_lifetime.GetLifetimeName()
                    << rhs_lifetime.GetLifetimeName() << op->getSourceRange();
                Factory.PrintNotes(rhs_lifetime, rhs_var_decl,
                                   diag::note_lifetime_declared_here);
              }
              Factory.PrintNotes(lhs_lifetime, lhs_var_decl,
                                 diag::note_lifetime_declared_here, i);
            }
          }
        }
      }
    }
  }
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
  unsigned int first_arg_size = first_lifetime.GetPossibleLifetimes().size();
  unsigned int second_arg_size = second_lifetime.GetPossibleLifetimes().size();
  if (first_lifetime.IsNotSet() && second_lifetime.IsNotSet()) {
    unsigned int max_size = std::max(first_arg_size, second_arg_size);
    for (unsigned int i = 0; i < max_size; i++) {
      if ((i >= second_arg_size ||
           !second_lifetime.ContainsShortestLifetime(i)) &&
          (i < first_arg_size && first_lifetime.ContainsShortestLifetime(i))) {
        PrintNotes(first_arg, first_lifetime, i, first_num_indirections);
      } else if ((i >= first_arg_size ||
                  !first_lifetime.ContainsShortestLifetime(i)) &&
                 (i < second_arg_size &&
                  second_lifetime.ContainsShortestLifetime(i))) {
        PrintNotes(second_arg, second_lifetime, i, second_num_indirections);
      }
    }
  } else if (first_lifetime.IsNotSet()) {
    for (unsigned int i = 0; i < first_arg_size; i++) {
      if ((char)i == second_lifetime.GetId() ||
          !first_lifetime.ContainsShortestLifetime(i))
        continue;
      PrintNotes(first_arg, first_lifetime, i, first_num_indirections);
    }
    PrintNotes(second_arg, second_lifetime, second_num_indirections);
  } else if (second_lifetime.IsNotSet()) {
    for (unsigned int i = 0; i < second_arg_size; i++) {
      if ((char)i == first_lifetime.GetId() ||
          !second_lifetime.ContainsShortestLifetime(i))
        continue;
      PrintNotes(second_arg, second_lifetime, i, second_num_indirections);
    }
    PrintNotes(first_arg, first_lifetime, first_num_indirections);
  } else {
    PrintNotes(second_arg, second_lifetime, second_num_indirections);
    PrintNotes(first_arg, first_lifetime, first_num_indirections);
  }
}

void LifetimesCheckerVisitor::CompareAndCheck(
    const clang::VarDecl *lhs_var_decl, clang::QualType lhs_type,
    const clang::Expr *expr, const clang::Expr *rhs, clang::QualType rhs_type,
    const clang::Stmt *stmt, const clang::BinaryOperator *op, bool is_return,
    PrintNotesFactory factory) const {
  if (expr == nullptr) return;
  if (const auto *rhs_decl_ref_expr =
          clang::dyn_cast<clang::DeclRefExpr>(expr)) {
    // there can only be one pointer/reference variable
    if (const auto *rhs_var_decl =
            dyn_cast<clang::VarDecl>(rhs_decl_ref_expr->getDecl())) {
      while (lhs_type->isPointerType() || lhs_type->isReferenceType()) {
        Lifetime &lhs_lifetime =
            is_return ? State.GetReturnLifetime(lhs_type)
                      : State.GetLifetime(lhs_var_decl, lhs_type);
        Lifetime &rhs_lifetime =
            State.GetLifetimeOrLocal(rhs_var_decl, rhs_type);

        if (is_return && rhs_lifetime.IsLocal()) {
          S.Diag(expr->getExprLoc(), diag::warn_cannot_return_local)
              << rhs_type.getCanonicalType() << expr->getSourceRange();
          const auto &maybe_stmts = rhs_lifetime.GetStmts(LOCAL);
          if (maybe_stmts.has_value()) {
            const auto &stmts = maybe_stmts.value();
            for (const auto &stmt : stmts) {
              S.Diag(stmt->getBeginLoc(), diag::note_lifetime_declared_here)
                  << Lifetime::GetLifetimeName(LOCAL) << stmt->getSourceRange();
            }
          } else {
            S.Diag(rhs_var_decl->getBeginLoc(), diag::note_lifetime_declared_here) << rhs_lifetime.GetLifetimeName() << rhs_var_decl->getSourceRange();
          }
        } else if (lhs_lifetime.IsSet() && rhs_lifetime < lhs_lifetime) {
          factory(lhs_var_decl, rhs_var_decl, op, expr, stmt, lhs_lifetime,
                  rhs_lifetime);
        }

        lhs_type = lhs_type->getPointeeType();
        rhs_type = rhs_type->getPointeeType();
      }
    }
  } else if (const auto *call_expr = clang::dyn_cast<clang::CallExpr>(expr)) {
    auto &call_info = PointsTo.GetCallExprInfo(call_expr);
    while (lhs_type->isPointerType() || lhs_type->isReferenceType()) {
      Lifetime &lhs_lifetime = is_return
                                   ? State.GetReturnLifetime(lhs_type)
                                   : State.GetLifetime(lhs_var_decl, lhs_type);
      if (lhs_lifetime.IsNotSet()) {
        lhs_type = lhs_type->getPointeeType();
        continue;
      }
      auto &current_type_call_info = call_info[lhs_type];

      if (current_type_call_info.is_local) {
        if (is_return) {
          S.Diag(expr->getExprLoc(), diag::warn_cannot_return_local)
              << rhs_type.getCanonicalType() << expr->getSourceRange();
        } else {
          Lifetime arg_lifetime(LOCAL);
          if (Lifetime(LOCAL) < lhs_lifetime) {
            factory(lhs_var_decl, call_expr->getCalleeDecl(), op, expr, stmt,
                    lhs_lifetime, arg_lifetime);
          }
        }
      }

      for (const auto &[arg, arg_type] : current_type_call_info.info) {
        const auto &arg_points_to = PointsTo.GetExprPointsTo(arg);
        for (const auto &expr : arg_points_to) {
          if (expr == nullptr) continue;
          if (const auto *rhs_ref_decl =
                  clang::dyn_cast<clang::DeclRefExpr>(expr)) {
            if (const auto *rhs_var_decl = clang::dyn_cast<clang::VarDecl>(
                    rhs_ref_decl->getDecl()->getCanonicalDecl())) {
              Lifetime &arg_lifetime =
                  State.GetLifetimeOrLocal(rhs_var_decl, arg_type);
              if (arg_lifetime < lhs_lifetime) {
                factory(lhs_var_decl, rhs_var_decl, op, expr, stmt,
                        lhs_lifetime, arg_lifetime);
              }
            }
          }
        }
      }
      lhs_type = lhs_type->getPointeeType();
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

  clang::QualType base_type = lhs->getType().getCanonicalType();
  clang::QualType lhs_type = PointsTo.GetExprType(lhs);
  lhs_type = lhs_type.isNull() ? base_type : lhs_type;

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes.
  // We don't want to change points-to sets in those cases.

  if (!lhs_type->isPointerType() && !lhs_type->isReferenceType()) {
    // debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);

  const auto &rhs = op->getRHS()->IgnoreParens();
  const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);

  if (const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs)) {
    VerifyBinAssign(lhs_type, base_type, rhs, lhs_decl_ref_expr, rhs_points_to,
                    op, Factory.BinAssignFactory());
  } else if (clang::isa<clang::UnaryOperator>(lhs)) {
    VerifyMaxLifetimes(lhs, rhs, op, lhs_points_to, rhs_points_to);
  }

  for (const auto &expr : lhs_points_to) {
    if (expr == nullptr) continue;

    VerifyBinAssign(lhs_type, base_type, rhs, expr, rhs_points_to, op,
                    Factory.BinAssignFactory());

    if (clang::isa<clang::UnaryOperator>(expr)) {
      VerifyMaxLifetimes(expr, rhs, op, lhs_points_to, rhs_points_to);
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
  if (debugEnabled) debugLifetimes("[VisitCallExpr]");

  const clang::FunctionDecl *direct_callee = call->getDirectCallee();
  if (direct_callee) {
    auto it = FuncInfo.find(direct_callee);
    assert(it != FuncInfo.end());

    auto &func_info = it->second;
    const auto &params_info_vec = func_info.GetParamsInfo();

    unsigned int num_args = call->getNumArgs();

    llvm::SmallVector<ParamInfo> params_set;
    params_set.resize(num_args);

    unsigned int num_indirections = params_info_vec.size();

    while (num_indirections-- > 0) {
      llvm::SmallVector<llvm::SmallVector<ParamInfo>> param_lifetimes;

      // get lifetimes for the current indirection level
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
      for (const auto &params : param_lifetimes) {
        if (params.size() < 2) continue;

        // ! param_lifetimes should have ParamInfo sorted
        auto it = params.begin();
        const ParamInfo *first_param_info = it;
        const auto *first_arg_expr = call->getArg(first_param_info->index);
        const auto *first_arg = GetDeclFromArg(first_arg_expr);
        if (first_arg == nullptr) continue;

        clang::QualType first_arg_type = PointsTo.GetExprType(first_arg_expr);
        first_arg_type =
            first_arg_type.isNull() ? first_param_info->type : first_arg_type;

        Lifetime &first_arg_lifetime =
            State.GetLifetime(first_arg, first_arg_type);

        while (++it != params.end()) {
          if (it->index != first_param_info->index) {
            const auto *current_arg_expr = call->getArg(it->index);
            const auto *current_arg = GetDeclFromArg(current_arg_expr);
            if (current_arg != nullptr) {
              clang::QualType current_arg_type =
                  PointsTo.GetExprType(current_arg_expr);
              current_arg_type =
                  current_arg_type.isNull() ? it->type : current_arg_type;
              Lifetime &current_arg_lifetime =
                  State.GetLifetime(current_arg, current_arg_type);
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
      if (!param_lifetimes.empty()) {
        for (const auto &param_info : params_info_vec[num_indirections]) {
          Lifetime &current_lifetime =
              func_info.GetParamLifetime(param_info.param, param_info.type);
          char current_id = current_lifetime.GetId();

          if (param_lifetimes.size() <= (unsigned int)current_id) {
            continue;
          }

          const auto &filtered_params = param_lifetimes[current_id];
          const auto *current_arg_expr = call->getArg(param_info.index);
          const auto *current_arg = GetDeclFromArg(current_arg_expr);
          if (current_arg == nullptr) continue;
          clang::QualType arg_type = PointsTo.GetExprType(current_arg_expr);
          arg_type = arg_type.isNull()
                         ? current_arg->getType().getCanonicalType()
                         : arg_type;

          Lifetime &current_arg_lifetime =
              State.GetLifetimeOrLocal(current_arg, arg_type);
          for (const auto &other_param_info : filtered_params) {
            const auto *arg = call->getArg(other_param_info.index);
            const auto *arg_decl = GetDeclFromArg(arg);
            if (arg_decl == nullptr) continue;
            Lifetime &arg_lifetime =
                State.GetLifetimeOrLocal(arg_decl, arg_type);
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
      for (const auto &param_info : params_info_vec[num_indirections]) {
        assert(param_info.index < params_set.size());
        params_set[param_info.index] = param_info;
      }

      // check static params
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
              unsigned int arg_possible_lifetimes_size =
                  arg_lifetime.GetPossibleLifetimes().size();
              for (unsigned int i = 0; i < arg_possible_lifetimes_size; i++) {
                if (!arg_lifetime.ContainsShortestLifetime(i)) continue;
                S.Diag(arg->getExprLoc(), diag::warn_arg_lifetimes_differ)
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
              S.Diag(arg->getExprLoc(), diag::warn_arg_lifetimes_differ)
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
      if (init_points_to.empty() && !clang::isa<clang::DeclRefExpr>(init)) {
        debugWarn("Initializer is not in PointsToMap");
        return std::nullopt;
      }

      clang::QualType init_type = PointsTo.GetExprType(init);
      init_type = init_type.isNull() ? var_decl_type : init_type;
      CompareAndCheck(var_decl, var_decl_type, init, init, init_type, nullptr,
                      nullptr, false, Factory.DeclStmtFactory());

      for (const auto &expr : init_points_to) {
        CompareAndCheck(var_decl, var_decl_type, expr, init, init_type, nullptr,
                        nullptr, false, Factory.DeclStmtFactory());
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

  if (!return_type->isPointerType() && !return_type->isReferenceType()) {
    return std::nullopt;
  }

  const auto &return_value = return_stmt->getRetValue()->IgnoreParens();
  clang::QualType return_value_type = PointsTo.GetExprType(return_value);
  return_value_type =
      return_value_type.isNull() ? return_type : return_value_type;

  // TODO remove this
  if (PointsTo.IsEmpty(return_value) &&
      !clang::isa<clang::DeclRefExpr>(return_value)) {
    debugWarn("Return expr is not in PointsToMap");
    Visit(const_cast<clang::Expr *>(return_value));
  }

  CompareAndCheck(nullptr, return_type, return_value, return_value,
                  return_value_type, return_stmt, nullptr, true,
                  Factory.ReturnStmtFactory());

  const auto &return_expr = PointsTo.GetExprPointsTo(return_value);
  for (const auto &expr : return_expr) {
    CompareAndCheck(nullptr, return_type, expr, return_value, return_value_type,
                    return_stmt, nullptr, true, Factory.ReturnStmtFactory());
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

std::optional<std::string> LifetimesCheckerVisitor::VisitUnaryOperator(
    const clang::UnaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitUnaryAddrOf]");
  for (const auto &child : op->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
