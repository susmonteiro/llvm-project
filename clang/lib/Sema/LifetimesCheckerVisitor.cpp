#include "clang/Sema/LifetimesCheckerVisitor.h"

#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"

namespace clang {

std::string GenerateArgName(const clang::VarDecl *arg, int num_indirections) {
  if (num_indirections < 0)
    return '\'' + std::string(-num_indirections, '&') + arg->getNameAsString() +
           '\'';
  return '\'' + std::string(num_indirections, '*') + arg->getNameAsString() +
         '\'';
}

std::string GenerateLifetimeName(Lifetime &lifetime) {
  return std::string(lifetime.GetNumIndirections(), '*') +
         lifetime.GetLifetimeName();
}

std::string GenerateLifetimeName(char id, unsigned int num_indirections) {
  return std::string(num_indirections, '*') + Lifetime::GetLifetimeName(id);
}

int LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                               const clang::Decl *var_decl,
                                               int msg, int notes) const {
  char id = lifetime.GetId();
  return PrintNotes(lifetime, var_decl, msg, id, notes);
}

int LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                               clang::SourceLocation loc,
                                               clang::SourceRange range,
                                               int msg, int notes) const {
  char id = lifetime.GetId();
  return PrintNotes(lifetime, loc, range, msg, id, notes);
}

int LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                               const clang::Decl *var_decl,
                                               int msg, char id,
                                               int notes) const {
  return PrintNotes(lifetime, var_decl->getLocation(),
                    var_decl->getSourceRange(), msg, id, notes);
}

int LifetimesCheckerVisitorFactory::PrintNotes(Lifetime &lifetime,
                                               clang::SourceLocation loc,
                                               clang::SourceRange range,
                                               int msg, char id,
                                               int notes) const {
  unsigned int num_indirections = lifetime.GetNumIndirections();
  const auto &maybe_stmts = lifetime.GetStmts(id);
  if (maybe_stmts.has_value()) {
    const auto &stmts = maybe_stmts.value();
    for (const auto &stmt : stmts) {
      S.Diag(stmt->getBeginLoc(), msg)
          << GenerateLifetimeName(id, num_indirections)
          << stmt->getSourceRange();
      if (--notes < 0) return notes;
    }
  } else {
    S.Diag(loc, msg) << GenerateLifetimeName(lifetime) << range;
    notes--;
  }
  return notes;
}

PrintNotesFactory LifetimesCheckerVisitorFactory::BinAssignFactory() const {
  return [this](const clang::VarDecl *lhs_var_decl,
                const clang::Decl *rhs_var_decl,
                const clang::BinaryOperator *op, const clang::Expr *expr,
                const clang::Stmt *stmt, clang::Lifetime &lhs_lifetime,
                Lifetime &rhs_lifetime) {
    assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr && op != nullptr);
    assert(lhs_lifetime.IsSet() && "BinAssignFactory");
    debugInfo("BinAssignFactory");
    if (rhs_lifetime.IsNotSet()) {
      unsigned int possible_lifetimes_size =
          rhs_lifetime.GetDependencies().size();
      unsigned int num_indirections = rhs_lifetime.GetNumIndirections();
      for (unsigned int i = OFFSET; i < possible_lifetimes_size; i++) {
        if (!rhs_lifetime.ContainsShortestLifetime(i) ||
            (char)i == lhs_lifetime.GetId())
          continue;
        S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
            << GenerateLifetimeName(lhs_lifetime)
            << GenerateLifetimeName(i, num_indirections)
            << op->getSourceRange();
        PrintNotes(rhs_lifetime, rhs_var_decl,
                   diag::note_lifetime_declared_here, i, MAX_NUM_NOTES);
      }
      PrintNotes(lhs_lifetime, lhs_var_decl, diag::note_lifetime_declared_here,
                 MAX_NUM_NOTES);
    } else {
      S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
          << GenerateLifetimeName(lhs_lifetime)
          << GenerateLifetimeName(rhs_lifetime) << op->getSourceRange();
      PrintNotes(rhs_lifetime, rhs_var_decl, diag::note_lifetime_declared_here,
                 MAX_NUM_NOTES / 2);
      PrintNotes(lhs_lifetime, lhs_var_decl, diag::note_lifetime_declared_here,
                 MAX_NUM_NOTES / 2);
    }
  };
}

PrintNotesFactory LifetimesCheckerVisitorFactory::DeclStmtFactory() const {
  return
      [this](const clang::VarDecl *lhs_var_decl,
             const clang::Decl *rhs_var_decl, const clang::BinaryOperator *op,
             const clang::Expr *expr, const clang::Stmt *stmt,
             Lifetime &lhs_lifetime, Lifetime &rhs_lifetime) {
        assert(lhs_var_decl != nullptr && rhs_var_decl != nullptr);
        assert(lhs_lifetime.IsSet() && "DeclStmtFactory");
        debugInfo("DeclStmtFactory");

        if (rhs_lifetime.IsNotSet()) {
          unsigned int init_possible_lifetimes_size =
              rhs_lifetime.GetDependencies().size();
          unsigned int num_indirections = rhs_lifetime.GetNumIndirections();
          for (unsigned int i = 0; i < init_possible_lifetimes_size; i++) {
            if (!rhs_lifetime.ContainsShortestLifetime(i) ||
                (char)i == lhs_lifetime.GetId())
              continue;
            S.Diag(lhs_var_decl->getInit()->getExprLoc(),
                   diag::warn_decl_lifetimes_differ)
                << GenerateLifetimeName(lhs_lifetime)
                << GenerateLifetimeName(i, num_indirections)
                << lhs_var_decl->getInit()->getSourceRange();
            PrintNotes(rhs_lifetime, rhs_var_decl,
                       diag::note_lifetime_declared_here, i, MAX_NUM_NOTES);
          }
        } else {
          S.Diag(lhs_var_decl->getInitializingDeclaration()->getLocation(),
                 diag::warn_decl_lifetimes_differ)
              << GenerateLifetimeName(lhs_lifetime)
              << GenerateLifetimeName(rhs_lifetime)
              << lhs_var_decl->getInitializingDeclaration()->getSourceRange();
          PrintNotes(rhs_lifetime, rhs_var_decl,
                     diag::note_lifetime_declared_here, MAX_NUM_NOTES);
        }
      };
}

PrintNotesFactory LifetimesCheckerVisitorFactory::ReturnStmtFactory() const {
  return [this](const clang::VarDecl *_lhs_var_decl,
                const clang::Decl *var_decl, const clang::BinaryOperator *_op,
                const clang::Expr *expr, const clang::Stmt *return_stmt,
                Lifetime &return_lifetime, Lifetime &var_lifetime) {
    debugInfo("ReturnStmtFactory");
    assert(var_decl != nullptr && expr != nullptr && return_stmt != nullptr);
    assert(return_lifetime.IsSet());
    if (var_lifetime.IsNotSet()) {
      unsigned int var_possible_lifetimes_size =
          var_lifetime.GetDependencies().size();
      unsigned int num_indirections = var_lifetime.GetNumIndirections();
      for (unsigned int i = 0; i < var_possible_lifetimes_size; i++) {
        if (!var_lifetime.ContainsShortestLifetime(i) ||
            (char)i == return_lifetime.GetId())
          continue;
        S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
            << GenerateLifetimeName(return_lifetime)
            << GenerateLifetimeName(i, num_indirections)
            << return_stmt->getSourceRange();
        PrintNotes(var_lifetime, var_decl, diag::note_lifetime_declared_here,
                   (char)i, MAX_NUM_NOTES);
      }
    } else {
      S.Diag(expr->getExprLoc(), diag::warn_return_lifetimes_differ)
          << GenerateLifetimeName(return_lifetime)
          << GenerateLifetimeName(var_lifetime)
          << return_stmt->getSourceRange();
      PrintNotes(var_lifetime, var_decl, diag::note_lifetime_declared_here,
                 MAX_NUM_NOTES);
    }
  };
}

const clang::VarDecl *LifetimesCheckerVisitor::GetDeclFromArg(
    const clang::Expr *arg) const {
  const auto &arg_points_to = PointsTo.GetExprDecls(arg->IgnoreParens());
  assert(arg_points_to.size() <= 1U);
  for (const auto &decl : arg_points_to) {
    return decl;
  }
  // debugWarn("Arg is not a declrefexpr or memberexpr");
  return nullptr;
}

void LifetimesCheckerVisitor::VerifyBinAssign(
    clang::Expr *lhs, const clang::Expr *rhs, const clang::Expr *expr,
    const clang::BinaryOperator *op,
    const llvm::SmallSet<const clang::Expr *, 2U> &lhs_points_to) const {
  if (expr == nullptr) return;

  // expr refers to lhs
  if (clang::isa<clang::MemberExpr>(expr)) {
    // debugInfo("<Member Expr>");
    auto &points_to = PointsTo.GetExprDecls(expr);
    // debugLifetimes("Points to", points_to.size());
    // TODO delete this
    assert(points_to.size() == 1 && "Handle multiple points to in MemberExpr");
    const auto *lhs_var_decl = *points_to.begin();

    unsigned int lhs_num_indirections = 1;
    unsigned int rhs_num_indirections =
        Lifetime::GetNumIndirections(rhs->getType());

    CompareAndCheck(lhs_var_decl, lhs_num_indirections, rhs, rhs,
                    rhs_num_indirections, nullptr, op, false,
                    Factory.BinAssignFactory());

    const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
    for (const auto &expr : rhs_points_to) {
      if (expr != nullptr) {
        CompareAndCheck(lhs_var_decl, lhs_num_indirections, expr, rhs,
                        rhs_num_indirections, nullptr, op, false,
                        Factory.BinAssignFactory());
      }
    }

  } else if (const auto *unary_op =
                 clang::dyn_cast<clang::UnaryOperator>(expr)) {
    VerifyMaxLifetimes(unary_op, rhs, op, lhs_points_to);
  } else if (const auto *lhs_decl_ref_expr =
                 clang::dyn_cast<clang::DeclRefExpr>(expr)) {
    unsigned int lhs_num_indirections =
        Lifetime::GetNumIndirections(lhs->getType());
    unsigned int rhs_num_indirections =
        Lifetime::GetNumIndirections(rhs->getType());

    if (const auto *lhs_var_decl =
            dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
      CompareAndCheck(lhs_var_decl, lhs_num_indirections, rhs, rhs,
                      rhs_num_indirections, nullptr, op, false,
                      Factory.BinAssignFactory());

      const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
      for (const auto &expr : rhs_points_to) {
        if (expr != nullptr) {
          CompareAndCheck(lhs_var_decl, lhs_num_indirections, expr, rhs,
                          rhs_num_indirections, nullptr, op, false,
                          Factory.BinAssignFactory());
        }
      }
    }
  }
}

void LifetimesCheckerVisitor::VerifyMaxLifetimes(
    const clang::UnaryOperator *deref_op, const clang::Expr *rhs,
    const clang::BinaryOperator *op,
    const llvm::SmallSet<const clang::Expr *, 2U> &lhs_points_to) const {
  const auto &rhs_points_to = PointsTo.GetExprPointsTo(rhs);
  if (deref_op->getOpcode() != clang::UO_Deref ||
      op->getType().getCanonicalType() !=
          deref_op->getType().getCanonicalType())
    return;

  for (const auto &expr : lhs_points_to) {
    if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) continue;
    const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(expr);
    if (const auto *lhs_var_decl =
            dyn_cast<clang::VarDecl>(lhs_decl_ref_expr->getDecl())) {
      clang::QualType lhs_type = PointsTo.GetExprType(deref_op);
      lhs_type =
          lhs_type.isNull() ? deref_op->getType().getCanonicalType() : lhs_type;
      Lifetime &lhs_lifetime = State.GetLifetime(lhs_var_decl, lhs_type);
      if (lhs_lifetime.IsDead()) continue;

      for (const auto &expr : rhs_points_to) {
        if (expr == nullptr || !clang::isa<clang::DeclRefExpr>(expr)) continue;
        const auto *rhs_decl_ref_expr =
            clang::dyn_cast<clang::DeclRefExpr>(expr);
        if (const auto *rhs_var_decl =
                dyn_cast<clang::VarDecl>(rhs_decl_ref_expr->getDecl())) {
          if (!lhs_type->isPointerType() && !lhs_type->isReferenceType())
            continue;
          clang::QualType rhs_type = PointsTo.GetExprType(rhs);
          rhs_type = rhs_type.isNull() ? deref_op->getType().getCanonicalType()
                                       : rhs_type;
          char id = PointsTo.GetExprLifetime(rhs_decl_ref_expr);
          Lifetime rhs_lifetime =
              id != NOTSET ? Lifetime(id, rhs_type)
                           : State.GetLifetimeOrLocal(rhs_var_decl, rhs_type);
          if (rhs_lifetime.IsDead()) continue;
          unsigned int i = LOCAL - 1;
          unsigned int lhs_possible_lifetimes_size =
              lhs_lifetime.GetDependencies().size();
          while (++i < lhs_possible_lifetimes_size) {
            if (lhs_lifetime.GetPossibleLifetime(i).empty()) continue;
            Lifetime specific_lifetime(Lifetime::IdToChar(i));
            unsigned int lhs_num_indirections =
                lhs_lifetime.GetNumIndirections();
            if (rhs_lifetime < specific_lifetime) {
              if (rhs_lifetime.IsNotSet()) {
                unsigned int possible_lifetimes_size =
                    rhs_lifetime.GetDependencies().size();
                const auto &stmts = lhs_lifetime.GetStmts(i);
                unsigned int rhs_num_indirections =
                    rhs_lifetime.GetNumIndirections();
                for (unsigned int j = OFFSET; j < possible_lifetimes_size;
                     j++) {
                  if (!rhs_lifetime.ContainsShortestLifetime(j) ||
                      (char)j == specific_lifetime.GetId())
                    continue;
                  S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                      << GenerateLifetimeName(i, lhs_num_indirections)
                      << GenerateLifetimeName(j, rhs_num_indirections)
                      << op->getSourceRange();
                  Factory.PrintNotes(rhs_lifetime, rhs_var_decl,
                                     diag::note_lifetime_declared_here, j,
                                     MAX_NUM_NOTES);
                }
              } else {
                S.Diag(op->getExprLoc(), diag::warn_assign_lifetimes_differ)
                    << GenerateLifetimeName(i, lhs_num_indirections)
                    << GenerateLifetimeName(rhs_lifetime)
                    << op->getSourceRange();
                Factory.PrintNotes(rhs_lifetime, rhs_var_decl,
                                   diag::note_lifetime_declared_here,
                                   MAX_NUM_NOTES / 2);
              }
              Factory.PrintNotes(lhs_lifetime, lhs_var_decl,
                                 diag::note_lifetime_declared_here, i,
                                 MAX_NUM_NOTES / 2);
            }
          }
        }
      }
    }
  }
}

void LifetimesCheckerVisitor::ParamsCallExprChecker(
    const clang::CallExpr *call, const clang::FunctionDecl *direct_callee,
    const clang::VarDecl *first_arg, const clang::VarDecl *second_arg,
    Lifetime &first_lifetime, Lifetime &second_lifetime,
    int first_num_indirections, int second_num_indirections,
    unsigned int num_indirections, int msg) const {
  S.Diag(call->getExprLoc(), msg)
      << direct_callee << GenerateArgName(first_arg, first_num_indirections)
      << GenerateArgName(second_arg, second_num_indirections)
      << call->getSourceRange();

  unsigned int first_arg_size = first_lifetime.GetDependencies().size();
  unsigned int second_arg_size = second_lifetime.GetDependencies().size();
  if (first_lifetime.IsNotSet() && second_lifetime.IsNotSet()) {
    unsigned int max_size = std::max(first_arg_size, second_arg_size);
    int num_notes = MAX_NUM_NOTES;

    for (unsigned int i = 0; i < max_size; i++) {
      if ((i >= second_arg_size ||
           !second_lifetime.ContainsShortestLifetime(i)) &&
          (i < first_arg_size && first_lifetime.ContainsShortestLifetime(i))) {
        num_notes = GenerateNotes(first_arg, first_lifetime, i,
                                  num_indirections, num_notes);
      } else if ((i >= first_arg_size ||
                  !first_lifetime.ContainsShortestLifetime(i)) &&
                 (i < second_arg_size &&
                  second_lifetime.ContainsShortestLifetime(i))) {
        num_notes = GenerateNotes(second_arg, second_lifetime, i,
                                  num_indirections, num_notes);
      }
      if (num_notes < 0) return;
    }
  } else if (first_lifetime.IsNotSet()) {
    int num_notes = MAX_NUM_NOTES;
    for (unsigned int i = 0; i < first_arg_size; i++) {
      if ((char)i == second_lifetime.GetId() ||
          !first_lifetime.ContainsShortestLifetime(i))
        continue;
      num_notes = GenerateNotes(first_arg, first_lifetime, i, num_indirections,
                                num_notes);
      if (num_notes < 0) return;
    }
    GenerateNotes(second_arg, second_lifetime, num_indirections,
                  MAX_NUM_NOTES / 2);
  } else if (second_lifetime.IsNotSet()) {
    GenerateNotes(first_arg, first_lifetime, num_indirections,
                  MAX_NUM_NOTES / 2);
    int num_notes = MAX_NUM_NOTES;
    for (unsigned int i = 0; i < second_arg_size; i++) {
      if ((char)i == first_lifetime.GetId() ||
          !second_lifetime.ContainsShortestLifetime(i))
        continue;
      num_notes = GenerateNotes(second_arg, second_lifetime, i,
                                num_indirections, num_notes);
      if (num_notes < 0) return;
    }
  } else {
    GenerateNotes(first_arg, first_lifetime, num_indirections,
                  MAX_NUM_NOTES / 2);
    GenerateNotes(second_arg, second_lifetime, num_indirections,
                  MAX_NUM_NOTES / 2);
  }
}

void LifetimesCheckerVisitor::CallExprChecker(
    const clang::VarDecl *lhs_var_decl, unsigned int lhs_num_indirections,
    const clang::Expr *expr, unsigned int rhs_num_indirections,
    const clang::Stmt *stmt, const clang::BinaryOperator *op, bool is_return,
    clang::TypeToSet &call_info, PrintNotesFactory factory) const {
  debugInfo("CallExprChecker");
  while (lhs_num_indirections > 0) {
    Lifetime &lhs_lifetime =
        is_return ? State.GetReturnLifetimeOrLocal(lhs_num_indirections)
                  : State.GetLifetime(lhs_var_decl, lhs_num_indirections);
    debugLifetimes("LHS lifetime", lhs_lifetime.DebugString());
    if (lhs_lifetime.IsDead()) {
      lhs_num_indirections--;
      continue;
    }

    auto &current_type_call_info = call_info[lhs_num_indirections];
    if (current_type_call_info.is_local) {
      // debugLight("FuncCall is local");
      if (is_return) {
        S.Diag(expr->getExprLoc(), diag::warn_cannot_return_local)
            << std::string(rhs_num_indirections, '*') << expr->getSourceRange();
        const auto *func_decl =
            current_type_call_info.call_expr->getDirectCallee();
        S.Diag(func_decl->getLocation(), diag::note_return_value_not_annotated)
            << func_decl->getSourceRange();
      } else {
        Lifetime arg_lifetime(LOCAL);
        if (lhs_lifetime.IsSet() && arg_lifetime < lhs_lifetime) {
          factory(lhs_var_decl,
                  current_type_call_info.call_expr->getCalleeDecl(), op, expr,
                  stmt, lhs_lifetime, arg_lifetime);
        }
      }
    }

    if (lhs_lifetime.IsSet()) {
      for (const auto &[arg, arg_num_indiretions] :
           current_type_call_info.info) {
        const auto &arg_points_to = PointsTo.GetExprDecls(arg);
        for (const auto &decl : arg_points_to) {
          char id = PointsTo.GetExprLifetime(arg);
          // debugLifetimes("DECL", decl->getNameAsString());
          // debugLifetimes("ARG ID", id);
          // debugLifetimes("ARG NUM INDIRECTIONS", arg_num_indiretions);
          Lifetime arg_lifetime =
              id != NOTSET && arg_num_indiretions ==
                                  Lifetime::GetNumIndirections(arg->getType())
                  ? Lifetime(id, arg_num_indiretions)
                  : State.GetLifetimeOrLocal(decl, arg_num_indiretions);
          // debugLifetimes("ARG LIFETIME", arg_lifetime.DebugString());
          if (arg_lifetime < lhs_lifetime && !arg_lifetime.IsDead()) {
            factory(lhs_var_decl, decl, op, expr, stmt, lhs_lifetime,
                    arg_lifetime);
          }
        }
      }
    }

    lhs_num_indirections--;
  }
}

void LifetimesCheckerVisitor::DeclChecker(
    const clang::VarDecl *lhs_var_decl, unsigned int lhs_num_indirections,
    const clang::Expr *expr, const clang::VarDecl *rhs_var_decl,
    unsigned int rhs_num_indirections, const clang::Stmt *stmt,
    const clang::BinaryOperator *op, bool is_return,
    PrintNotesFactory factory) const {
  debugInfo("DeclChecker");
  char id = PointsTo.GetExprLifetime(expr);
  if (id != NOTSET) {
    Lifetime &lhs_lifetime =
        is_return ? State.GetReturnLifetimeOrLocal(lhs_num_indirections)
                  : State.GetLifetime(lhs_var_decl, lhs_num_indirections);
    Lifetime rhs_lifetime = Lifetime(id, rhs_num_indirections);
    if (is_return && rhs_lifetime.IsLocal()) {
      S.Diag(expr->getExprLoc(), diag::warn_cannot_return_local)
          << std::string(rhs_lifetime.GetNumIndirections(), '*')
          << expr->getSourceRange();
      const auto &maybe_stmts = rhs_lifetime.GetStmts(LOCAL);
      if (maybe_stmts.has_value()) {
        const auto &stmts = maybe_stmts.value();
        int num_notes = MAX_NUM_NOTES;
        for (const auto &stmt : stmts) {
          S.Diag(stmt->getBeginLoc(), diag::note_lifetime_declared_here)
              << GenerateLifetimeName(LOCAL, rhs_lifetime.GetNumIndirections())
              << stmt->getSourceRange();
          if (--num_notes < 0) break;
        }
      } else {
        S.Diag(rhs_var_decl->getBeginLoc(), diag::note_lifetime_declared_here)
            << GenerateLifetimeName(rhs_lifetime)
            << rhs_var_decl->getSourceRange();
      }
    } else if (lhs_lifetime.IsSet() && rhs_lifetime < lhs_lifetime) {
      factory(lhs_var_decl, rhs_var_decl, op, expr, stmt, lhs_lifetime,
              rhs_lifetime);
    }
    lhs_num_indirections--;
    rhs_num_indirections--;
  }

  while (lhs_num_indirections > 0 && rhs_num_indirections > 0) {
    Lifetime &lhs_lifetime =
        is_return ? State.GetReturnLifetimeOrLocal(lhs_num_indirections)
                  : State.GetLifetime(lhs_var_decl, lhs_num_indirections);
    Lifetime rhs_lifetime =
        State.GetLifetimeOrLocal(rhs_var_decl, rhs_num_indirections);
    if (is_return && rhs_lifetime.IsLocal()) {
      S.Diag(expr->getExprLoc(), diag::warn_cannot_return_local)
          << std::string(rhs_lifetime.GetNumIndirections(), '*')
          << expr->getSourceRange();
      const auto &maybe_stmts = rhs_lifetime.GetStmts(LOCAL);
      if (maybe_stmts.has_value()) {
        const auto &stmts = maybe_stmts.value();
        int num_notes = MAX_NUM_NOTES;
        for (const auto &stmt : stmts) {
          S.Diag(stmt->getBeginLoc(), diag::note_lifetime_declared_here)
              << GenerateLifetimeName(LOCAL, rhs_lifetime.GetNumIndirections())
              << stmt->getSourceRange();
          if (--num_notes < 0) break;
        }
      } else {
        S.Diag(rhs_var_decl->getBeginLoc(), diag::note_lifetime_declared_here)
            << GenerateLifetimeName(rhs_lifetime)
            << rhs_var_decl->getSourceRange();
      }
    } else if (lhs_lifetime.IsSet() && rhs_lifetime < lhs_lifetime) {
      factory(lhs_var_decl, rhs_var_decl, op, expr, stmt, lhs_lifetime,
              rhs_lifetime);
    }
    lhs_num_indirections--;
    rhs_num_indirections--;
  }
}

void LifetimesCheckerVisitor::CompareAndCheck(
    const clang::VarDecl *lhs_var_decl, unsigned int lhs_num_indirections,
    const clang::Expr *expr, const clang::Expr *rhs,
    unsigned int rhs_num_indirections, const clang::Stmt *stmt,
    const clang::BinaryOperator *op, bool is_return,
    PrintNotesFactory factory) const {
  debugInfo("CompareAndCheck");
  // debugLifetimes("LHS num_indirections", lhs_num_indirections);
  // debugLifetimes("RHS num_indirections", rhs_num_indirections);
  if (expr == nullptr) return;
  if (const auto *call_expr = clang::dyn_cast<clang::CallExpr>(expr)) {
    auto &call_info = PointsTo.GetCallExprInfo(call_expr);
    CallExprChecker(lhs_var_decl, lhs_num_indirections, expr,
                    rhs_num_indirections, stmt, op, is_return, call_info,
                    factory);
  } else if (const auto *member_expr =
                 clang::dyn_cast<clang::MemberExpr>(expr)) {
    // debugInfo("<Member Expr>");
    rhs_num_indirections = 1;
    auto &member_expr_points_to = PointsTo.GetExprDecls(member_expr);
    if (member_expr_points_to.size() == 1) {
      for (const auto *decl : member_expr_points_to) {
        DeclChecker(lhs_var_decl, lhs_num_indirections, expr, decl,
                    rhs_num_indirections, stmt, op, is_return, factory);
      }
    } else if (PointsTo.HasCallExprInfo(member_expr)) {
      CallExprChecker(lhs_var_decl, lhs_num_indirections, expr,
                      rhs_num_indirections, stmt, op, is_return,
                      PointsTo.GetCallExprInfo(member_expr), factory);
    } else {
      // expr->dump();
      // member_expr->dump();
      // rhs->dump();
      // debugLifetimes("Size of member_expr_points_to",
      // member_expr_points_to.size()); for (const auto *decl :
      // member_expr_points_to) {
      //   debugLifetimes("RHS decl", decl->getNameAsString());
      // }
      // TODO uncomment assert
      assert(false && "Should not reach here");
    }
  } else if (const auto *rhs_decl_ref_expr =
                 clang::dyn_cast<clang::DeclRefExpr>(expr)) {
    // there can only be one pointer/reference variable
    if (const auto *rhs_var_decl =
            dyn_cast<clang::VarDecl>(rhs_decl_ref_expr->getDecl())) {
      DeclChecker(lhs_var_decl, lhs_num_indirections, expr, rhs_var_decl,
                  rhs_num_indirections, stmt, op, is_return, factory);
    }
  }
}

int LifetimesCheckerVisitor::GenerateNotes(const clang::VarDecl *var_decl,
                                           Lifetime &lifetime,
                                           unsigned int num_indirections,
                                           int notes) const {
  char id = lifetime.GetId();
  return GenerateNotes(var_decl, lifetime, id, num_indirections, notes);
}

int LifetimesCheckerVisitor::GenerateNotes(const clang::VarDecl *var_decl,
                                           Lifetime &lifetime, char id,
                                           unsigned int num_indirections,
                                           int notes) const {
  const auto &maybe_stmts = lifetime.GetStmts(id);
  if (maybe_stmts.has_value()) {
    const auto &stmts = maybe_stmts.value();
    for (const auto &stmt : stmts) {
      S.Diag(stmt->getBeginLoc(), diag::note_lifetime_of)
          << GenerateLifetimeName(id, num_indirections)
          << stmt->getSourceRange();
      if (--notes < 0) return notes;
    }
  } else {
    GenerateNotes(var_decl, id, num_indirections);
    notes--;
  }
  return notes;
}

void LifetimesCheckerVisitor::GenerateNotes(
    const clang::VarDecl *var_decl, char id,
    unsigned int num_indirections) const {
  S.Diag(var_decl->getLocation(), diag::note_lifetime_of)
      << GenerateLifetimeName(id, num_indirections)
      << var_decl->getSourceRange();
}

std::optional<std::string> LifetimesCheckerVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  if (debugEnabled) debugLifetimes("[VisitBinAssign]");
  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS()->IgnoreParens();
  clang::QualType lhs_type = lhs->getType().getCanonicalType();
  if (!lhs_type->isPointerType() && !lhs_type->isReferenceType()) {
    // debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  const auto &rhs = op->getRHS()->IgnoreParens();
  Visit(rhs);

  const auto &lhs_points_to = PointsTo.GetExprPointsTo(lhs);
  VerifyBinAssign(lhs, rhs, lhs, op, lhs_points_to);
  for (const auto &expr : lhs_points_to) {
    VerifyBinAssign(lhs, rhs, expr, op, lhs_points_to);
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
  if (debugEnabled) debugLifetimes("[VisitCallExpr]");

  const clang::FunctionDecl *direct_callee = call->getDirectCallee();
  if (direct_callee) {
    debugInfo(direct_callee->getNameAsString());
    auto it = FuncInfo.find(direct_callee);
    // TODO remove this assert
    assert(it != FuncInfo.end());

    auto &func_info = it->second;
    const auto &params_info_vec = func_info.GetParamsInfo();

    unsigned int num_args = func_info.GetNumParams();
    for (unsigned int idx = 0; idx < num_args; idx++) {
      Visit(const_cast<clang::Expr *>(call->getArg(idx)));
    }

    llvm::SmallVector<ParamInfo> params_set;
    params_set.resize(num_args);

    unsigned int num_indirections = params_info_vec.size();

    while (num_indirections-- > 1) {
      debugLifetimes("Num indirections", num_indirections);
      llvm::SmallVector<llvm::SmallVector<ParamInfo>> param_lifetimes;

      // get lifetimes for the current indirection level
      debugInfo("1) Params for current indirection level");
      for (const auto &param_info : params_set) {
        debugLifetimes("Inside first loop");
        if (param_info.param == nullptr) continue;
        // TODO delete this (3 lines)
        assert(!param_info.type.isNull());
        clang::QualType param_type = param_info.type->getPointeeType();
        params_set[param_info.ptr_idx].type = param_type;
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(param_info.param, num_indirections);
        debugLifetimes("Param", param_info.param->getNameAsString());
        debugLifetimes("Param lifetime", param_lifetime.DebugString());
        char lifetime_id = param_lifetime.GetId();
        if ((unsigned int)lifetime_id >= param_lifetimes.size()) {
          param_lifetimes.resize(lifetime_id + 1);
        }
        param_lifetimes[lifetime_id].emplace_back(param_info);
      }

      // check lifetimes of higher indirections
      debugInfo("2) Check params for HIGHER indirection level");
      debugLifetimes("Size of param_lifetimes", param_lifetimes.size());
      for (const auto &params : param_lifetimes) {
        debugLifetimes("Size of params", params.size());
        if (params.size() < 2) continue;

        // param_lifetimes have ParamInfo sorted
        auto it = params.begin();
        const ParamInfo *first_param_info = it;
        const auto *first_expr = call->getArg(first_param_info->glb_idx);
        const auto *first_decl = GetDeclFromArg(first_expr);
        if (first_decl == nullptr) continue;

        Lifetime &first_lifetime =
            State.GetLifetime(first_decl, num_indirections);
        if (first_lifetime.EmptyDependencies() && first_lifetime.IsNotSet())
          continue;
        if (first_lifetime.IsDead()) continue;

        int first_num_indirections =
            first_param_info->original_num_indirections - num_indirections +
            (Lifetime::GetNumIndirections(first_decl->getType()) -
             Lifetime::GetNumIndirections(first_expr->getType()));

        while (++it != params.end()) {
          if (it->ptr_idx != first_param_info->ptr_idx) {
            const auto *second_expr = call->getArg(it->glb_idx);
            const auto *second_decl = GetDeclFromArg(second_expr);
            if (second_decl != nullptr) {
              int second_num_indirections =
                  it->original_num_indirections - num_indirections +
                  (Lifetime::GetNumIndirections(second_decl->getType()) -
                   Lifetime::GetNumIndirections(second_expr->getType()));
              Lifetime &second_lifetime =
                  State.GetLifetime(second_decl, num_indirections);
              if (second_lifetime.EmptyDependencies() &&
                  second_lifetime.IsNotSet())
                continue;
              if (second_lifetime.IsDead()) continue;
              if (first_lifetime != second_lifetime) {
                if (first_param_info->type.isConstQualified() &&
                    it->type.isConstQualified()) {
                  continue;
                } else if (first_param_info->type.isConstQualified()) {
                  ParamsCallExprChecker(
                      call, direct_callee, first_decl, second_decl,
                      first_lifetime, second_lifetime, first_num_indirections,
                      second_num_indirections, num_indirections,
                      diag::warn_func_params_lifetimes_shorter);
                } else if (it->type.isConstQualified()) {
                  ParamsCallExprChecker(
                      call, direct_callee, second_decl, first_decl,
                      second_lifetime, first_lifetime, second_num_indirections,
                      first_num_indirections, num_indirections,
                      diag::warn_func_params_lifetimes_shorter);
                } else {
                  ParamsCallExprChecker(
                      call, direct_callee, first_decl, second_decl,
                      first_lifetime, second_lifetime, first_num_indirections,
                      second_num_indirections, num_indirections,
                      diag::warn_func_params_lifetimes_equal);
                }
                return std::nullopt;
              }
            }
          }
        }
      }

      // check lifetimes of current indirection level
      debugInfo("3) Check params for CURRENT indirection level");
      if (!param_lifetimes.empty()) {
        for (const auto &first_param_info : params_info_vec[num_indirections]) {
          Lifetime &param_lifetime = func_info.GetParamLifetime(
              first_param_info.param, num_indirections);
          char param_lifetime_id = param_lifetime.GetId();

          if (param_lifetimes.size() <= (unsigned int)param_lifetime_id) {
            continue;
          }

          const auto &filtered_params = param_lifetimes[param_lifetime_id];
          const auto *first_expr = call->getArg(first_param_info.glb_idx);
          const auto *first_decl = GetDeclFromArg(first_expr);
          if (first_decl == nullptr) continue;

          Lifetime &first_lifetime =
              State.GetLifetimeOrLocal(first_decl, num_indirections);
          if (first_lifetime.EmptyDependencies() && first_lifetime.IsNotSet())
            continue;

          int first_num_indirections =
              first_param_info.original_num_indirections - num_indirections +
              (Lifetime::GetNumIndirections(first_decl->getType()) -
               Lifetime::GetNumIndirections(first_expr->getType()));

          for (const auto &second_param_info : filtered_params) {
            // skip const params
            if (second_param_info.type.isConstQualified()) {
              continue;
            }

            const auto *second_expr = call->getArg(second_param_info.glb_idx);
            const auto *second_decl = GetDeclFromArg(second_expr);
            if (second_decl == nullptr) continue;
            Lifetime &second_lifetime =
                State.GetLifetimeOrLocal(second_decl, num_indirections);
            if (second_lifetime.EmptyDependencies() &&
                second_lifetime.IsNotSet())
              continue;
            if (first_lifetime < second_lifetime) {
              int second_num_indirections =
                  second_param_info.original_num_indirections -
                  num_indirections +
                  (Lifetime::GetNumIndirections(second_decl->getType()) -
                   Lifetime::GetNumIndirections(second_expr->getType()));
              ParamsCallExprChecker(call, direct_callee, first_decl,
                                    second_decl, first_lifetime,
                                    second_lifetime, first_num_indirections,
                                    second_num_indirections, num_indirections,
                                    diag::warn_func_params_lifetimes_shorter);
            }
          }
        }
      }

      // insert params for the next indirection level
      debugInfo("4) Insert params for NEXT indirection level");
      for (const auto &param_info : params_info_vec[num_indirections]) {
        assert(param_info.ptr_idx < params_set.size());
        params_set[param_info.ptr_idx] = param_info;
      }

      // check static params
      debugInfo("5) Check STATIC params");
      for (const auto &param_info : params_set) {
        debugLifetimes("Checking static params");
        if (param_info.param == nullptr) continue;
        // TODO delete this
        assert(!param_info.type.isNull());
        Lifetime &param_lifetime =
            func_info.GetParamLifetime(param_info.param, num_indirections);
        if (param_lifetime.IsStatic()) {
          const auto *arg_expr = call->getArg(param_info.glb_idx);
          const auto *arg_decl = GetDeclFromArg(arg_expr);
          if (arg_decl == nullptr) continue;
          Lifetime &arg_lifetime =
              State.GetLifetimeOrLocal(arg_decl, num_indirections);
          if (!arg_lifetime.IsStatic()) {
            if (arg_lifetime.IsNotSet()) {
              unsigned int arg_possible_lifetimes_size =
                  arg_lifetime.GetDependencies().size();
              for (unsigned int i = 0; i < arg_possible_lifetimes_size; i++) {
                if (!arg_lifetime.ContainsShortestLifetime(i)) continue;
                S.Diag(arg_expr->getExprLoc(), diag::warn_arg_lifetimes_differ)
                    << GenerateLifetimeName(STATIC, num_indirections)
                    << GenerateLifetimeName(i, num_indirections)
                    << call->getSourceRange();
                GenerateNotes(arg_decl, arg_lifetime, i, num_indirections,
                              MAX_NUM_NOTES);
              }
              GenerateNotes(param_info.param, STATIC, num_indirections);

            } else {
              S.Diag(arg_expr->getExprLoc(), diag::warn_arg_lifetimes_differ)
                  << GenerateLifetimeName(STATIC, num_indirections)
                  << GenerateLifetimeName(arg_lifetime)
                  << call->getSourceRange();
              GenerateNotes(arg_decl, arg_lifetime, num_indirections,
                            MAX_NUM_NOTES);
            }
            GenerateNotes(param_info.param, STATIC, num_indirections);
          }
        }
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  if (debugEnabled) debugLifetimes("[VisitDeclRefExpr]");

  auto *decl = decl_ref->getDecl();
  if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
    ObjectLifetimes &lifetimes = State.GetObjectLifetimes(var_decl);
    for (auto &lifetime : lifetimes.GetLifetimes()) {
      if (lifetime.IsDead()) {
        const auto &maybe_stmts = lifetime.GetStmts(DEAD);
        if (maybe_stmts.has_value()) {
          const auto &stmts = maybe_stmts.value();
          const auto *stmt = *stmts.begin();
          if (stmts.size() == 1) {
            if (const auto *call_expr =
                    clang::dyn_cast<clang::CallExpr>(stmt)) {
              if (PointsTo.HasCallExprPointsTo(call_expr, decl_ref)) {
                continue;
              }
            }
          }
          S.Diag(decl_ref->getExprLoc(), diag::warn_read_dead)
              << std::string(lifetime.GetNumIndirections(), '*')
              << decl_ref->getSourceRange();

          int num_notes = MAX_NUM_NOTES;
          for (const auto &stmt : stmts) {
            if (const auto *call_expr =
                    clang::dyn_cast<clang::CallExpr>(stmt)) {
              if (PointsTo.HasCallExprPointsTo(call_expr, decl_ref)) {
                continue;
              }
              const auto *call_decl = call_expr->getCalleeDecl();
              S.Diag(call_expr->getBeginLoc(),
                     diag::note_lifetime_declared_here)
                  << GenerateLifetimeName(DEAD, lifetime.GetNumIndirections())
                  << call_expr->getSourceRange();
              S.Diag(call_decl->getBeginLoc(), diag::note_param_lifetime_local)
                  << GenerateLifetimeName(LOCAL, lifetime.GetNumIndirections())
                  << call_decl->getSourceRange();
              if ((num_notes -= 2) < 0) return std::nullopt;
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
        continue;
      }
      clang::QualType var_decl_type = var_decl->getType().getCanonicalType();
      unsigned int var_num_indirections =
          Lifetime::GetNumIndirections(var_decl->getType().getCanonicalType());
      Lifetime &var_decl_lifetime =
          State.GetLifetime(var_decl, var_num_indirections);
      // no initializer, nothing to check
      if (!var_decl->hasInit() || var_decl_lifetime.IsNotSet())
        return std::nullopt;

      const auto *init = var_decl->getInit()->IgnoreParens();
      Visit(const_cast<clang::Expr *>(init));
      const auto &init_points_to = PointsTo.GetExprPointsTo(init);

      // TODO remove this
      if (init_points_to.empty() && !clang::isa<clang::DeclRefExpr>(init)) {
        debugWarn("Initializer is not in PointsToMap");
        return std::nullopt;
      }

      clang::QualType init_type = PointsTo.GetExprType(init);
      init_type = init_type.isNull() ? var_decl_type : init_type;
      CompareAndCheck(var_decl, var_num_indirections, init, init,
                      var_num_indirections, nullptr, nullptr, false,
                      Factory.DeclStmtFactory());

      for (const auto &expr : init_points_to) {
        CompareAndCheck(var_decl, var_num_indirections, expr, init,
                        var_num_indirections, nullptr, nullptr, false,
                        Factory.DeclStmtFactory());
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  if (debugEnabled) debugLifetimes("[VisitExpr]");
  for (const auto &child : expr->children()) {
    if (child == nullptr) continue;
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  if (debugEnabled) debugLifetimes("[VisitReturnStmt]");

  clang::QualType return_type = Func->getReturnType().IgnoreParens();
  unsigned int return_num_indirections =
      Lifetime::GetNumIndirections(return_type);

  if (!return_type->isPointerType() && !return_type->isReferenceType()) {
    return std::nullopt;
  }

  const auto &return_value = return_stmt->getRetValue()->IgnoreParens();
  // TODO remove this
  if (PointsTo.IsEmpty(return_value) &&
      !clang::isa<clang::DeclRefExpr>(return_value)) {
    debugWarn("Return expr is not in PointsToMap");
    Visit(const_cast<clang::Expr *>(return_value));
  }

  CompareAndCheck(nullptr, return_num_indirections, return_value, return_value,
                  return_num_indirections, return_stmt, nullptr, true,
                  Factory.ReturnStmtFactory());

  const auto &return_expr = PointsTo.GetExprPointsTo(return_value);
  for (const auto &expr : return_expr) {
    CompareAndCheck(nullptr, return_num_indirections, expr, return_value,
                    return_num_indirections, return_stmt, nullptr, true,
                    Factory.ReturnStmtFactory());
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

}  // namespace clang
