#ifndef LIFETIMES_CHECKER_VISITOR_H_
#define LIFETIMES_CHECKER_VISITOR_H_

#include <iostream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/Error.h"

namespace clang {

using PrintNotesFactory =
    std::function<void(const clang::VarDecl *, const clang::VarDecl *,
                       const clang::BinaryOperator *, const clang::Expr *,
                       const clang::Stmt *, Lifetime &, Lifetime &)>;

class LifetimesCheckerVisitorFactory {
 public:
  // TODO implement constructor
  LifetimesCheckerVisitorFactory(Sema &sema) : S(sema) {}

  void PrintNotes(Lifetime &lifetime, const clang::VarDecl *var_decl,
                  int msg) const;
  void PrintNotes(Lifetime &lifetime, clang::SourceLocation loc,
                  clang::SourceRange range, int msg) const;
  void PrintNotes(Lifetime &lifetime, const clang::VarDecl *var_decl, int msg,
                  char id) const;
  void PrintNotes(Lifetime &lifetime, clang::SourceLocation Loc,
                  clang::SourceRange range, int msg, char id) const;

  PrintNotesFactory BinAssignFactory() const;
  PrintNotesFactory DeclStmtFactory() const;
  PrintNotesFactory ReturnStmtFactory() const;

 private:
  Sema &S;
};

class LifetimesCheckerVisitor
    : public clang::StmtVisitor<LifetimesCheckerVisitor,
                                std::optional<std::string>> {
 public:
  LifetimesCheckerVisitor(const clang::FunctionDecl *func,
                          LifetimeAnnotationsAnalysis &state, Sema &sema)
      : Func(func),
        State(state),
        PointsTo(state.GetPointsTo()),
        S(sema),
        Factory(sema) {}

  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitReturnStmt(
      const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);
  std::optional<std::string> VisitUnaryAddrOf(const clang::UnaryOperator *op);
  std::optional<std::string> VisitUnaryDeref(const clang::UnaryOperator *op);
  // TODO delete this
  std::optional<std::string> VisitUnaryOperator(const clang::UnaryOperator *op);

  void VerifyBinAssign(
      clang::QualType lhs_type, const clang::Expr *rhs, const clang::Expr *expr,
      const llvm::SmallSet<const clang::Expr *, 2U> &rhs_points_to,
      const clang::BinaryOperator *op, PrintNotesFactory factory) const;

  void CompareAndCheck(
      const clang::VarDecl *lhs_var_decl, clang::QualType lhs_type,
      const clang::Expr *rhs, const clang::Stmt *stmt,
      const llvm::SmallSet<const clang::Expr *, 2U> &rhs_points_to,
      const clang::BinaryOperator *op, bool return_lifetime,
      PrintNotesFactory factory) const;

 private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalysis &State;
  PointsToMap &PointsTo;
  Sema &S;
  LifetimesCheckerVisitorFactory Factory;
  bool debugEnabled = true;  // TODO delete this
};

}  // namespace clang

#endif  // LIFETIMES_CHECKER_VISITOR_H_
