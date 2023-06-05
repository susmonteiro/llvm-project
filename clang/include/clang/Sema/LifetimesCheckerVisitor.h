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

class LifetimesCheckerVisitor
    : public clang::StmtVisitor<LifetimesCheckerVisitor,
                                std::optional<std::string>> {
 public:
  LifetimesCheckerVisitor(const clang::FunctionDecl *func,
                          LifetimeAnnotationsAnalysis &state, Sema &sema)
      : Func(func), State(state), PointsTo(state.GetPointsTo()), S(sema) {}

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

  void CompareAndCheckLifetimes(Lifetime &lhs_lifetime, Lifetime &rhs_lifetime,
                                const clang::VarDecl *lhs_var_decl,
                                const clang::ValueDecl *rhs_var_decl, int warn,
                                int note) const;

  void CompareAndCheckLifetimes(
      Lifetime &lhs_lifetime, Lifetime &rhs_lifetime,
      const clang::VarDecl *lhs_var_decl, clang::SourceLocation loc,
      clang::SourceRange range, int warn, int note) const;

  void PrintNotes(Lifetime &lifetime, const clang::NamedDecl *var_decl,
                  int msg) const;
  void PrintNotes(Lifetime &lifetime, clang::SourceLocation loc,
                  clang::SourceRange range, int msg) const;
  void PrintNotes(Lifetime &lifetime, const clang::NamedDecl *var_decl, int msg,
                  char id) const;
  void PrintNotes(Lifetime &lifetime, clang::SourceLocation Loc,
                  clang::SourceRange range, int msg, char id) const;

 private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalysis &State;
  PointsToMap &PointsTo;
  Sema &S;
  bool debugEnabled = true;  // TODO delete this
};

}  // namespace clang

#endif  // LIFETIMES_CHECKER_VISITOR_H_
