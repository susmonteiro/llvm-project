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
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitReturnStmt(
      const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

 private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalysis &State;
  PointsToMap &PointsTo;
  Sema &S;
};

}  // namespace clang

#endif  // LIFETIMES_CHECKER_VISITOR_H_
