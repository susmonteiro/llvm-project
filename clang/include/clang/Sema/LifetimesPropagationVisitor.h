#ifndef LIFETIMES_PROPAGATION_VISITOR_H_
#define LIFETIMES_PROPAGATION_VISITOR_H_

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Sema/LifetimeAnnotationsAnalysis.h"
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/Support/Error.h"
#include "llvm/ADT/DenseMap.h"

namespace clang {

class LifetimesPropagationVisitor
    : public clang::StmtVisitor<LifetimesPropagationVisitor,
                                std::optional<std::string>> {
 public:
  LifetimesPropagationVisitor(const clang::FunctionDecl *func,
                              LifetimeAnnotationsAnalysis &state,
                              llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &func_info)
      : Func(func), State(state), FuncInfo(func_info), PointsTo(state.GetPointsTo()), Factory(func) {}

  std::optional<std::string> VisitBinaryOperator(
      const clang::BinaryOperator *op);
  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitCallExpr(const clang::CallExpr *call);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalysis &State;
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &FuncInfo;
  PointsToMap &PointsTo;
  FunctionLifetimeFactory Factory;
};

}  // namespace clang

#endif  // LIFETIMES_PROPAGATION_VISITOR_H_
