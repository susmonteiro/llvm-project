#ifndef LIFETIMES_PROPAGATION_VISITOR_H_
#define LIFETIMES_PROPAGATION_VISITOR_H_

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/LifetimeAnnotationsAnalysis.h"
#include "clang/Sema/LifetimeAnnotationsAnalyzer.h"
#include "clang/Sema/PointsToMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Error.h"

namespace clang {

class LifetimesPropagationVisitor
    : public clang::StmtVisitor<LifetimesPropagationVisitor,
                                std::optional<std::string>> {
 public:
  LifetimesPropagationVisitor(const clang::FunctionDecl *func,
                              LifetimeAnnotationsAnalyzer *analyzer,
                              LifetimeAnnotationsAnalysis &state)
      : Func(func),
        Analyzer(analyzer),
        State(state),
        PointsTo(state.GetPointsTo()) {}

  std::optional<std::string> VisitArraySubscriptExpr(
      const clang::ArraySubscriptExpr *expr);
  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitCallExpr(const clang::CallExpr *call);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string> VisitConditionalOperator(
      const clang::ConditionalOperator *op);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitMemberExpr(
      const clang::MemberExpr *member_expr);
  std::optional<std::string> VisitReturnStmt(
      const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);
  std::optional<std::string> VisitUnaryAddrOf(const clang::UnaryOperator *op);
  std::optional<std::string> VisitUnaryOperator(const clang::UnaryOperator *op);

  void PropagateBinAssign(const clang::Expr *lhs, const clang::Expr *rhs,
                          const clang::VarDecl *lhs_decl,
                          const clang::BinaryOperator *op) const;

 private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalyzer *Analyzer;
  LifetimeAnnotationsAnalysis &State;
  PointsToMap &PointsTo;
  bool debugEnabled = true;  // TODO delete this
};

}  // namespace clang

#endif  // LIFETIMES_PROPAGATION_VISITOR_H_
