#ifndef LIFETIMES_PROPAGATION_VISITOR_H_
#define LIFETIMES_PROPAGATION_VISITOR_H_

#include <iostream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/IdentifierResolver.h"
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/Sema.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"

namespace clang {

class LifetimesPropagationVisitor
    : public clang::StmtVisitor<LifetimesPropagationVisitor,
                                std::optional<std::string>> {
 public:
  // TODO func: pointer or reference?
  LifetimesPropagationVisitor(const clang::FunctionDecl *func,
                              LifetimeAnnotationsAnalysis &state)
      : func_(func), state_(state), factory(func) {}

  std::optional<std::string> VisitBinaryOperator(
      const clang::BinaryOperator *op);
  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string> VisitCompoundStmt(const clang::CompoundStmt *stmt);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

 private:
  const clang::FunctionDecl *func_;
  LifetimeAnnotationsAnalysis &state_;
  PointsToMap points_to_map;
  FunctionLifetimeFactory factory;
};

}  // namespace clang

#endif  // LIFETIMES_PROPAGATION_VISITOR_H_
