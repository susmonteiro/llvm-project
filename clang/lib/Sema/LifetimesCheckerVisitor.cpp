#include "clang/Sema/LifetimesCheckerVisitor.h"

namespace clang {

std::optional<std::string> LifetimesCheckerVisitor::VisitCompoundStmt(
    const clang::CompoundStmt *stmt) {
  debugLifetimes("[VisitCompoundStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt]");
  // TODO
  return std::nullopt;
}

}  // namespace clang
