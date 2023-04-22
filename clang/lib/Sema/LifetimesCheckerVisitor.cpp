#include "clang/Sema/LifetimesCheckerVisitor.h"

namespace clang {

std::optional<std::string> LifetimesCheckerVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  debugLifetimes("[VisitReturnStmt]");
  debugLifetimes("This is the return lifetime",
                 state_.GetReturnLifetime().DebugString());

  clang::QualType return_type = func_->getReturnType();

  // We only need to handle pointers and references.
  // For record types, initialization of the return value has already been
  // handled in VisitCXXConstructExpr() or VisitInitListExpr(), so nothing
  // to do here.
  if (!return_type->isPointerType() && !return_type->isReferenceType()) {
    return std::nullopt;
  }

  Lifetime &return_lifetime = state_.GetReturnLifetime();
  if (return_lifetime.IsNotSet()) {
    debugWarn("Return does not have a valid lifetime");
    // TODO error
  }

  // TODO need to analyze expression from ReturnStmt and then get the value from points_to
  Visit(const_cast<clang::Expr*>(return_stmt->getRetValue()));
  // TODO need to implement operator == for Lifetimes

  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesCheckerVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

}  // namespace clang
