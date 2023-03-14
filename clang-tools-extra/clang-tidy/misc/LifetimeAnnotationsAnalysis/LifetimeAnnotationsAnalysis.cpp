#include "LifetimeAnnotationsAnalysis.h"

#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "Lifetime.h"
#include "Object.h"
#include "ObjectSet.h"
#include "Object_repository.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/Type.h"
#include "clang/Analysis/CFG.h"
#include "clang/Analysis/FlowSensitive/DataflowAnalysis.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

void debug(std::string text) {
  std::cout << "\033[1;34m>> \033[0m" << text << std::endl;
}

void debugVisitor(std::string text) {
  std::cout << "\033[1;35m[visitor] \033[0m" << text << std::endl;
}

namespace clang {
namespace tidy {
namespace lifetimes {

namespace {

class TransferStmtVisitor
    : public clang::StmtVisitor<TransferStmtVisitor,
                                std::optional<std::string>> {
public:
  TransferStmtVisitor(
      ObjectRepository &object_repository, PointsToMap &points_to_map,
      LifetimeConstraints &constraints, ObjectSet &single_valued_objects,
      const clang::FunctionDecl *func,
      const llvm::DenseMap<const clang::FunctionDecl *,
                           FunctionLifetimesOrError> &callee_lifetimes,
      const DiagnosticReporter &diag_reporter)
      : object_repository_(object_repository), points_to_map_(points_to_map),
        constraints_(constraints),
        single_valued_objects_(single_valued_objects), func_(func),
        callee_lifetimes_(callee_lifetimes), diag_reporter_(diag_reporter) {}

  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string>
  VisitDeclRefExpr(const clang::DeclRefExpr *decl_ref);
  std::optional<std::string>
  VisitStringLiteral(const clang::StringLiteral *strlit);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string>
  VisitReturnStmt(const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitUnaryOperator(const clang::UnaryOperator *op);
  std::optional<std::string>
  VisitArraySubscriptExpr(const clang::ArraySubscriptExpr *subscript);
  std::optional<std::string>
  VisitBinaryOperator(const clang::BinaryOperator *op);
  std::optional<std::string>
  VisitConditionalOperator(const clang::ConditionalOperator *op);
  std::optional<std::string>
  VisitInitListExpr(const clang::InitListExpr *init_list);
  std::optional<std::string> VisitMaterializeTemporaryExpr(
      const clang::MaterializeTemporaryExpr *temporary_expr);
  std::optional<std::string> VisitMemberExpr(const clang::MemberExpr *member);
  std::optional<std::string>
  VisitCXXThisExpr(const clang::CXXThisExpr *this_expr);
  std::optional<std::string> VisitCallExpr(const clang::CallExpr *call);
  std::optional<std::string>
  VisitCXXConstructExpr(const clang::CXXConstructExpr *construct_expr);

private:
  ObjectRepository &object_repository_;
  ObjectSet &single_valued_objects_;
  const clang::FunctionDecl *func_;
  // TODO implement callee lifetimes
  // TODO implement diagnosticreporter
  //   const llvm::DenseMap<const clang::FunctionDecl *,
  //   FunctionLifetimesOrError>
  //       &callee_lifetimes_;
  //   const DiagnosticReporter &diag_reporter_;
};

LifetimeLattice LifetimeAnalysis::initialElement() {
  // * empty points_to_map
  // * empty single_valued_objects
  // + single valued objects hold just one value at a time
  // + such as int, char, float or double
  return LifetimeLattice(object_repository_.InitialPointsToMap(),
                         object_repository_.InitialSingleValuedObjects());
}

std::string LifetimeAnalysis::ToString(const LifetimeLattice &state) {
  return state.ToString();
}

bool LifetimeAnalysis::IsEqual(const LifetimeLattice &state1,
                               const LifetimeLattice &state2) {
  return state1 == state2;
}

// TODO implement this function
void LifetimeAnalysis::transfer(
    const clang::CFGElement &elt, LifetimeLattice &state,
    clang::dataflow::Environment & /*environment*/) {
  if (state.IsError())
    return;

  auto cfg_stmt = elt.getAs<clang::CFGStmt>();
  if (!cfg_stmt)
    return;
  auto stmt = cfg_stmt->getStmt();

  TransferStmtVisitor visitor(object_repository_, state.PointsTo(),
                              state.Constraints(), state.SingleValuedObjects(),
                              func_, callee_lifetimes_, diag_reporter_);

  // * visitor pattern -> visit the specific function and handle different
  // * elements in each specific way
  if (std::optional<std::string> err =
          visitor.Visit(const_cast<clang::Stmt *>(stmt))) {
    state = LifetimeLattice(*err);
  }
}

namespace {

// * Below is the implementation of all visit functions defined in the
// * LifetimeAnalysis class

std::optional<std::string>
TransferStmtVisitor::VisitExpr(const clang::Expr *expr) {
  debugVisitor("VisitExpr");
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitDeclRefExpr(const clang::DeclRefExpr *decl_ref) {
  debugVisitor("VisitDeclRefExpr");
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitStringLiteral(const clang::StringLiteral *strlit) {
  debugVisitor("VisitStringLiteral");
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitCastExpr(const clang::CastExpr *cast) {
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitReturnStmt(const clang::ReturnStmt *return_stmt) {
  clang::QualType return_type = func_->getReturnType();
  debugVisitor("VisitReturnStmt");
  // TODO
  debugVisitor("End VisitReturnStmt");
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitDeclStmt(const clang::DeclStmt *decl_stmt) {
  debugVisitor("VisitDeclStmt");
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitUnaryOperator(const clang::UnaryOperator *op) {
  debugVisitor("VisitUnaryOperator");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitArraySubscriptExpr(
    const clang::ArraySubscriptExpr *subscript) {
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitConditionalOperator(
    const clang::ConditionalOperator *op) {
  debugVisitor("VisitConditionalOperator");
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitInitListExpr(const clang::InitListExpr *init_list) {
  debugVisitor("VisitInitListExpr");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitMaterializeTemporaryExpr(
    const clang::MaterializeTemporaryExpr *temporary_expr) {
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitCXXThisExpr(const clang::CXXThisExpr *this_expr) {
  // TODO
  return std::nullopt;
}

std::optional<std::string>
TransferStmtVisitor::VisitCallExpr(const clang::CallExpr *call) {
  debugVisitor("VisitCallExpr");
  // TODO
  return std::nullopt;
} // namespace

std::optional<std::string> TransferStmtVisitor::VisitCXXConstructExpr(
    const clang::CXXConstructExpr *construct_expr) {
  // TODO
  return std::nullopt;
}

} // namespace

} // namespace lifetimes
} // namespace tidy
} // namespace clang
