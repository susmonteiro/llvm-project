#include "clang/Sema/LifetimeAnnotationsChecker.h"

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
#include "clang/Sema/Sema.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"

namespace clang {

using namespace ast_matchers;

namespace {
class TransferStmtVisitor
    : public clang::StmtVisitor<TransferStmtVisitor,
                                std::optional<std::string>> {
 public:
  TransferStmtVisitor(const clang::FunctionDecl *func) : func_(func) {}

  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitStringLiteral(
      const clang::StringLiteral *strlit);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string> VisitReturnStmt(
      const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitUnaryOperator(const clang::UnaryOperator *op);
  std::optional<std::string> VisitArraySubscriptExpr(
      const clang::ArraySubscriptExpr *subscript);
  std::optional<std::string> VisitBinaryOperator(
      const clang::BinaryOperator *op);
  std::optional<std::string> VisitConditionalOperator(
      const clang::ConditionalOperator *op);
  std::optional<std::string> VisitInitListExpr(
      const clang::InitListExpr *init_list);
  std::optional<std::string> VisitMaterializeTemporaryExpr(
      const clang::MaterializeTemporaryExpr *temporary_expr);
  std::optional<std::string> VisitMemberExpr(const clang::MemberExpr *member);
  std::optional<std::string> VisitCXXThisExpr(
      const clang::CXXThisExpr *this_expr);
  std::optional<std::string> VisitCallExpr(const clang::CallExpr *call);
  std::optional<std::string> VisitCXXConstructExpr(
      const clang::CXXConstructExpr *construct_expr);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

 private:
  const clang::FunctionDecl *func_;
};

}  // namespace

void VisitVarDecl(const clang::VarDecl *var_decl) {

}

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::Stmt *functionBody, clang::ASTContext &Context,
    const clang::FunctionDecl *func) {
  auto matcher = compoundStmt(hasDescendant(varDecl().bind("vardecl")));
  // auto matcher = stmt();

  TransferStmtVisitor visitor(func);

  auto Results = match(matcher, *functionBody, Context);

  // DEBUG
  // functionBody->dump();

  if (Results.empty()) {
    // FIXME
    debugWarn("Results is empty");
    return;
  }

  const BoundNodes &Nodes = Results[0];

  debugLifetimes("Matched expressions");

  // for (const auto &node : Nodes) {
  const auto *var_decl = Nodes.getNodeAs<clang::VarDecl>("vardecl");
  VisitVarDecl(var_decl);
  debugLifetimes("Visiting stmt");
  // auto *after_cast = const_cast<clang::Stmt *>(stmt);
  // visitor.VisitVarDecl(vardecl);
  // debugLifetimes("After cast");
  // std::optional<std::string> err = visitor.Visit(after_cast);
  // debugLifetimes("Error?");
  // stmt->dump();
  // }
}

void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  auto functionBody = func->getBody();
  clang::ASTContext &Context = func->getASTContext();

  // step 1
  GetLifetimeDependencies(functionBody, Context, func);

  // clang::SourceManager &source_manager = ast_context.getSourceManager();
  // clang::SourceRange func_source_range = func->getSourceRange();

  // step 2
  LifetimeAnnotationsChecker::PropagateLifetimes();

  // step 3
  LifetimeAnnotationsChecker::CheckLifetimes();
}

void LifetimeAnnotationsChecker::PropagateLifetimes() {
  // TODO
  // After capturing lifetimes from the function, apply the fixed point
  // algorithm
}

void LifetimeAnnotationsChecker::CheckLifetimes() {
  // TODO
  // With all the lifetime information acquired, check that the return
  // statements and the attributions are correct
}

// TODO list
//   TODO add this function header to .h
//   TODO define the class TransferStmtVisitor in this file
//   TODO define LifetimeLattice class
//   TODO make this function be called from the GetLifetimes
//   TODO implement some functions of the StmtVisitor

// void LifetimeAnnotationsChecker::transfer(
//     const clang::CFGElement &elt, LifetimeLattice &state,
//     clang::dataflow::Environment & /*environment*/) {
//   if (state.IsError())
//     return;

//   auto cfg_stmt = elt.getAs<clang::CFGStmt>();
//   if (!cfg_stmt)
//     return;
//   auto stmt = cfg_stmt->getStmt();

//   TransferStmtVisitor visitor(object_repository_, state.PointsTo(),
//                               state.Constraints(),
//                               state.SingleValuedObjects(), func_,
//                               callee_lifetimes_, diag_reporter_);

//   // * visitor pattern -> visit the specific function and handle different
//   // * elements in each specific way
//   if (std::optional<std::string> err =
//           visitor.Visit(const_cast<clang::Stmt *>(stmt))) {
//     state = LifetimeLattice(*err);
//   }
// }

// TODO this is just an experience - delete or change this
// void CheckReturnLifetime(const clang::FunctionDecl *func,
//                          FunctionLifetimes &function_lifetimes, Sema &S) {
//   clang::QualType return_type = func->getReturnType();
//   clang::QualType return_pointee = PointeeType(return_type);
//   if (return_pointee.isNull()) return;

//   Lifetime l = function_lifetimes.GetReturnLifetimes();

//   if (!function_lifetimes.CheckIfLifetimeIsDefined(l))
//     S.Diag(func->getLocation(), diag::warn_return_undefined_lifetime)
//         << func->getSourceRange();
// }

void LifetimeAnnotationsChecker::GetLifetimes(const FunctionDecl *func,
                                              Sema &S) {
  debugLifetimes("GetLifetimes of function", func->getNameAsString());

  // experiment();

  // BuildBaseToOverrides
  // AnalyzeTranslationUnitAndCollectTemplates -> templates

  func = func->getCanonicalDecl();

  // TODO AnalyzeFunctionRecursive -> templates, virtual, etc.
  // auto *cxxmethod = clang::dyn_cast<clang::CXXMethodDecl>(func);
  // bool is_virtual = cxxmethod != nullptr && cxxmethod->isVirtual();
  // bool is_pure_virtual = is_virtual && cxxmethod->isPure();

  // TODO uncomment when we have defined the data structures
  // if (!func->isDefined() && !is_pure_virtual && !is_analyzed) {
  //     FunctionLifetimes annotations;
  //     if (llvm::Error err = GetLifetimeAnnotations(func, lifetime_context)
  //                             .moveInto(annotations)) {
  //     analyzed[func] = FunctionAnalysisError(err);
  //     } else {
  //     analyzed[func] = annotations;
  //     }
  //     return;
  // }

  if (!func->isDefined()) {
    // DEBUG
    // debugLifetimes("Function is not defined");
    // func->dump();
  }

  // TODO ellision

  // Following Case 2. Not part of a cycle.
  // AnalyzeSingleFunction()
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func);

  // if (function_info_[func]) {
  //   debugLifetimes("Before setting of map");

  //   func_lifetimes.DumpParameters();
  //   func_lifetimes.DumpReturn();

  //   function_info_[func] = std::move(func_lifetimes);

  //   // TODO this should be validated in the check step
  //   // CheckReturnLifetime(func, func_lifetimes, S);

  //   debugLifetimes("After setting of map");

  //   // TODO maybe keep track of analyzed functions

  // } else {
  //   // TODO error
  //   /* return llvm::createStringError(
  //         llvm::inconvertibleErrorCode(),
  //         llvm::toString(func_lifetimes.takeError())
  //         // TODO abseil
  //         // absl::StrCat("Lifetime elision not enabled for '",
  //         //              func->getNameAsString(), "'")
  //     ); */
  //   return;
  // }

  llvm::Expected<FunctionLifetimes> expected_func_lifetimes =
      FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  if (expected_func_lifetimes) {
    FunctionLifetimes func_lifetimes = *expected_func_lifetimes;

    func_lifetimes.DumpParameters();
    func_lifetimes.DumpReturn();

    // TODO insert on map

    // TODO this should be validated in the check step
    // CheckReturnLifetime(func, func_lifetimes, S);

    // TODO maybe keep track of analyzed functions

  } else {
    // TODO error
    /* return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          llvm::toString(func_lifetimes.takeError())
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
      ); */
    return;
  }
}

// namespace {
// // Below is the implementation of all visit functions
// std::optional<std::string> TransferStmtVisitor::VisitDeclStmt(
//     const clang::DeclStmt *decl_stmt) {
//   // TODO implement
// }
// }  // namespace

namespace {

std::optional<std::string> TransferStmtVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  debugLifetimes("[VisitDeclRefExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitStringLiteral(
    const clang::StringLiteral *strlit) {
  debugLifetimes("[VisitStringLiteral]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitCastExpr(
    const clang::CastExpr *cast) {
  debugLifetimes("[VisitCastExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitReturnStmt(
    const clang::ReturnStmt *return_stmt) {
  debugLifetimes("[VisitReturnStmt]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  debugLifetimes("[VisitVarDecl]");
  decl_stmt->dump();
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitUnaryOperator(
    const clang::UnaryOperator *op) {
  debugLifetimes("[VisitUnaryOperator]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitArraySubscriptExpr(
    const clang::ArraySubscriptExpr *subscript) {
  debugLifetimes("[VisitArraySubscriptExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitBinaryOperator(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinaryOperator]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitConditionalOperator(
    const clang::ConditionalOperator *op) {
  debugLifetimes("[VisitConditionalOperator]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitInitListExpr(
    const clang::InitListExpr *init_list) {
  debugLifetimes("[VisitInitListExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitMaterializeTemporaryExpr(
    const clang::MaterializeTemporaryExpr *temporary_expr) {
debugLifetimes("[VisitMaterializeTemporaryExpr]");
  // TODO
  return std::nullopt;}

std::optional<std::string> TransferStmtVisitor::VisitMemberExpr(
    const clang::MemberExpr *member) {
debugLifetimes("[VisitMembersExpr]");
  // TODO
  return std::nullopt;}

std::optional<std::string> TransferStmtVisitor::VisitCXXThisExpr(
    const clang::CXXThisExpr *this_expr) {
debugLifetimes("[VisitCXXThisExpr]");
  // TODO
  return std::nullopt;}

std::optional<std::string> TransferStmtVisitor::VisitCallExpr(
    const clang::CallExpr *call) {
debugLifetimes("[VisitCallExpr]");
  // TODO
  return std::nullopt;}

std::optional<std::string> TransferStmtVisitor::VisitCXXConstructExpr(
    const clang::CXXConstructExpr *construct_expr) {
  debugLifetimes("[VisitCXXConstructExpr]");
  // TODO
  return std::nullopt;
}

  std::optional<std::string> TransferStmtVisitor::VisitStmt(const clang::Stmt *stmt) {
    debugLifetimes("[VisitStmt] - default?");
    // TODO 
    return std::nullopt;
  }


}  // namespace

}  // namespace clang
