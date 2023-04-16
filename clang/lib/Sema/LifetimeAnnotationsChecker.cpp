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
class LifetimeAnnotationsProcessor : public MatchFinder::MatchCallback {
 public:
  LifetimeAnnotationsProcessor(LifetimeAnnotationsAnalysis *state)
      : state_(state) {}

 private:
  void VisitVarDecl(const clang::VarDecl *var_decl);
  void VisitAssignment(const clang::BinaryOperator *bin_op);

  void run(const ast_matchers::MatchFinder::MatchResult &Result) override;

  LifetimeAnnotationsAnalysis *state_;
};

void LifetimeAnnotationsProcessor::VisitVarDecl(
    const clang::VarDecl *var_decl) {
  debugLifetimes("[VisitVarDecl]", var_decl->getNameAsString());
  // TODO check if pointer?
  state_->CreateVariable(var_decl);
  // TODO if annotations, store annotation

  // Don't need to record initializers because initialization has already
  // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
  // VisitCallExpr().
  if (var_decl->hasInit() && !var_decl->getType()->isRecordType()) {
    debugLifetimes("VarDecl has initializer!");
    state_->CreateDependency(var_decl);
    // TODO implement
  }
  // return std::nullopt;
}

void LifetimeAnnotationsProcessor::VisitAssignment(
    const clang::BinaryOperator *bin_op) {
  debugLifetimes("[VisitAssignment]");
  bin_op->dump();
}

void LifetimeAnnotationsProcessor::run(
    const ast_matchers::MatchFinder::MatchResult &Result) {
  // DEBUG
  // debugInfo("On run...");
  if (const auto *var_decl =
          Result.Nodes.getNodeAs<clang::VarDecl>("vardecl")) {
    VisitVarDecl(var_decl);
    return;
  }

  if (const auto *assignment =
          Result.Nodes.getNodeAs<clang::BinaryOperator>("assignment")) {
    VisitAssignment(assignment);
    return;
  }
}

}  // namespace

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::Stmt *functionBody, clang::ASTContext &Context,
    const clang::FunctionDecl *func) {
  debugLifetimes("[GetLifetimeDependencies]");
  auto var_decl_matcher =
      findAll(varDecl(unless(parmVarDecl())).bind("vardecl"));
  // FIXME
  auto assign_matcher = findAll(
      binaryOperator(hasOperatorName("="),
                     hasLHS(declRefExpr(to(varDecl().bind("rhs_vardecl")))))
          .bind("assignment"));

  MatchFinder Finder;
  LifetimeAnnotationsProcessor Callback(&state_);
  Finder.addMatcher(var_decl_matcher, &Callback);
  Finder.addMatcher(assign_matcher, &Callback);

  Finder.match(*func, Context);

  // DEBUG
  // functionBody->dump();
  // func->dump();
}

void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  // DEBUG
  // DumpFunctionInfo();

  auto functionBody = func->getBody();
  clang::ASTContext &Context = func->getASTContext();

  // step 1
  GetLifetimeDependencies(functionBody, Context, func);

  debugLifetimes(state_.DebugString());

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

    // DEBUG
    // func_lifetimes.DumpParameters();
    // func_lifetimes.DumpReturn();

    // TODO insert on map
    // TODO std::move?
    function_info_[func] = std::move(func_lifetimes);

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

// std::optional<std::string> TransferStmtVisitor::VisitExpr(
//     const clang::Expr *expr) {
//   debugLifetimes("[VisitExpr]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitDeclRefExpr(
//     const clang::DeclRefExpr *decl_ref) {
//   debugLifetimes("[VisitDeclRefExpr]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitStringLiteral(
//     const clang::StringLiteral *strlit) {
//   debugLifetimes("[VisitStringLiteral]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitCastExpr(
//     const clang::CastExpr *cast) {
//   debugLifetimes("[VisitCastExpr]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitReturnStmt(
//     const clang::ReturnStmt *return_stmt) {
//   debugLifetimes("[VisitReturnStmt]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitDeclStmt(
//     const clang::DeclStmt *decl_stmt) {
//   debugLifetimes("[VisitVarDecl]");
//   decl_stmt->dump();
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitUnaryOperator(
//     const clang::UnaryOperator *op) {
//   debugLifetimes("[VisitUnaryOperator]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitArraySubscriptExpr(
//     const clang::ArraySubscriptExpr *subscript) {
//   debugLifetimes("[VisitArraySubscriptExpr]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitBinaryOperator(
//     const clang::BinaryOperator *op) {
//   debugLifetimes("[VisitBinaryOperator]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitConditionalOperator(
//     const clang::ConditionalOperator *op) {
//   debugLifetimes("[VisitConditionalOperator]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string> TransferStmtVisitor::VisitInitListExpr(
//     const clang::InitListExpr *init_list) {
//   debugLifetimes("[VisitInitListExpr]");
//   // TODO
//   return std::nullopt;
// }

// std::optional<std::string>
// TransferStmtVisitor::VisitMaterializeTemporaryExpr(
//     const clang::MaterializeTemporaryExpr *temporary_expr) {
// debugLifetimes("[VisitMaterializeTemporaryExpr]");
//   // TODO
//   return std::nullopt;}

// std::optional<std::string> TransferStmtVisitor::VisitMemberExpr(
//     const clang::MemberExpr *member) {
// debugLifetimes("[VisitMembersExpr]");
//   // TODO
//   return std::nullopt;}

// std::optional<std::string> TransferStmtVisitor::VisitCXXThisExpr(
//     const clang::CXXThisExpr *this_expr) {
// debugLifetimes("[VisitCXXThisExpr]");
//   // TODO
//   return std::nullopt;}

// std::optional<std::string> TransferStmtVisitor::VisitCallExpr(
//     const clang::CallExpr *call) {
// debugLifetimes("[VisitCallExpr]");
//   // TODO
//   return std::nullopt;}

// std::optional<std::string> TransferStmtVisitor::VisitCXXConstructExpr(
//     const clang::CXXConstructExpr *construct_expr) {
//   debugLifetimes("[VisitCXXConstructExpr]");
//   // TODO
//   return std::nullopt;
// }

//   // std::optional<std::string> TransferStmtVisitor::VisitStmt(const
//   clang::Stmt *stmt) {
//   //   debugLifetimes("[VisitStmt] - default?");
//   //   // TODO
//   //   return std::nullopt;
//   // }

// }  // namespace

}  // namespace clang
