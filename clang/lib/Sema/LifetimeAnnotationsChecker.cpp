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
  LifetimeAnnotationsProcessor(LifetimeAnnotationsAnalysis *state,
                               FunctionLifetimes *func_info)
      : state_(state), func_info_(func_info) {}

 private:
  void VisitVarDecl(const clang::VarDecl *var_decl);
  void VisitCallExpr(const clang::CallExpr *call_expr);
  void VisitAssignment(const clang::BinaryOperator *bin_op);

  void run(const ast_matchers::MatchFinder::MatchResult &Result) override;

  LifetimeAnnotationsAnalysis *state_;
  FunctionLifetimes *func_info_;
};

}  // namespace

// TODO change to not void
void GetExprObjectSet(const clang::Expr *expr,
                      LifetimeAnnotationsAnalysis *state) {
  // We can't handle `ParenExpr`s like other `Expr`s because the CFG doesn't
  // contain `CFGStmt`s for them. Instead, if we encounter a `ParenExpr` here,
  // we simply return the object set for its subexpression.
  if (auto paren = clang::dyn_cast<clang::ParenExpr>(expr)) {
    expr = paren->getSubExpr();
  }

  assert(expr->isGLValue() || expr->getType()->isPointerType() ||
         expr->getType()->isArrayType() || expr->getType()->isFunctionType() ||
         expr->getType()->isBuiltinType());

  // TODO implement this better

  // const auto &variable_lifetimes = state->GetVariableLifetimes();
  // auto iter = variable_lifetimes.find(expr);
  // if (iter == variable_lifetimes.end()) {
  //   // TODO error?
  // }
  // return iter->second();
}

namespace {

void LifetimeAnnotationsProcessor::VisitVarDecl(
    const clang::VarDecl *var_decl) {
  debugLifetimes("[VisitVarDecl]", var_decl->getNameAsString());
  // TODO check if pointer?
  state_->CreateVariable(var_decl);
  // TODO if annotations, store annotation

  // Don't need to record initializers because initialization has already
  // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
  // VisitCallExpr().
  // If the variable of the LHS has a lifetime annotation, don't process RHS
  if (var_decl->hasInit() && !var_decl->getType()->isRecordType() &&
      state_->GetLifetime(var_decl).IsUnset()) {
    debugLifetimes("VarDecl has initializer!");
    const clang::Expr *init_expr = var_decl->getInit();

    GetExprObjectSet(init_expr, state_);
    if (const clang::DeclRefExpr *rhs =
            dyn_cast<clang::DeclRefExpr>(init_expr)) {
      debugLifetimes("RHS is a DeclRefExpr");
      state_->CreateDependency(var_decl, rhs);
    }
    // TODO implement
  }
  // return std::nullopt;
}

void LifetimeAnnotationsProcessor::VisitCallExpr(
    const clang::CallExpr *call_expr) {
  debugLifetimes("[VisitCallExpr]");
}

void LifetimeAnnotationsProcessor::VisitAssignment(
    const clang::BinaryOperator *bin_op) {
  debugLifetimes("[VisitAssignment]");
  bin_op->dump();
}

void LifetimeAnnotationsProcessor::run(
    const ast_matchers::MatchFinder::MatchResult &Result) {
  // DEBUG
  debugInfo("On run...");
  if (const auto *var_decl =
          Result.Nodes.getNodeAs<clang::VarDecl>("var_decl")) {
    VisitVarDecl(var_decl);
    return;
  }

  if (const auto *call_expr =
          Result.Nodes.getNodeAs<clang::CallExpr>("call_expr")) {
    VisitCallExpr(call_expr);
    return;
  }

  if (const auto *assignment =
          Result.Nodes.getNodeAs<clang::BinaryOperator>("assignment")) {
    VisitAssignment(assignment);
    return;
  }
  debugWarn("Matched something with no visit function");
}

}  // namespace

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::Stmt *functionBody, clang::ASTContext &Context,
    const clang::FunctionDecl *func, FunctionLifetimes &func_info) {
  debugLifetimes("[GetLifetimeDependencies]");
  // auto expr_matcher = findAll(expr());
  auto var_decl_matcher =
      findAll(varDecl(unless(parmVarDecl())).bind("var_decl"));

  auto call_expr_matcher = findAll(callExpr().bind("call_expr"));

  // FIXME
  // auto assign_matcher = findAll(
  //     binaryOperator(hasOperatorName("="),
  //                    hasLHS(declRefExpr(to(varDecl().bind("rhs_vardecl")))))
  //         .bind("assignment"));
  // TODO try on a matcher which receives func->getCompoundStmt()
  auto assign_matcher = compoundStmt(hasDescendant(binaryOperator()));

  MatchFinder Finder;
  LifetimeAnnotationsProcessor Callback(&state_, &func_info);
  Finder.addMatcher(var_decl_matcher, &Callback);
  Finder.addMatcher(call_expr_matcher, &Callback);
  Finder.addMatcher(assign_matcher, &Callback);
  // Finder.addMatcher(expr_matcher, &Callback);

  // func->dump();
  // decls need func
  Finder.match(*func, Context);
  // exprs need functionBody
  Finder.match(*functionBody, Context);
  // FIXME operators?

  // DEBUG
  // functionBody->dump();
  // func->dump();
}

void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  // DEBUG
  // DumpFunctionInfo();

  auto function_body = func->getBody();
  auto function_info = function_info_[func];
  clang::ASTContext &Context = func->getASTContext();
  state_ = LifetimeAnnotationsAnalysis(function_info.GetParamsLifetimes());

  // TODO create new state for the new function
  // TODO copy the lifetimes of the parameters to the new state

  // step 1
  GetLifetimeDependencies(function_body, Context, func, function_info);

  debugLifetimes(state_.DebugString());

  // clang::SourceManager &source_manager = ast_context.getSourceManager();
  // clang::SourceRange func_source_range = func->getSourceRange();

  // step 2
  LifetimeAnnotationsChecker::PropagateLifetimes();

  // step 3
  LifetimeAnnotationsChecker::CheckLifetimes();
}

// After capturing lifetimes from the function, apply the fixed point
// algorithm
void LifetimeAnnotationsChecker::PropagateLifetimes() {
  auto children = state_.GetDependencies();
  auto parents = std::move(state_.TransposeDependencies());

  debugLifetimes("=== dependencies_ ===");
  debugLifetimes(children);

  debugLifetimes("=== parents (transposed) ===");
  debugLifetimes(parents);

  auto worklist = state_.InitializeWorklist();

  debugLifetimes("=== worklist ===");
  debugLifetimes(worklist);

  while (!worklist.empty()) {
    auto &el = worklist.back();
    worklist.pop_back();
    // TODO duplicates?
    llvm::DenseSet<const clang::NamedDecl *> result = {el};
    for (const auto &child : children[el]) {
      if (child == el) continue;
      result.insert(children[child].begin(), children[child].end());
    }
    if (children[el].size() != result.size()) {
      children[el] = result;
      for (const auto &parent : parents[el]) worklist.emplace_back(parent);
    }
    debugLifetimes("\nPropagation of", el->getNameAsString());
    debugLifetimes("=== dependencies_ ===");
    debugLifetimes(children[el]);
  }
  // return children;
  // TODO
}

void LifetimeAnnotationsChecker::CheckLifetimes() {
  // TODO
  // With all the lifetime information acquired, check that the return
  // statements and the attributions are correct
}

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
