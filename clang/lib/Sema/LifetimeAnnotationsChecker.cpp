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
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/Sema.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"
#include "clang/Sema/LifetimesPropagationVisitor.h"
#include "clang/Sema/LifetimesCheckerVisitor.h"

namespace clang {

using namespace ast_matchers;

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


// Process functions' headers
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

// Process functions' bodies
void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  // DEBUG
  // DumpFunctionInfo();
  auto function_info = function_info_[func];
  // clang::ASTContext &Context = func->getASTContext();
  state_ = LifetimeAnnotationsAnalysis(function_info);

  // step 1
  debugInfo("\n====== START STEP 1 ======\n");
  GetLifetimeDependencies(func);
  debugInfo("\n====== FINISH STEP 1 ======\n");
  debugLifetimes(state_.DebugString());

  // clang::SourceManager &source_manager = ast_context.getSourceManager();
  // clang::SourceRange func_source_range = func->getSourceRange();

  // step 2
  debugInfo("\n====== START STEP 2 ======\n");
  LifetimeAnnotationsChecker::PropagateLifetimes();
  debugInfo("\n====== FINISH STEP 2 ======\n");
  debugLifetimes(state_.DebugString());

  // step 3
  debugInfo("\n====== START STEP 3 ======\n");
  LifetimeAnnotationsChecker::CheckLifetimes(func);
  debugInfo("\n====== FINISH STEP 3 ======\n");
  debugLifetimes(state_.DebugString());
}

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::FunctionDecl *func) {
  LifetimesPropagationVisitor visitor(func, state_);
  // debugLifetimes(">> Dumping function body before visit...");
  // func->getBody()->dump();
  std::optional<std::string> err = visitor.Visit(func->getBody());
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

  // DEBUG
  int i = 1;

  while (!worklist.empty()) {
    debugInfo("---> Iteration", i++);
    debugLifetimes("=== worklist ===");
    debugLifetimes(worklist);

    auto &el = worklist.back();
    worklist.pop_back();

    debugLifetimes("\nPropagation of", el->getNameAsString());

    llvm::DenseSet<const clang::NamedDecl *> result = {el};
    llvm::DenseSet<char> shortest_lifetimes;
    for (const auto &child : children[el]) {
      if (child == el) continue;
      result.insert(children[child].begin(), children[child].end());
      auto tmp_lifetimes = state_.GetShortestLifetimes(child);
      if (state_.IsLifetimeNotset(child)) {
        shortest_lifetimes.insert(tmp_lifetimes.begin(), tmp_lifetimes.end());
      } else {
        shortest_lifetimes.insert(state_.GetLifetime(child)->Id());
      }
    }
    if (children[el] != result ||
        state_.GetShortestLifetimes(el) != shortest_lifetimes) {
      children[el].insert(result.begin(), result.end());
      state_.PropagateShortestLifetimes(el, shortest_lifetimes);

      for (const auto &parent : parents[el]) {
        worklist.emplace_back(parent);
      }
    }
    debugLifetimes("=== children ===");
    debugLifetimes(children);
  }
  // return children;

  state_.SetDependencies(children);

  debugLifetimes("=== children ===");
  debugLifetimes(children);

  debugInfo2("\n====== BEFORE PROCESSING SHORTEST LIFETIMES ======\n");
  debugLifetimes(state_.DebugString());

  // finally, process the lifetimes dependencies to attribute the correct set of
  // lifetimes to each variable
  state_.ProcessShortestLifetimes();

  // TODO
}

// With all the lifetime information acquired, check that the return
// statements and the attributions are correct
void LifetimeAnnotationsChecker::CheckLifetimes(const clang::FunctionDecl *func) {
  LifetimesCheckerVisitor visitor(func, state_);
  std::optional<std::string> err = visitor.Visit(func->getBody());

}

}  // namespace clang
