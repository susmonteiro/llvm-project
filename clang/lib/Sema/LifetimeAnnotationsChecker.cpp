#include "clang/Sema/LifetimeAnnotationsChecker.h"

#include <iostream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/IdentifierResolver.h"
#include "clang/Sema/LifetimesCheckerVisitor.h"
#include "clang/Sema/LifetimesPropagationVisitor.h"
#include "clang/Sema/PointsToMap.h"
#include "llvm/Support/Error.h"

namespace clang {

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

// Process functions' headers
void LifetimeAnnotationsChecker::GetLifetimes(const FunctionDecl *func,
                                              Sema &S) {
  debugImportant("ANALYZING FUNCTION", func->getNameAsString());
  func = func->getCanonicalDecl();

  if (!func->isDefined()) {
    // DEBUG
    // debugLifetimes("Function is not defined");
    // func->dump();
    // TODO error?
  }

  // TODO ellision

  // Following Case 2. Not part of a cycle.
  // AnalyzeSingleFunction()
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func);

  llvm::Expected<FunctionLifetimes> expected_func_lifetimes =
      FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  if (expected_func_lifetimes) {
    FunctionLifetimes func_lifetimes = *expected_func_lifetimes;

    // DEBUG
    // debugLifetimes(func_lifetimes.DebugString());

    FunctionInfo[func] = func_lifetimes;
    // TODO maybe keep track of analyzed functions
    // TODO need to check if the lifetimes in the declaration and definition are
    // the same?
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
  auto function_info = FunctionInfo[func];
  State = LifetimeAnnotationsAnalysis(function_info);

  // step 1
  debugInfo("\n====== START STEP 1 ======\n");
  GetLifetimeDependencies(func);
  debugInfo("\n====== FINISH STEP 1 ======\n");
  debugLifetimes(State.DebugString());

  // step 2
  debugInfo("\n====== START STEP 2 ======\n");
  LifetimeAnnotationsChecker::PropagateLifetimes();
  debugInfo("\n====== FINISH STEP 2 ======\n");
  debugLifetimes(State.DebugString());

  // step 3
  debugInfo("\n====== START STEP 3 ======\n");
  LifetimeAnnotationsChecker::CheckLifetimes(func, S);
  debugInfo("\n====== FINISH STEP 3 ======\n");
  debugLifetimes(State.DebugString());
}

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::FunctionDecl *func) {
  LifetimesPropagationVisitor visitor(func, State, FunctionInfo);
  std::optional<std::string> err = visitor.Visit(func->getBody());
}

// After capturing lifetimes from the function, apply the fixed point
// algorithm
void LifetimeAnnotationsChecker::PropagateLifetimes() {
  auto children = State.GetDependencies();
  auto parents = std::move(State.TransposeDependencies());

  debugLifetimes("=== dependencies_ ===");
  debugLifetimes(children);

  debugLifetimes("=== parents (transposed) ===");
  debugLifetimes(parents);

  auto worklist = State.InitializeWorklist();

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
      auto tmp_lifetimes = State.GetShortestLifetimes(child);
      if (State.IsLifetimeNotset(child)) {
        shortest_lifetimes.insert(tmp_lifetimes.begin(), tmp_lifetimes.end());
      } else {
        shortest_lifetimes.insert(State.GetLifetime(child).GetId());
      }
    }
    if (children[el] != result ||
        State.GetShortestLifetimes(el) != shortest_lifetimes) {
      children[el].insert(result.begin(), result.end());
      State.PropagateShortestLifetimes(el, shortest_lifetimes);

      for (const auto &parent : parents[el]) {
        worklist.emplace_back(parent);
      }
    }
    debugLifetimes("=== children ===");
    debugLifetimes(children);
  }

  State.SetDependencies(children);

  debugLifetimes("=== children ===");
  debugLifetimes(children);

  debugInfo2("\n====== BEFORE PROCESSING SHORTEST LIFETIMES ======\n");
  debugLifetimes(State.DebugString());

  // finally, process the lifetimes dependencies to attribute the correct set of
  // lifetimes to each variable
  State.ProcessShortestLifetimes();
}

// With all the lifetime information acquired, check that the return
// statements and the attributions are correct
void LifetimeAnnotationsChecker::CheckLifetimes(const clang::FunctionDecl *func,
                                                Sema &S) {
  LifetimesCheckerVisitor visitor(func, State, S);
  std::optional<std::string> err = visitor.Visit(func->getBody());
}

}  // namespace clang
