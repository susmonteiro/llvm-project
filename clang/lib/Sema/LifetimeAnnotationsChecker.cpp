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
  expr = expr->IgnoreParens();

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
    func_lifetimes.ProcessParams();
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

void LifetimeAnnotationsChecker::PropagateLifetimes() {
  auto children = State.GetLifetimeDependencies();
  auto new_children = State.GetLifetimeDependencies();
  auto stmt_dependencies = State.GetStmtDependencies();
  auto parents = std::move(State.TransposeDependencies());

  debugLifetimes("=== dependencies_ ===");
  for (const auto &pair : children) {
    debugLifetimes(pair.first.first, pair.first.second, pair.second,
                   stmt_dependencies);
  }

  debugLifetimes("State at the beginning of step 2", State.DebugString());

  // debugLifetimes("=== parents (transposed) ===");
  // debugLifetimes(parents);

  auto worklist = State.InitializeWorklist();

  // DEBUG
  int i = 1;

  while (!worklist.empty()) {
    debugInfo("---> Iteration", i++);
    debugLifetimes("=== worklist ===");
    debugLifetimes(worklist);

    auto &el = worklist.back();
    worklist.pop_back();

    auto *current_var = el.first;
    auto current_type = el.second;
    debugLifetimes("\nPropagation of", current_type.getAsString() + ' ' +
                                           current_var->getNameAsString());

    // each entry of the vector corresponds to a lifetime char
    llvm::SmallVector<llvm::DenseSet<const clang::Stmt *>> shortest_lifetimes;
    llvm::DenseSet<const clang::Stmt *> stmts;

    // ObjectLifetimes objectLifetimes = State.GetObjectLifetimes(el);
    // debugLifetimes("Processing " + objectLifetimes.DebugString());

    for (auto &stmt : children[el]) {
      stmts.insert(stmt);
      for (const auto &var_decl : stmt_dependencies[stmt]) {
        if (var_decl == current_var) continue;
        Lifetime &rhs_lifetime =
            State.GetLifetimeOrLocal(var_decl, current_type);
        auto &rhs_shortest_lifetimes = rhs_lifetime.GetShortestLifetimes();

        // TODO relation between lifetimes and stmts
        if (rhs_lifetime.IsNotSet()) {
          for (unsigned int i = 0; i < rhs_shortest_lifetimes.size(); i++) {
            if (!rhs_shortest_lifetimes[i].empty()) {
              Lifetime::InsertShortestLifetimes(i, stmt, shortest_lifetimes);
            }
          }
        } else {
          char vardecl_lifetime_id = rhs_lifetime.GetId();
          Lifetime::InsertShortestLifetimes(vardecl_lifetime_id, stmt,
                                            shortest_lifetimes);
        }
        // }
      }
    }
    // debugLifetimes("Original shortest lifetimes",
    //                State.GetShortestLifetimes(el, el_type).size());
    // debugLifetimes("New shortest lifetimes", shortest_lifetimes.size());
    // TODO this is not perfect
    if (new_children[el] != stmts ||
        State.GetShortestLifetimes(current_var, current_type) !=
            shortest_lifetimes) {
      new_children[el].insert(stmts.begin(), stmts.end());
      State.PropagateShortestLifetimes(current_var, shortest_lifetimes,
                                       current_type);
      for (const auto &parent : parents[el]) {
        worklist.emplace_back(parent);
      }
    }

    debugLifetimes("=== children ===");
    for (const auto &pair : new_children) {
      debugLifetimes(pair.first.first, pair.first.second, pair.second,
                     stmt_dependencies);
    }
  }

  State.SetDependencies(new_children);

  // finally, process the lifetimes dependencies to attribute the correct set of
  // lifetimes to each variable
  State.ProcessShortestLifetimes();
}

// With all the lifetime information acquired, check that the return
// statements and the attributions are correct
void LifetimeAnnotationsChecker::CheckLifetimes(const clang::FunctionDecl *func,
                                                Sema &S) {
  LifetimesCheckerVisitor visitor(func, State, S, FunctionInfo);
  std::optional<std::string> err = visitor.Visit(func->getBody());
}

}  // namespace clang
