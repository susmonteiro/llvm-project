#include "clang/Sema/LifetimeAnnotationsAnalyzer.h"

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

// Process functions' headers
void LifetimeAnnotationsAnalyzer::GetLifetimes(const FunctionDecl *func) {
  debugImportant("ANALYZING FUNCTION", func->getNameAsString());
  func = func->getCanonicalDecl();

  FunctionLifetimeFactory function_lifetime_factory(func);

  llvm::Expected<FunctionLifetimes> expected_func_lifetimes =
      FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  if (!expected_func_lifetimes) {
    // TODO
    return;
  }

  FunctionLifetimes func_lifetimes = *expected_func_lifetimes;
  if (func_lifetimes.IsReturnLifetimeLocal()) {
    S.Diag(func->getLocation(), diag::warn_func_return_lifetime_local)
        << func->getSourceRange();
  }

  auto &params_lifetimes = func_lifetimes.GetParamsLifetimes();
  ObjectLifetimes return_lifetime = func_lifetimes.GetReturnLifetime();
  for (Lifetime &rl : return_lifetime.GetLifetimes()) {
    if (rl.IsStatic() || rl.IsNotSet() || rl.IsLocal() || rl.IsNull()) continue;
    auto it = params_lifetimes.begin();
    while (it != params_lifetimes.end() &&
           !it->second.HasLifetime(rl.GetId())) {
      it++;
    }

    if (it == params_lifetimes.end()) {
      S.Diag(func->getLocation(), diag::warn_func_return_unknown_lifetime)
          << rl.GetLifetimeName() << func->getSourceRange();
    }
  }

  // DEBUG
  // debugLifetimes(func_lifetimes.DebugString());
  func_lifetimes.ProcessParams();
  FunctionInfo[func] = func_lifetimes;
}

// Process functions' bodies
void LifetimeAnnotationsAnalyzer::AnalyzeFunctionBody(
    const FunctionDecl *func) {
  auto function_info = FunctionInfo[func];
  State = LifetimeAnnotationsAnalysis(function_info, func);
  std::string func_name = func->getNameAsString();

  // step 1
  debugInfo("\n====== START STEP 1 - " + func_name + " ======\n");
  GetLifetimeDependencies(func);
  debugInfo("\n====== FINISH STEP 1 - " + func_name + " ======\n");
  debugLifetimes(State.DebugString());

  // step 2
  debugInfo("\n====== START STEP 2 - " + func_name + " ======\n");
  LifetimeAnnotationsAnalyzer::PropagateLifetimes();
  debugInfo("\n====== FINISH STEP 2 - " + func_name + " ======\n");
  debugLifetimes(State.DebugString());

  // step 3
  debugInfo("\n====== START STEP 3 - " + func_name + " ======\n");
  LifetimeAnnotationsAnalyzer::CheckLifetimes(func);
  debugInfo("\n====== FINISH STEP 3 - " + func_name + " ======\n");
  debugLifetimes(State.DebugString());
}

void LifetimeAnnotationsAnalyzer::GetLifetimeDependencies(
    const clang::FunctionDecl *func) {
  LifetimesPropagationVisitor visitor(func, this, State);
  std::optional<std::string> err = visitor.Visit(func->getBody());
}

void LifetimeAnnotationsAnalyzer::PropagateLifetimes() {
  auto children = State.GetLifetimeDependencies();
  auto new_children = State.GetLifetimeDependencies();
  auto stmt_dependencies = State.GetStmtDependencies();
  auto parents = std::move(State.TransposeDependencies());

  // DEBUG
  debugLifetimes("=== dependencies_ ===");
  for (const auto &pair : children) {
    debugLifetimes(DebugDependencies(pair.first.var_decl,
                                     pair.first.num_indirections, pair.second,
                                     stmt_dependencies));
  }

  auto worklist = State.InitializeWorklist();

  // DEBUG
  int i = 1;

  while (!worklist.empty()) {
    debugInfo("---> Iteration", i++);
    // DEBUG
    // debugLifetimes("=== worklist ===");
    // debugLifetimes(LifetimeAnnotationsAnalysis::WorklistDebugString(worklist));

    auto &el = worklist.back();
    worklist.pop_back();

    auto *current_var = el.var_decl;
    unsigned int lhs_num_indirections = el.num_indirections;
    // DEBUG

    debugLifetimes("\nPropagation of", std::string(lhs_num_indirections, '*') +
                                           ' ' +
                                           current_var->getNameAsString());

    // each entry of the vector corresponds to a lifetime char
    llvm::SmallVector<llvm::DenseSet<const clang::Stmt *>> possible_lifetimes;
    llvm::DenseSet<RHSTypeStruct> stmts;

    // ObjectLifetimes objectLifetimes = State.GetObjectLifetimes(el);
    // debugLifetimes("Processing " + objectLifetimes.DebugString());

    for (auto &rhs_info : children[el]) {
      unsigned int rhs_num_indirections = rhs_info.num_indirections;
      stmts.insert(rhs_info);

      // This should be correct -> don't want to propagate $dead
      if (rhs_info.extra_lifetime >= LOCAL) {
        Lifetime::InsertDependencies(rhs_info.extra_lifetime,
                                          rhs_info.stmt, possible_lifetimes);
      }

      auto &this_stmt_dependencies = stmt_dependencies[rhs_info.stmt];
      for (const auto &var_decl : this_stmt_dependencies) {
        if (var_decl == current_var) continue;
        Lifetime &rhs_lifetime =
            State.GetLifetime(var_decl, rhs_num_indirections);
        
        if (rhs_lifetime.IsDead()) {
          continue;
        } else if (rhs_lifetime.IsNotSet()) {
          auto &rhs_possible_lifetimes = rhs_lifetime.GetDependencies();
          for (unsigned int i = 0; i < rhs_possible_lifetimes.size(); i++) {
            if (!rhs_possible_lifetimes[i].empty()) {
              Lifetime::InsertDependencies(i, rhs_info.stmt,
                                                possible_lifetimes);
            }
          }
        } else {
          char vardecl_lifetime_id = rhs_lifetime.GetId();
          Lifetime::InsertDependencies(vardecl_lifetime_id, rhs_info.stmt,
                                            possible_lifetimes);
        }
      }
    }
    // debugLifetimes("Original shortest lifetimes",
    //                State.GetPossibleLifetimes(el, el_type).size());
    // debugLifetimes("New shortest lifetimes", possible_lifetimes.size());
    // TODO this is not perfect
    if (new_children[el] != stmts ||
        State.GetPossibleLifetimes(current_var, lhs_num_indirections) !=
            possible_lifetimes) {
      new_children[el].insert(stmts.begin(), stmts.end());
      State.PropagatePossibleLifetimes(current_var, possible_lifetimes,
                                       lhs_num_indirections);
      for (const auto &parent : parents[el]) {
        worklist.emplace_back(parent);
      }
    }

    // DEBUG
    debugLifetimes("=== children ===");
    for (const auto &pair : new_children) {
      debugLifetimes(DebugDependencies(pair.first.var_decl,
                                       pair.first.num_indirections, pair.second,
                                       stmt_dependencies));
    }
  }

  State.SetDependencies(new_children);

  // finally, process the lifetimes dependencies to attribute the correct set of
  // lifetimes to each variable
  State.ProcessPossibleLifetimes();
}

// With all the lifetime information acquired, check that the return
// statements and the attributions are correct
void LifetimeAnnotationsAnalyzer::CheckLifetimes(
    const clang::FunctionDecl *func) {
  LifetimesCheckerVisitor visitor(func, State, S, FunctionInfo);
  std::optional<std::string> err = visitor.Visit(func->getBody());
}

}  // namespace clang
