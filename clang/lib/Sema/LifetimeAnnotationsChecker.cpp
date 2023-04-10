#include "LifetimeAnnotationsChecker.h"

#include <iostream>

#include "FunctionLifetimes.h"
#include "llvm/Support/Error.h"

namespace clang {

void LifetimeAnnotationsChecker::PropagateLifetimes() {
  // TODO
  // After capturing lifetimes from the function, apply the fixed point algorithm
}

void LifetimeAnnotationsChecker::CheckLifetimes() {
  // TODO
  // With all the lifetime information acquired, check that the return statements and the attributions are correct 
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
//                               state.Constraints(), state.SingleValuedObjects(),
//                               func_, callee_lifetimes_, diag_reporter_);

//   // * visitor pattern -> visit the specific function and handle different
//   // * elements in each specific way
//   if (std::optional<std::string> err =
//           visitor.Visit(const_cast<clang::Stmt *>(stmt))) {
//     state = LifetimeLattice(*err);
//   }
// }

void LifetimeAnnotationsChecker::GetLifetimes(FunctionDecl* func) {
  debugLifetimes("Analyzing function", func->getNameAsString());

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
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func);
  auto func_lifetimes =
      FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  if (!func_lifetimes) {
    /* return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          llvm::toString(func_lifetimes.takeError())
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
      ); */
    // TODO error
  }

  func_lifetimes->DumpParameters();
  func_lifetimes->DumpReturn();


  // TODO keep track of analyzed functions

  // step 2
  LifetimeAnnotationsChecker::PropagateLifetimes();

  // step 3
  LifetimeAnnotationsChecker::CheckLifetimes();
}
}  // namespace clang
