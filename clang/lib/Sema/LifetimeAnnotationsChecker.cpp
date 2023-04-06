#include "LifetimeAnnotationsChecker.h"

#include "LifetimeTypes.h"
#include "FunctionLifetimes.h"

namespace clang {

void LifetimeAnnotationsChecker::GetLifetimes(FunctionDecl* func) {
  debug("Found function");
  debug(func->getNameAsString());

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
    debug("Function is not defined");
    func->dump();
  }

  // TODO ellision

  // Following Case 2. Not part of a cycle.
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func /* , symbol_table */);
    FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  // TODO keep track of analyzed functions
}
}  // namespace clang
