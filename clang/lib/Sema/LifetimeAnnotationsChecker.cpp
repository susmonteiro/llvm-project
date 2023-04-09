#include "LifetimeAnnotationsChecker.h"

#include <iostream>

#include "FunctionLifetimes.h"
#include "LifetimeSymbolTable.h"
#include "LifetimeTypes.h"
#include "llvm/Support/Error.h"

namespace clang {

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
    debugLifetimes("Function is not defined");
    func->dump();
  }

  // TODO correct?
  LifetimeSymbolTable symbol_table;

  // TODO ellision

  // Following Case 2. Not part of a cycle.
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func, symbol_table);
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

  debugLifetimes("Now should have all lifetimes");
  func_lifetimes->DumpParameters();


  // debugLifetimes("Name to lifetimes");
  // const auto names_to_lifetimes = symbol_table.GetMapping();

  // for (const auto &pair : names_to_lifetimes) {
  //   std::cout << pair.getKey().str() << ": " << pair.getValue() << '\n';
  // }

  // TODO keep track of analyzed functions
}
}  // namespace clang
