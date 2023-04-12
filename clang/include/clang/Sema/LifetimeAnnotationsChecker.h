#ifndef LIFETIME_ANNOTATIONS_CHECKER_H_
#define LIFETIME_ANNOTATIONS_CHECKER_H_

#include <iostream>
#include <string>
#include <vector>

#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/Sema.h"

// DEBUG
#include "clang/AST/ASTContext.h"
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

struct FunctionInfo {
  std::vector<Lifetime> param_lifetimes;
  std::optional<Lifetime> return_lifetime;
  const clang::FunctionDecl *func;
};

class LifetimeAnnotationsChecker {
 public:
  // TODO probably not void
  void GetLifetimes(const FunctionDecl *func, Sema &S);

  void AnalyzeFunctionBody(const clang::FunctionDecl *func, Sema &S);

  void PropagateLifetimes();
  void CheckLifetimes();

  // TODO this should hold the lifetimes of the parameters and return values for
  // all functions
  std::vector<FunctionInfo> function_info_;
};
}  // namespace clang

#endif