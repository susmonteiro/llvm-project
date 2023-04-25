#ifndef LIFETIME_ANNOTATIONS_CHECKER_H_
#define LIFETIME_ANNOTATIONS_CHECKER_H_

#include <iostream>
#include <string>
#include <vector>

#include "clang/AST/Decl.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/LifetimeAnnotationsAnalysis.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/DenseMap.h"

// DEBUG
#include "clang/AST/ASTContext.h"
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

class LifetimeAnnotationsChecker {
 public:
  void GetLifetimes(const FunctionDecl *func, Sema &S);
  void AnalyzeFunctionBody(const clang::FunctionDecl *func, Sema &S);
  void GetLifetimeDependencies(const clang::FunctionDecl *func);
  void PropagateLifetimes();
  void CheckLifetimes(const clang::FunctionDecl *func, Sema &S);

  void DumpFunctionInfo() const {
    debugLifetimes("[LifetimeAnnotationsChecker]: Functions Information");
    for (const auto &pair : function_info_) {
      debugLifetimes("Function", pair.first->getNameAsString());
      debugLifetimes(pair.second.DebugString());
      debugLifetimes("============");
    }
  }

 private:
  // TODO maybe need to store the case where there are errors?
  // llvm::DenseMap<const clang::FunctionDecl*, FunctionLifetimesOrError>
  // function_info_;
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> function_info_;
  LifetimeAnnotationsAnalysis state_;
};

}  // namespace clang

#endif
