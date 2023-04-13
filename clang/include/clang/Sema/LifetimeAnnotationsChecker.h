#ifndef LIFETIME_ANNOTATIONS_CHECKER_H_
#define LIFETIME_ANNOTATIONS_CHECKER_H_

#include <iostream>
#include <string>
#include <vector>

#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/DenseMap.h"
// TODO uncomment
// #include "clang/Sema/LifetimeAnnotationsAnalysis.h"

// DEBUG
#include "clang/AST/ASTContext.h"
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

class LifetimeAnnotationsChecker {
 public:
 // TODO remove this
  // void InsertInMap(const FunctionDecl *func, int i) {
  //     debug_map_[func] = 1;
  // }

  void GetLifetimes(const FunctionDecl *func, Sema &S);
  void AnalyzeFunctionBody(const clang::FunctionDecl *func, Sema &S);
  void PropagateLifetimes();
  void CheckLifetimes();

  private:

  // llvm::DenseMap<const clang::FunctionDecl*, FunctionLifetimesOrError> function_info_;
  // FIXME 
  llvm::DenseMap<const clang::FunctionDecl*, FunctionLifetimes> function_info_;
  // llvm::DenseMap<const clang::FunctionDecl*, int> debug_map_;
  // llvm::DenseMap<int, int> whatever_tmp_;
  // llvm::DenseSet<FunctionLifetimes> function_info_;
  // TODO
  // LifetimeAnnotationsAnalysis state_;
};

}  // namespace clang

#endif