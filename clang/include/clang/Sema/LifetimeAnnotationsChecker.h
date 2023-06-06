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
#include "clang/Sema/PointsToMap.h"
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

  std::string DebugString() {
    std::string res;
    res += "[LifetimeAnnotationsChecker]: Functions Information\n";
    for (auto &pair : FunctionInfo) {
      res += "Function " + pair.first->getNameAsString() + '\n' +
             pair.second.DebugString() + "\n\n============\n\n";
    }
    return res;
  }

 private:
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> FunctionInfo;
  LifetimeAnnotationsAnalysis State;
};

}  // namespace clang

#endif
