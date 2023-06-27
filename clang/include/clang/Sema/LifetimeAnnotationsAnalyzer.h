#ifndef LIFETIME_ANNOTATIONS_ANALYZER_H_
#define LIFETIME_ANNOTATIONS_ANALYZER_H_

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

class LifetimeAnnotationsAnalyzer {
 public:
  LifetimeAnnotationsAnalyzer(Sema &s) : S(s) {}
  void GetLifetimes(const FunctionDecl *func);
  void AnalyzeFunctionBody(const clang::FunctionDecl *func);
  void GetLifetimeDependencies(const clang::FunctionDecl *func);
  void PropagateLifetimes();
  void CheckLifetimes(const clang::FunctionDecl *func);

  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &GetFunctionInfo() {
    return FunctionInfo;
  }

  std::string DebugString() {
    std::string res;
    res += "[LifetimeAnnotationsAnalyzer]: Functions Information\n";
    for (auto &pair : FunctionInfo) {
      res += "Function " + pair.first->getNameAsString() + '\n' +
             pair.second.DebugString() + "\n\n============\n\n";
    }
    return res;
  }

 private:
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> FunctionInfo;
  Sema &S;
  LifetimeAnnotationsAnalysis State;
};

}  // namespace clang

#endif
