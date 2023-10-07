#ifndef LIFETIME_ANNOTATIONS_ANALYZER_H_
#define LIFETIME_ANNOTATIONS_ANALYZER_H_

#include <iostream>
#include <string>
#include <vector>

#include "clang/AST/Decl.h"
#include "clang/Sema/LifetimeAnnotationsAnalysis.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/DenseMap.h"

namespace clang {

class LifetimeAnnotationsAnalyzer {
 public:
  LifetimeAnnotationsAnalyzer(Sema &s) : S(s) {}
  void GetLifetimes(const FunctionDecl *func);
  void AnalyzeFunctionBody(const clang::FunctionDecl *func);
  void GetLifetimeDependencies(const clang::FunctionDecl *func);
  void PropagateLifetimes();
  void CheckLifetimes(const clang::FunctionDecl *func);

  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &
  GetFunctionInfo() {
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

  std::string DebugDependencies(
      const clang::VarDecl *var, unsigned int num_indirections,
      llvm::DenseSet<RHSTypeStruct> var_stmt,
      llvm::DenseMap<const clang::Stmt *,
                     llvm::DenseSet<const clang::VarDecl *>>
          stmt_var) {
    std::string res;
    res += "Dependencies of " + var->getNameAsString() + ": ";
    res += "[Type]: " + std::string(num_indirections, '*') + "\t[vars]: ";
    for (const auto &info : var_stmt) {
      for (const auto &rhs_var : stmt_var[info.stmt]) {
        if (var == rhs_var) continue;
        res += "(" + std::string(info.num_indirections, '*') + ")" +
               rhs_var->getNameAsString() + ' ';
      }
    }
    res += '\n';
    return res;
  }

 private:
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> FunctionInfo;
  Sema &S;
  LifetimeAnnotationsAnalysis State;
};

}  // namespace clang

#endif
