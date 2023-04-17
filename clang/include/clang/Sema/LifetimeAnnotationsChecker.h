#ifndef LIFETIME_ANNOTATIONS_CHECKER_H_
#define LIFETIME_ANNOTATIONS_CHECKER_H_

#include <iostream>
#include <string>
#include <vector>

#include "clang/AST/Decl.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/DenseMap.h"
#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

// DEBUG
#include "clang/AST/ASTContext.h"
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

class LifetimeAnnotationsChecker {
 public:
  void VisitVarDecl(const clang::VarDecl *var_decl);

  void GetLifetimes(const FunctionDecl *func, Sema &S);
  void AnalyzeFunctionBody(const clang::FunctionDecl *func, Sema &S);
  void GetLifetimeDependencies(const clang::Stmt *functionBody,
                               clang::ASTContext &Context,
                               const clang::FunctionDecl *func,
                               FunctionLifetimes &func_info);
  void PropagateLifetimes();
  void CheckLifetimes();

  void DumpFunctionInfo() const {
    debugLifetimes("[LifetimeAnnotationsChecker]: Functions Information");
    for (const auto &pair : function_info_) {
      debugLifetimes("Function", pair.first->getNameAsString());
      pair.second.DumpParameters();
      pair.second.DumpReturn();
      debugLifetimes("============");
    }
  }

 private:
  // TODO maybe need to store the case where there are errors?
  // llvm::DenseMap<const clang::FunctionDecl*, FunctionLifetimesOrError>
  // function_info_;
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> function_info_;
  // TODO
  LifetimeAnnotationsAnalysis state_;
};

}  // namespace clang

#endif