#ifndef LIFETIME_ANNOTATIONS_CHECKER_H_
#define LIFETIME_ANNOTATIONS_CHECKER_H_

#include <iostream>
#include <string>
#include "clang/Sema/Sema.h"

// TODO remove this
#include "DebugLifetimes.h"

#include "clang/AST/ASTContext.h"

namespace clang {

class LifetimeAnnotationsChecker {
 public:


  // TODO probably not void
  void GetLifetimes(const FunctionDecl* func, Sema &S);

  llvm::Error AnalyzeFunctionBody(const clang::FunctionDecl *func, clang::ASTContext &ast_context);

  void PropagateLifetimes();
  void CheckLifetimes();
};
}

#endif