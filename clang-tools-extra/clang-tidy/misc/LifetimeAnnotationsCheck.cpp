//===--- LifetimeAnnotationsCheck.cpp - clang-tidy ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <iostream>

#include "LifetimeAnnotationsCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

static constexpr llvm::StringLiteral FuncID("fun");

namespace clang {

using namespace ast_matchers;

// DEBUG
void debug(std::string text) {
  std::cout << "\033[1;33m>> \033[0m" << text << std::endl;
}

namespace dataflow {
// TODO
}

namespace tidy {
namespace misc {

void LifetimeAnnotationsCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(functionDecl().bind(FuncID), this);
}

void LifetimeAnnotationsCheck::check(const MatchFinder::MatchResult &Result) {
  if (Result.SourceManager->getDiagnostics().hasUncompilableErrorOccurred())
    return;

  const auto *FuncDecl = Result.Nodes.getNodeAs<FunctionDecl>(FuncID);
  if (FuncDecl->isTemplated())
    return; // TODO implement this

  // if (Optional<std::vector<SourceLocation>> Errors =
  //         analyzeCode(*FuncDecl, *Result.Context))
  // for (const SourceLocation &Loc : *Errors) {
  //   // TODO create diagnoser and use it here
  // }
  debug("Found a function");
}

} // namespace misc
} // namespace tidy
} // namespace clang
