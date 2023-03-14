//===--- LifetimeAnnotationsCheck.cpp - clang-tidy ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <iostream>

#include "LifetimeAnnotationsAnalysis/LifetimeAnnotationsAnalysis.h"
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

void AnalyzeFunctionBody(const clang::FunctionDecl *func) {
  // TODO do I need this?
  // clang::dataflow::DataflowAnalysisContext analysis_context(
  //     std::make_unique<clang::dataflow::WatchedLiteralsSolver>());
  // clang::dataflow::Environment environment(analysis_context);

  // TODO initialize analysis correctly
  // LifetimeAnnotationsAnalysis analysis(func);
}

void AnalyzeFunction(const clang::FunctionDecl *func) {
  // Make sure we're always using the canonical declaration when using the
  // function as a key in maps and sets.
  // ? what is the canonical function declaration
  func = func->getCanonicalDecl();

  // TODO necessary?
  // if (func->getBuiltinID() != 0) {
  //   return;
  // }

  if (func->getBody()) {
    AnalyzeFunctionBody(func);
  }

  // TODO take care of callees -> probably make recursive
  // TODO:
  // compare with AnalyzeFunctionRecursive from crubit to see if we have all the
  // necessary checks
}

void LifetimeAnnotationsCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(functionDecl().bind(FuncID), this);
}

void LifetimeAnnotationsCheck::check(const MatchFinder::MatchResult &Result) {
  if (Result.SourceManager->getDiagnostics().hasUncompilableErrorOccurred())
    return;

  const auto *FuncDecl = Result.Nodes.getNodeAs<FunctionDecl>(FuncID);
  if (FuncDecl->isTemplated())
    return; // TODO implement this

  AnalyzeFunction(FuncDecl);

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
