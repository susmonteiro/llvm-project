//===--- TaintWithoutCleanupCheck.cpp - clang-tidy ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TaintWithoutCleanupCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void TaintWithoutCleanupCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(binaryOperator(
    isExpansionInMainFile(),hasOperatorName("="),hasRHS(
      callExpr(callee(functionDecl(hasName("clean"))))
    )).bind("clean"), this);

      

  // find var usages
}

void TaintWithoutCleanupCheck::check(const MatchFinder::MatchResult &Result) {
  // FIXME: Add callback implementation.
  // if function cleanup
    // mark variable as safe
  // if var usage
    // check if safe
    // if not, show warn
    // if it is, do nothing

  // TODO change this
  const auto *bopNode = Result.Nodes.getNodeAs<BinaryOperator>("clean");
  // get the left operand
  const auto lhs = bopNode->getLHS()->getBeginLoc();
  auto start = bopNode->getBeginLoc();
  // auto Diag = diag(start, "found the clean function");
  auto Diag = diag(lhs, "this var should be clean");

  // if (!MatchedDecl->getIdentifier() || MatchedDecl->getName().startswith("awesome_"))
  //   return;
  // diag(MatchedDecl->getLocation(), "function %0 is insufficiently awesome")
  //     << MatchedDecl;
  // diag(MatchedDecl->getLocation(), "insert 'awesome'", DiagnosticIDs::Note)
  //     << FixItHint::CreateInsertion(MatchedDecl->getLocation(), "awesome_");
}

} // namespace misc
} // namespace tidy
} // namespace clang
