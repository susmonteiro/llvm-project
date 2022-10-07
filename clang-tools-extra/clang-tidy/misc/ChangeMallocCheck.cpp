//===--- ChangeMallocCheck.cpp - clang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ChangeMallocCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void ChangeMallocCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(callExpr(callee(functionDecl(hasName("malloc")))).bind("malloc"), this);
  Finder->addMatcher(callExpr(callee(functionDecl(hasName("free")))).bind("free"), this);
}

void ChangeMallocCheck::check(const MatchFinder::MatchResult &Result) {
  SmallString<64> NewArgument;
  // malloc
  const CallExpr *callExpr = Result.Nodes.getNodeAs<CallExpr>("malloc");
  if (callExpr) {
    auto start = callExpr->getBeginLoc();
    auto Diag = diag(start, "use acme_zalloc() instead of malloc()")
      << FixItHint::CreateReplacement(SourceRange(start, start.getLocWithOffset(strlen("malloc") - 1)),
      "acme_zalloc");
      // add a second parameter to malloc
    NewArgument = Twine(", ZERO_INITIALIZE").str();
    const auto InsertNewArgument = FixItHint::CreateInsertion(callExpr->getEndLoc(), NewArgument);
    Diag << InsertNewArgument;
  }

  // free
  callExpr = Result.Nodes.getNodeAs<CallExpr>("free");
  if (callExpr) {
    auto start = callExpr->getBeginLoc();
    auto Diag = diag(start, "use acme_free() instead of free()")
      << FixItHint::CreateReplacement(SourceRange(start, start.getLocWithOffset(strlen("free") - 1)),
      "acme_free");
    Diag << FixItHint::CreateInsertion(callExpr->getArg(0)->getBeginLoc(), "(void **)&");
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
