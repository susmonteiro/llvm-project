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

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

// DEBUG
void debug(std::string text) {
  std::cout << "\033[1;33m>> \033[0m" << text << std::endl;
}

void LifetimeAnnotationsCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(functionDecl().bind("fun"), this);
}

void LifetimeAnnotationsCheck::check(const MatchFinder::MatchResult &Result) {
  debug("Found a function");
}

} // namespace misc
} // namespace tidy
} // namespace clang
