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
#include "clang/Lex/Lexer.h"


using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void TaintWithoutCleanupCheck::registerMatchers(MatchFinder *Finder) {

  // implement checker only for cpp
  if (!getLangOpts().CPlusPlus)
    return;


  Finder->addMatcher(binaryOperator(
    isExpansionInMainFile(),hasOperatorName("="),hasRHS(
      callExpr(callee(functionDecl(hasName("dirty"))))
    )).bind("dirty"), this);

      

  // find var usages
}

void TaintWithoutCleanupCheck::check(const MatchFinder::MatchResult &Result) {
  // if (const auto *bopNode = Result.Nodes.getNodeAs<BinaryOperator>("dirty")) {
  //   // const auto lhs = bopNode->getLHS();
  //   FoundDecls[bopNode] = CharSourceRange::getCharRange(
  //       bopNode->getLHS()->getBeginLoc(),
  //       Lexer::findLocationAfterToken(
  //           bopNode->getLHS()->getEndLoc(), tok::semi, *Result.SourceManager,
  //           getLangOpts(),
  //           /*SkipTrailingWhitespaceAndNewLine=*/true));
  //   return;
  // }

  // TODO change this
  const auto *bopNode = Result.Nodes.getNodeAs<BinaryOperator>("dirty");
  // get the left operand
  const auto lhs = bopNode->getLHS()->getBeginLoc();
  // auto start = bopNode->getBeginLoc();
  // auto Diag = diag(lhs, "variable is dirty");
  auto Diag = diag(lhs, "variable %0 is dirty")
        << bopNode->getLHS();

  // if (!MatchedDecl->getIdentifier() || MatchedDecl->getName().startswith("awesome_"))
  //   return;
  // diag(MatchedDecl->getLocation(), "function %0 is insufficiently awesome")
  //     << MatchedDecl;
  // diag(MatchedDecl->getLocation(), "insert 'awesome'", DiagnosticIDs::Note)
  //     << FixItHint::CreateInsertion(MatchedDecl->getLocation(), "awesome_");
}

// void TaintWithoutCleanupCheck::onEndOfTranslationUnit() {
//   for (const auto &FoundDecl : FoundDecls) {
//     if (FoundDecl.second.isValid()) {
//       diag(FoundDecl.second->getLocation(), "variable %0 is dirty, should not be used")
//         << FoundDecl.first;
//         return;
//     }
//   }
// }

} // namespace misc
} // namespace tidy
} // namespace clang
