//===--- TaintDataflowCheck.cpp - clang-tidy ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TaintDataflowCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Analysis/CFG.h"
#include "clang/Analysis/FlowSensitive/ControlFlowContext.h"
#include "clang/Analysis/FlowSensitive/DataflowAnalysisContext.h"
#include "clang/Analysis/FlowSensitive/DataflowEnvironment.h"
#include "clang/Analysis/FlowSensitive/DataflowLattice.h"
#include "clang/Analysis/FlowSensitive/Models/UncheckedOptionalAccessModel.h"
#include "clang/Analysis/FlowSensitive/WatchedLiteralsSolver.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/Any.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Error.h"
#include <memory>
#include <vector>


namespace clang {
namespace dataflow {
namespace {
  using namespace ast_matchers;

  struct ValueLattice {

    enum class ValueState : bool {
      Taint,
      Clean,
    };

    ValueState State;

    // ? do I need this
    // When `None`, the lattice is either at top or bottom, based on `State`.
    // llvm::Optional<int64_t> Value;

    constexpr ValueLattice() : State(ValueState::Taint) {}
    constexpr ValueLattice(ValueState S) : State(S) {}

    static constexpr ValueLattice bottom() {
      return ValueLattice(ValueState::Taint);
    }

    friend bool operator==(const ValueLattice &Lhs, const ValueLattice &Rhs) {
      return Lhs.State == Rhs.State;
    }

    friend bool operator!=(const ValueLattice &Lhs, const ValueLattice &Rhs) {
      return !(Lhs == Rhs);
    }

    // ? unsure about this join function
    LatticeJoinEffect join(const ValueLattice &Other) {
      if (this->State != ValueState::Clean && *this != Other) {
        return LatticeJoinEffect::Unchanged;
      }
        
      *this = bottom();
      return LatticeJoinEffect::Changed;
    }
  };

  // using TaintDataflowLattice = VarMapLattice<ValueLattice>;


}
} // namespace dataflow

namespace tidy {
namespace misc {
using ast_matchers::MatchFinder;




/* static std::vector<SourceLocation> analyzeVariable(const VarDecl &VarDecl, ASTContext &ASTCtx) {
  

} */



void TaintDataflowCheck::registerMatchers(MatchFinder *Finder) {
  using namespace ast_matchers;

  /* Finder->addMatcher(binaryOperator(
    isExpansionInMainFile(),hasOperatorName("="),hasRHS(
      callExpr(callee(functionDecl(hasName("dirty"))))
    )).bind("dirty_assign"), this); */

  Finder->addMatcher(callExpr(callee(functionDecl(hasName("critical"))))
    .bind("call_critical"), this);


}

void TaintDataflowCheck::check(const MatchFinder::MatchResult &Result) {
  if (Result.SourceManager->getDiagnostics().hasUncompilableErrorOccurred())
    return;

  // TODO delete me
  if (const auto *bopNode = Result.Nodes.getNodeAs<CallExpr>("call_critical")) {
    auto Diag = diag(bopNode->getBeginLoc(), "found call to critical function");
  }  
}


} // namespace misc
} // namespace tidy
} // namespace clang
