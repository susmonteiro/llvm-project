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

  // TODO understand why this is needed and change name of ValueLattice if it is not needed
  // using TaintDataflowLattice = VarMapLattice<ValueLattice>;

  constexpr char kVar[] = "var";
  constexpr char kAssignment[] = "assignment";
  constexpr char kRHS[] = "rhs";


  auto refToVar() { return declRefExpr(to(varDecl().bind(kVar))); }

  class TaintDataflowAnalysis
    : public DataflowAnalysis<TaintDataflowAnalysis,
                              ValueLattice> {
    public:
      explicit TaintDataflowAnalysis(ASTContext &Context)
          : DataflowAnalysis<TaintDataflowAnalysis,
                            ValueLattice>(Context) {}

      static ValueLattice initialElement() {
        return ValueLattice::bottom();
      }

      void transfer(const CFGElement *E, ValueLattice &Vars,
                    Environment &Env) {
        auto CS = E->getAs<CFGStmt>();
        if (!CS)
          return;
        auto S = CS->getStmt();
        auto matcher =
            stmt(binaryOperator(hasOperatorName("="), hasLHS(refToVar()),
                                      hasRHS(expr().bind(kRHS)))
                          .bind(kAssignment));


    ASTContext &Context = getASTContext();
    auto Results = match(matcher, *S, Context);
    if (Results.empty())
      return;
    // const BoundNodes &Nodes = Results[0];

    // const auto *Var = Nodes.getNodeAs<clang::VarDecl>(kVar);
    // assert(Var != nullptr);

    // if (Nodes.getNodeAs<clang::VarDecl>(kDecl) != nullptr) {
    //   if (const auto *E = Nodes.getNodeAs<clang::Expr>(kInit)) {
    //     Expr::EvalResult R;
    //     Vars[Var] = (E->EvaluateAsInt(R, Context) && R.Val.isInt())
    //                     ? ValueLattice(R.Val.getInt().getExtValue())
    //                     : ValueLattice::top();
    //   } else {
    //     // An unitialized variable holds *some* value, but we don't know what it
    //     // is (it is implementation defined), so we set it to top.
    //     Vars[Var] = ValueLattice::top();
    //   }
    // } else if (Nodes.getNodeAs<clang::Expr>(kJustAssignment)) {
    //   const auto *E = Nodes.getNodeAs<clang::Expr>(kRHS);
    //   assert(E != nullptr);

    //   Expr::EvalResult R;
    //   Vars[Var] = (E->EvaluateAsInt(R, Context) && R.Val.isInt())
    //                   ? ValueLattice(R.Val.getInt().getExtValue())
    //                   : ValueLattice::top();
    // } else if (Nodes.getNodeAs<clang::Expr>(kAssignment)) {
    //   // Any assignment involving the expression itself resets the variable to
    //   // "unknown". A more advanced analysis could try to evaluate the compound
    //   // assignment. For example, `x += 0` need not invalidate `x`.
    //   Vars[Var] = ValueLattice::top();
    // }
  }
};


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
