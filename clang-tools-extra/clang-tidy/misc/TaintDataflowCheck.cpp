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
#include "clang/Analysis/FlowSensitive/MapLattice.h"
#include "clang/Analysis/FlowSensitive/WatchedLiteralsSolver.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/Any.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Error.h"
#include <memory>
#include <vector>

// TODO remove me
#include "clang/Analysis/FlowSensitive/Models/UncheckedOptionalAccessModel.h"
#include <iostream>

using namespace std;

static constexpr llvm::StringLiteral FuncID("fun");

namespace clang {
namespace dataflow {
// namespace {
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

  static constexpr ValueLattice taint() {
    return ValueLattice(ValueState::Taint);
  }

  static constexpr ValueLattice clean() {
    return ValueLattice(ValueState::Clean);
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

using TaintDataflowLattice = VarMapLattice<ValueLattice>;

// TODO understand why this is needed and change name of ValueLattice if it is
// not needed using TaintDataflowLattice = VarMapLattice<ValueLattice>;

constexpr char kVar[] = "var";
constexpr char kAssignment[] = "assignment";
constexpr char kRHS[] = "rhs";
constexpr char kTaint[] = "taint";
constexpr char kCleanup[] = "cleanup";
constexpr char readInput[] = "readInput";
constexpr char cleanup[] = "cleanup";
constexpr char kCallCritical[] = "critical";

auto refToVar() { return declRefExpr(to(varDecl().bind(kVar))); }

auto funcCall(const char name[]) {
  return callExpr(callee(functionDecl(hasName(name))));
}

class TaintDataflowAnalysis
    : public DataflowAnalysis<TaintDataflowAnalysis, TaintDataflowLattice> {
public:
  explicit TaintDataflowAnalysis(ASTContext &Context)
      : DataflowAnalysis<TaintDataflowAnalysis, TaintDataflowLattice>(Context) {
  }

  static TaintDataflowLattice initialElement() {
    return TaintDataflowLattice::bottom();
  }

  void transfer(const CFGElement *E, TaintDataflowLattice &Vars,
                Environment &Env) {
    auto CS = E->getAs<CFGStmt>();
    if (!CS)
      return;
    auto S = CS->getStmt();
    auto matcher =
        stmt(binaryOperator(hasOperatorName("="), hasLHS(refToVar()),
                            hasRHS(anyOf(funcCall("readInput").bind(kTaint),
                                         funcCall("cleanup").bind(kCleanup),
                                         expr().bind(kRHS))))
                 .bind(kAssignment));

    // find any assignment
    // m stmt(binaryOperator(hasOperatorName("="),
    // hasLHS(declRefExpr(to(varDecl()))), hasRHS(expr())))

    // find any assignment with readInput() dirty function
    // m stmt(binaryOperator(hasOperatorName("="),
    // hasLHS(declRefExpr(to(varDecl()))),
    // hasRHS(callExpr(callee(functionDecl(hasName("readInput")))))))

    // find any assignment with cleanup() clean function
    // m stmt(binaryOperator(hasOperatorName("="),
    // hasLHS(declRefExpr(to(varDecl()))),
    // hasRHS(callExpr(callee(functionDecl(hasName("cleanup")))))))

    // m stmt(binaryOperator(hasOperatorName("="),
    // hasLHS(declRefExpr(to(varDecl()))), hasRHS(anyOf(expr(),
    // callExpr(callee(functionDecl(hasName("readInput"))))))))

    ASTContext &Context = getASTContext();
    auto Results = match(matcher, *S, Context);
    if (Results.empty()) {
      // cout << "> There are no results for the matcher" << endl;

      return;
    }

    const BoundNodes &Nodes = Results[0];

    const auto *Var = Nodes.getNodeAs<clang::VarDecl>(kVar);
    assert(Var != nullptr);

    if (Nodes.getNodeAs<CallExpr>(kTaint)) {
      cout << "> Found a TAINT call expression match" << endl;
      Vars[Var] = ValueLattice::taint();

    } else if (Nodes.getNodeAs<CallExpr>(kCleanup)) {
      cout << "> Found a CLEAN call expression match" << endl;
      Vars[Var] = ValueLattice::clean();

    } else {
      // cout << "> Found a expression match" << endl;

      // then set the left part of the variable to taint/clean depending on the
      // expr if any of the vars in the expression is taint not clean, then set
      // the lhs to taint beware of values (instead of vars). Those are always
      // clean

      // const auto *Assignment = Nodes.getNodeAs<BinaryOperator>(kAssignment);

      // if (Assignment != nullptr) {
      //   const auto *E = Nodes.getNodeAs<Expr>(kRHS);
      //   assert(E != nullptr);
      // }
      //
      // TODO for now do nothing
    }
  }
};

class TaintDataflowDiagnoser {
public:
  TaintDataflowDiagnoser() {}

  std::vector<SourceLocation> diagnose(ASTContext &Ctx, const CFGElement *E,
                                       const Environment &Env) {
    auto CS = E->getAs<CFGStmt>();
    if (!CS)
      return {};
    auto S = CS->getStmt();
    auto matcher = stmt(callExpr(callee(functionDecl(hasName("critical"))))
                            .bind(kCallCritical));

    m callExpr(callee(functionDecl(hasName("critical"))))
    m callExpr(callee(functionDecl(hasName("critical"))), )


    auto Results = match(matcher, *S, Ctx);

    if (Results.empty()) {
      cout << "> Did not find critical call" << endl;
      return {};
    } else {
      cout << "> Found critical call" << endl;
      const BoundNodes &Nodes = Results[0];

      if (const auto *CE = Nodes.getNodeAs<CallExpr>(kCallCritical)) {
        
        // const auto *Args = CE->getArgs();
        cout << "Could get CallExpr!!" << endl;
        return { CE->getBeginLoc() };
      } 
    }

    // TODO change this
    return {};
  }
};
} // namespace dataflow

namespace tidy {
namespace misc {
using ast_matchers::MatchFinder;
using dataflow::TaintDataflowAnalysis;
// TODO change/remove me
using dataflow::TaintDataflowDiagnoser;
using dataflow::ValueLattice;
using llvm::Optional;

static Optional<std::vector<SourceLocation>>
analyzeCode(const FunctionDecl &FuncDecl, ASTContext &ASTCtx) {

  cout << "Analyzing code..." << endl;

  using dataflow::ControlFlowContext;
  using dataflow::DataflowAnalysisState;
  using llvm::Expected;

  Expected<ControlFlowContext> Context =
      ControlFlowContext::build(&FuncDecl, FuncDecl.getBody(), &ASTCtx);
  if (!Context)
    return llvm::None;

  dataflow::DataflowAnalysisContext AnalysisContext(
      std::make_unique<dataflow::WatchedLiteralsSolver>());
  dataflow::Environment Env(AnalysisContext, FuncDecl);
  TaintDataflowAnalysis Analysis(ASTCtx);
  // TODO this
  TaintDataflowDiagnoser Diagnoser;
  std::vector<SourceLocation> Diagnostics;

  // cout << "\tWill run dataflow analysis now. Everything should be fine up
  // until this point." << endl;

  // TODO change function
  Expected<std::vector<
      Optional<DataflowAnalysisState<TaintDataflowAnalysis::Lattice>>>>
      BlockToOutputState = dataflow::runDataflowAnalysis(
          *Context, Analysis, Env,
          [&ASTCtx, &Diagnoser, &Diagnostics](
              const CFGElement &Elt,
              const DataflowAnalysisState<TaintDataflowAnalysis::Lattice>
                  &State) mutable {
            auto EltDiagnostics = Diagnoser.diagnose(ASTCtx, &Elt, State.Env);
            llvm::move(EltDiagnostics, std::back_inserter(Diagnostics));
          });
  if (!BlockToOutputState)
    return llvm::None;

  return Diagnostics;
}

void TaintDataflowCheck::registerMatchers(MatchFinder *Finder) {
  using namespace ast_matchers;

  /* Finder->addMatcher(binaryOperator(
    isExpansionInMainFile(),hasOperatorName("="),hasRHS(
      callExpr(callee(functionDecl(hasName("dirty"))))
    )).bind("dirty_assign"), this); */

  // Finder->addMatcher(
  //     callExpr(callee(functionDecl(hasName("critical")))).bind("call_critical"),
  //     this);

  cout << "Beginning..." << endl;

  auto HasOptionalCallDescendant =
      hasDescendant(callExpr(callee(functionDecl(hasName("critical")))));

  Finder->addMatcher(
      decl(anyOf(functionDecl(unless(isExpansionInSystemHeader()),
                              // FIXME: Remove the filter below when lambdas are
                              // well supported by the check.
                              unless(hasDeclContext(cxxRecordDecl(isLambda()))),
                              hasBody(HasOptionalCallDescendant)),
                 cxxConstructorDecl(hasAnyConstructorInitializer(
                     withInitializer(HasOptionalCallDescendant)))))
          .bind(FuncID),
      this);
}

void TaintDataflowCheck::check(const MatchFinder::MatchResult &Result) {
  if (Result.SourceManager->getDiagnostics().hasUncompilableErrorOccurred())
    return;

  cout << "Checking..." << endl;

  // ? we want the function where critical is called?
  // ? or do we want the critical call itself
  const auto *FuncDecl = Result.Nodes.getNodeAs<FunctionDecl>(FuncID);
  if (FuncDecl->isTemplated())
    return;

  if (Optional<std::vector<SourceLocation>> Errors =
          analyzeCode(*FuncDecl, *Result.Context))
    for (const SourceLocation &Loc : *Errors)
      diag(Loc, "calling critical function with taint value");
}

} // namespace misc
} // namespace tidy
} // namespace clang
