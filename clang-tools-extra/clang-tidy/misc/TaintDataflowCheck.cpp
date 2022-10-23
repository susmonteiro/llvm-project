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

// TODO understand why this is needed and change name of ValueLattice if it is
// not needed using TaintDataflowLattice = VarMapLattice<ValueLattice>;

constexpr char kVar[] = "var";
constexpr char kAssignment[] = "assignment";
constexpr char kRHS[] = "rhs";
constexpr char kTaint[] = "taint";
constexpr char kCleanup[] = "cleanup";
constexpr char readInput[] = "readInput";
constexpr char cleanup[] = "cleanup";


auto refToVar() { return declRefExpr(to(varDecl().bind(kVar))); }

auto funcCall(const char name[]) { return callExpr(callee(functionDecl(hasName(name)))); }


class TaintDataflowAnalysis
    : public DataflowAnalysis<TaintDataflowAnalysis, ValueLattice> {
public:
  explicit TaintDataflowAnalysis(ASTContext &Context)
      : DataflowAnalysis<TaintDataflowAnalysis, ValueLattice>(Context) {}

  static ValueLattice initialElement() { return ValueLattice::bottom(); }

  void transfer(const CFGElement *E, ValueLattice &Vars, Environment &Env) {
    auto CS = E->getAs<CFGStmt>();
    if (!CS)
      return;
    auto S = CS->getStmt();
    auto matcher = stmt(binaryOperator(hasOperatorName("="), hasLHS(refToVar()),
                                       hasRHS(anyOf(funcCall("readInput").bind(kTaint), funcCall("cleanup").bind(kCleanup), expr().bind(kRHS))))
                            .bind(kAssignment));

    // find any assignment
    // m stmt(binaryOperator(hasOperatorName("="), hasLHS(declRefExpr(to(varDecl()))), hasRHS(expr())))

    // find any assignment with readInput() dirty function
    // m stmt(binaryOperator(hasOperatorName("="), hasLHS(declRefExpr(to(varDecl()))), hasRHS(callExpr(callee(functionDecl(hasName("readInput")))))))

    // find any assignment with cleanup() clean function
    // m stmt(binaryOperator(hasOperatorName("="), hasLHS(declRefExpr(to(varDecl()))), hasRHS(callExpr(callee(functionDecl(hasName("cleanup")))))))

    // m stmt(binaryOperator(hasOperatorName("="), hasLHS(declRefExpr(to(varDecl()))), hasRHS(anyOf(expr(), callExpr(callee(functionDecl(hasName("readInput"))))))))

    ASTContext &Context = getASTContext();
    auto Results = match(matcher, *S, Context);
    if (Results.empty()) {
      cout << "> There are no results for the matcher" << endl;

      return;
    }

    


    const BoundNodes &Nodes = Results[0];

    if (const auto *CallExpression = Nodes.getNodeAs<CallExpr>(kTaint)) {
      cout << "> Found a TAINT call expression match" << endl;
      
    } else if (const auto *CallExpression = Nodes.getNodeAs<CallExpr>(kCleanup)){
      cout << "> Found a CLEAN call expression match" << endl;
    } else {
      cout << "> Found a expression match" << endl;
      const auto *Assignment = Nodes.getNodeAs<BinaryOperator>(kAssignment);

      if (Assignment != nullptr) {
        const auto *E = Nodes.getNodeAs<Expr>(kRHS);
        assert(E != nullptr);
      }
    }

    

  }
};

class TaintDataflowDiagnoser {
public:
  // ? erased "options"
  TaintDataflowDiagnoser() : DiagnoseMatchSwitch() {}

  std::vector<SourceLocation> diagnose(ASTContext &Ctx, const CFGElement *Elt,
                                       const Environment &Env) {
    return DiagnoseMatchSwitch(*Elt, Ctx, Env);
  }

private:
  CFGMatchSwitch<const Environment, std::vector<SourceLocation>>
      DiagnoseMatchSwitch;
};

// } // namespace
} // namespace dataflow

namespace tidy {
namespace misc {
using ast_matchers::MatchFinder;
// TODO change/remove me
using dataflow::TaintDataflowAnalysis;
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


  cout << "\tWill run dataflow analysis now. Everything should be fine up until this point." << endl;


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
      diag(Loc, "unchecked access to optional value");
}

} // namespace misc
} // namespace tidy
} // namespace clang
