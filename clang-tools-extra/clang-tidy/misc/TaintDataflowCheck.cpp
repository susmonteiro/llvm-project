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
#include <vector>

// TODO remove me
#include <iostream>

using namespace std;

static constexpr llvm::StringLiteral FuncID("fun");

namespace clang {
namespace dataflow {
using namespace ast_matchers;

struct ValueLattice {

  enum class ValueState : bool {
    Taint,
    Clean,
  };

  ValueState State;

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

    cout << "Join function called!" << endl;

    if (*this == clean() && Other == taint()) {
      *this = bottom();
      return LatticeJoinEffect::Changed;
    }

    return LatticeJoinEffect::Unchanged;
  }
};

using TaintDataflowLattice = VarMapLattice<ValueLattice>;

constexpr char kVar[] = "var";
constexpr char kVarRHS[] = "varRHS";
constexpr char kAssignment[] = "assignment";
constexpr char kRHS[] = "rhs";
constexpr char kExpr[] = "expr";
constexpr char kTaint[] = "taint";
constexpr char kCleanup[] = "cleanup";
constexpr char readInput[] = "readInput";
constexpr char cleanup[] = "cleanup";
constexpr char kCallCritical[] = "critical";

auto refToVar() { return declRefExpr(to(varDecl().bind(kVar))); }

auto funcCall(const char name[]) {
  return callExpr(callee(functionDecl(hasName(name))));
}

auto usingVar() { return declRefExpr(to(varDecl().bind(kVarRHS))); }

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
        stmt(binaryOperator(
                 hasOperatorName("="), hasLHS(refToVar()),
                 hasRHS(stmt(eachOf(findAll(funcCall("readInput").bind(kTaint)),
                                    findAll(funcCall("cleanup").bind(kCleanup)),
                                    expr()))
                            .bind("stmt")))
                 .bind(kAssignment));

    ASTContext &Context = getASTContext();
    auto Results = match(matcher, *S, Context);
    if (Results.empty()) {
      return;
    }

    const BoundNodes &Nodes = Results[0];

    const auto *Var = Nodes.getNodeAs<clang::VarDecl>(kVar);
    assert(Var != nullptr);

    // const auto *stmt = Nodes.getNodeAs<Stmt>("stmt");
    // stmt->viewAST();

    if (Nodes.getNodeAs<CallExpr>(kTaint)) {
      Vars[Var] = ValueLattice::taint();
    } else if (Nodes.getNodeAs<CallExpr>(kCleanup)) {
      Vars[Var] = ValueLattice::clean();
    } else {
      cout << "HELLOOOO" << endl;
      const auto *S = Nodes.getNodeAs<Stmt>("stmt");
      assert(S != nullptr);

      // const auto *stmt = Nodes.getNodeAs<Stmt>("stmt");
      // stmt->viewAST();

      ValueLattice L = ValueLattice(ValueLattice::clean());

      for (const auto *s : S->children()) {
        cout << s->getStmtClassName() << endl;
        const auto stmtClass = s->getStmtClass();
        const auto *E = dyn_cast<Expr>(s);
        assert(E != nullptr);

        // integers are clean
        if (dyn_cast<IntegerLiteral>(E))
          continue;

        if (dyn_cast<CallExpr>(E)) {
          // TODO ?
          L = ValueLattice::taint();
        }

        if (const auto *ICE = E->getReferencedDeclOfCallee()) {
          cout << "YESSSSSSSSSSSSSS!" << endl;

          if (const auto *VD = dyn_cast<VarDecl>(ICE)) {
            L.join(Vars[VD]);
          }
        }
      }
      Vars[Var] = L;
    }
  }
};

class TaintDataflowDiagnoser {
public:
  TaintDataflowDiagnoser() {}

  std::vector<SourceLocation> diagnose(ASTContext &Ctx, const CFGElement *E,
                                       const Environment &Env,
                                       const TaintDataflowLattice &Vars) {
    auto CS = E->getAs<CFGStmt>();
    if (!CS)
      return {};
    auto S = CS->getStmt();
    auto matcher = stmt(callExpr(callee(functionDecl(hasName("critical"))))
                            .bind(kCallCritical));

    auto Results = match(matcher, *S, Ctx);

    if (Results.empty())
      return {};

    const BoundNodes &Nodes = Results[0];

    if (const auto *CE = Nodes.getNodeAs<CallExpr>(kCallCritical)) {
      if (CE->getNumArgs() != 1)
        return {};

      const auto *Arg = CE->getArg(0);

      if (const auto *Decl = Arg->getReferencedDeclOfCallee()) {
        // check if it is a variable
        if (const auto *ArgDecl = dyn_cast<VarDecl>(Decl)) {

          if (Vars.contains(ArgDecl) &&
              Vars.lookup(ArgDecl) == ValueLattice::taint()) {

            return {CE->getBeginLoc()};
          }
        }
      }
    }
    return {};
  }
};
} // namespace dataflow

namespace tidy {
namespace misc {
using ast_matchers::MatchFinder;
using dataflow::TaintDataflowAnalysis;
using dataflow::TaintDataflowDiagnoser;
using dataflow::ValueLattice;
using llvm::Optional;

static Optional<std::vector<SourceLocation>>
analyzeCode(const FunctionDecl &FuncDecl, ASTContext &ASTCtx) {

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
  TaintDataflowDiagnoser Diagnoser;
  std::vector<SourceLocation> Diagnostics;

  Expected<std::vector<
      Optional<DataflowAnalysisState<TaintDataflowAnalysis::Lattice>>>>
      BlockToOutputState = dataflow::runDataflowAnalysis(
          *Context, Analysis, Env,
          [&ASTCtx, &Diagnoser, &Diagnostics](
              const CFGElement &Elt,
              const DataflowAnalysisState<TaintDataflowAnalysis::Lattice>
                  &State) mutable {
            auto EltDiagnostics =
                Diagnoser.diagnose(ASTCtx, &Elt, State.Env, State.Lattice);
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

  // TODO check matcher
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
