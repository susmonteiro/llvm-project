#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include <limits.h>

using namespace clang;
using namespace ento;

namespace {
class SimpleOverflowChecker : public Checker<check::PreStmt<BinaryOperator>> {
  mutable std::unique_ptr<BuiltinBug> BT;
  void reportBug(const char *Msg, ProgramStateRef StateZero, CheckerContext &C,
                 std::unique_ptr<BugReporterVisitor> Visitor = nullptr) const;

  public:
    void checkPreStmt(const BinaryOperator *B, CheckerContext &C) const;
  };
} // end anonymous namespace

// TODO change this?
static const Expr *getExpr(const ExplodedNode *N) {
  const Stmt *S = N->getLocationAs<PreStmt>()->getStmt();
  if (const auto *BE = dyn_cast<BinaryOperator>(S))
    return BE->getRHS();
  return nullptr;
}

void SimpleOverflowChecker::reportBug(
    const char *Msg, ProgramStateRef StateZero, CheckerContext &C,
    std::unique_ptr<BugReporterVisitor> Visitor) const {
  if (ExplodedNode *N = C.generateErrorNode(StateZero)) {
    if (!BT)
      BT.reset(new BuiltinBug(this, "Overflow"));

    auto R = std::make_unique<PathSensitiveBugReport>(*BT, Msg, N);
    R->addVisitor(std::move(Visitor));
    bugreporter::trackExpressionValue(N, getExpr(N), *R);
    C.emitReport(std::move(R));
  }
}

void SimpleOverflowChecker::checkPreStmt(const BinaryOperator *B,
                                         CheckerContext &C) const {
  BinaryOperator::Opcode Op = B->getOpcode();

  if (Op != BO_Add && Op != BO_AddAssign && 
       Op != BO_Sub && Op != BO_SubAssign /* && 
      Op != BO_Mul && Op != BO_MulAssign */) 
    return;
  
  if (!B->getRHS()->getType()->isScalarType())
    return;

  ConstraintManager &CM = C.getConstraintManager();
  ProgramStateRef stateNotOverflow, stateOverflow;

  SVal left = C.getSVal(B->getLHS());
  SVal right = C.getSVal(B->getRHS());
  
  // doesn't work
  // SVal isOverflow = SVB.evalBinOp(C.getState(), BO_LT, applyOperation, SVB.makeIntVal(INT_MAX, false), SVB.getConditionType());  

  SValBuilder &SVB = C.getSValBuilder();
  SVal finalExpression;

  if (Op == BO_Add || Op == BO_AddAssign) {
    // overflow if r > 0 && l > INT_MAX - r
    // r > 0
    SVal positive = SVB.evalBinOp(C.getState(), BO_GT, right, SVB.makeIntVal(0, false), SVB.getConditionType());
    // INT_MAX - r
    SVal applyOperation = SVB.evalBinOp(C.getState(), BO_Sub, SVB.makeIntVal(INT_MAX, false), right, SVB.getConditionType());
    // l > INT_MAX - r
    SVal compareOperation = SVB.evalBinOp(C.getState(), BO_GT, left, applyOperation, SVB.getConditionType());
    // r > 0 && l > INT_MAX - r
    finalExpression = SVB.evalBinOp(C.getState(), BO_And, positive, compareOperation, SVB.getConditionType());
  } else if (Op == BO_Sub || Op == BO_SubAssign) {
    // overflow if r < 0 && l > INT_MAX + r
    // r < 0
    SVal positive = SVB.evalBinOp(C.getState(), BO_LT, right, SVB.makeIntVal(0, false), SVB.getConditionType());
    // INT_MAX + r
    SVal applyOperation = SVB.evalBinOp(C.getState(), BO_Add, SVB.makeIntVal(INT_MAX, false), right, SVB.getConditionType());
    // l > INT_MAX + r
    SVal compareOperation = SVB.evalBinOp(C.getState(), BO_GT, left, applyOperation, SVB.getConditionType());
    // r < 0 && l > INT_MAX + r
    finalExpression = SVB.evalBinOp(C.getState(), BO_And, positive, compareOperation, SVB.getConditionType());
  }


  if (Optional<DefinedSVal> isOverflowDVal =
          finalExpression.getAs<DefinedSVal>()) {
    // Returns a pair of states (StInRange, StOutOfRange) where the given value is assumed to be in the range or out of the range, respectively.
    std::tie(stateOverflow, stateNotOverflow) = CM.assumeDual(C.getState(), *isOverflowDVal);

    if (!stateNotOverflow) {
      assert(stateOverflow);
      reportBug("Found overflow", stateOverflow, C);
      return;
    }
  }

  // If we get here, then the overflow should never happen. 
  C.addTransition(stateNotOverflow);
}

void ento::registerSimpleOverflowChecker(CheckerManager &mgr) {
  mgr.registerChecker<SimpleOverflowChecker>();
}

bool ento::shouldRegisterSimpleOverflowChecker(const CheckerManager &mgr) {
  return true;
}