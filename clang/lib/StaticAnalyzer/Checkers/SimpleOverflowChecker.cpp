#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Checkers/Taint.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include <limits.h>

// TODO remove following import
#include <iostream>

using namespace clang;
using namespace ento;
using namespace taint;

namespace {
class SimpleOverflowChecker : public Checker<check::PreStmt<BinaryOperator>> {
  mutable std::unique_ptr<BuiltinBug> BT;
  void reportBug(const char *Msg, ProgramStateRef StateZero, CheckerContext &C,
                 std::unique_ptr<BugReporterVisitor> Visitor = nullptr) const;

  public:
    void checkPreStmt(const BinaryOperator *B, CheckerContext &C) const;
  };
} // end anonymous namespace

// TODO change this
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
      Op != BO_Sub && Op != BO_SubAssign && 
      Op != BO_Mul && Op != BO_MulAssign &&
      Op != BO_Div && Op != BO_DivAssign) 
    return;
  
  if (!B->getRHS()->getType()->isScalarType())
    return;


  // Check if result of a Op b < INT_MAX
  ConstraintManager &CM = C.getConstraintManager();
  ProgramStateRef stateNotZero, stateZero;

  SVal left = C.getSVal(B->getLHS());
  SVal right = C.getSVal(B->getRHS());
  
  SValBuilder &SVB = C.getSValBuilder();
  SVal applyOperation = SVB.evalBinOp(C.getState(), Op, left, right, SVB.getConditionType());

  SVal isOverflow = SVB.evalBinOp(C.getState(), BO_LT, applyOperation, SVB.makeIntVal(INT_MAX, false), SVB.getConditionType());  

  if (Optional<DefinedSVal> isOverflowDVal =
          isOverflow.getAs<DefinedSVal>()) {
    // Returns a pair of states (StInRange, StOutOfRange) where the given value is assumed to be in the range or out of the range, respectively.
    std::tie(stateNotZero, stateZero) = CM.assumeDual(C.getState(), *isOverflowDVal);

    if (!stateNotZero) {
      assert(stateZero);
      reportBug("Found overflow", stateZero, C);
      return;
    }
  }

  // If we get here, then the denom should not be zero. We abandon the implicit
  // zero denom case for now.
  C.addTransition(stateNotZero);
}

void ento::registerSimpleOverflowChecker(CheckerManager &mgr) {
  mgr.registerChecker<SimpleOverflowChecker>();
}

bool ento::shouldRegisterSimpleOverflowChecker(const CheckerManager &mgr) {
  return true;
}