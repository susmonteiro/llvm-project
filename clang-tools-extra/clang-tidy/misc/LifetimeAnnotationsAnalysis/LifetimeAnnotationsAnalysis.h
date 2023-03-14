// TODO change this
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_

#include <functional>
#include <string>
#include <variant>

#include "LifetimeAnnotationsAnalysis/LifetimeAnnotationsLattice.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/Type.h"
#include "clang/Analysis/CFG.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace tidy {
namespace lifetimes {
// TODO
// Function to call to report a diagnostic.
// This has the same interface as ClangTidyCheck::diag().
// using DiagnosticReporter = std::function<clang::DiagnosticBuilder(
//     clang::SourceLocation, clang::StringRef, clang::DiagnosticIDs::Level)>;

class LifetimeAnnotationsAnalysis
    : public clang::dataflow::DataflowAnalysis<LifetimeAnnotationsAnalysis,
                                               LifetimeAnnotationsLattice> {
  // TODO implement this
};

} // namespace lifetimes
} // namespace tidy
} // namespace clang

class ObjectRepository {
  // TODO implement this maybe
  // TODO change file maybe
};

class PointsToMap {
  // TODO implement this maybe
  // TODO change file maybe
};

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_
