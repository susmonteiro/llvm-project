// TODO change this
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_

#include <functional>
#include <string>
#include <variant>

#include "LifetimeAnnotationsLattice.h"
#include "ObjectRepository.h"
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

public:
  // TODO taken from crubit, implement something similar
  // explicit LifetimeAnalysis(
  //     const clang::FunctionDecl *func, ObjectRepository &object_repository,
  //     const llvm::DenseMap<const clang::FunctionDecl *,
  //                          FunctionLifetimesOrError> &callee_lifetimes,
  //     const DiagnosticReporter &diag_reporter)
  //     : clang::dataflow::DataflowAnalysis<LifetimeAnalysis,
  //     LifetimeAnnotationsLattice>(
  //           func->getASTContext(), /*ApplyBuiltinTransfer=*/false),
  //       func_(func), object_repository_(object_repository),
  //       callee_lifetimes_(callee_lifetimes), diag_reporter_(diag_reporter) {}

  explicit LifetimeAnnotationsAnalysis(const clang::FunctionDecl *func)
      : clang::dataflow::DataflowAnalysis<LifetimeAnnotationsAnalysis,
                                          LifetimeAnnotationsLattice>(
            func->getASTContext(), /* ApplyBuiltinTransfer */ false),
        func_(func) {}

  LifetimeAnnotationsLattice initialElement();

  std::string ToString(const LifetimeAnnotationsLattice &state);

  bool IsEqual(const LifetimeAnnotationsLattice &state1,
               const LifetimeAnnotationsLattice &state2);

  void transfer(const clang::CFGElement &elt, LifetimeAnnotationsLattice &state,
                clang::dataflow::Environment &environment);

  // TODO(yitzhakm): remove once https://reviews.llvm.org/D143920 is committed
  // and integrated downstream.
  void transfer(const clang::CFGElement *elt,
                LifetimeAnnotationsLattice &lattice,
                clang::dataflow::Environment &env) {
    transfer(*elt, lattice, env);
  }

private:
  const clang::FunctionDecl *func_;
  // TODO implement when object_repository is part of the constructor
  // ObjectRepository &object_repository_;
  // TODO implement callee lifetimes map
  // const llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimesOrError>
  //     &callee_lifetimes_;
  // TODO implement diag_reporter
  // const DiagnosticReporter &diag_reporter_;
};

} // namespace lifetimes
} // namespace tidy
} // namespace clang

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_ANALYSIS_H_
