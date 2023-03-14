// TODO change me
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_

#include <string>
#include <utility>
#include <variant>

#include "clang/Analysis/FlowSensitive/DataflowAnalysis.h"
#include "clang/Analysis/FlowSensitive/DataflowLattice.h"
#include "llvm/ADT/StringRef.h"

namespace clang {
namespace tidy {
namespace lifetimes {

class LifetimeAnnotationsLattice {
public:
  // Creates a lattice holding an empty points-to map and empty constraints.
  LifetimeAnnotationsLattice() = default;

  // TODO implement this
};

} // namespace lifetimes
} // namespace tidy
} // namespace clang

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_
