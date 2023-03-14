// TODO change me
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_

#include <string>
#include <utility>
#include <variant>

#include "ObjectSet.h"
#include "clang/Analysis/FlowSensitive/DataflowAnalysis.h"
#include "clang/Analysis/FlowSensitive/DataflowLattice.h"
#include "llvm/ADT/StringRef.h"

namespace clang {
namespace tidy {
namespace lifetimes {

class LifetimeAnnotationsLattice {
public:
  // TODO need points-to-map? -> probably yes
  // TODO need constraints? -> probably not
  // Creates a lattice holding an empty points-to map and empty constraints.
  LifetimeAnnotationsLattice() = default;

  LifetimeAnnotationsLattice(const LifetimeAnnotationsLattice &) = default;
  LifetimeAnnotationsLattice(LifetimeAnnotationsLattice &&) = default;
  LifetimeAnnotationsLattice &
  operator=(const LifetimeAnnotationsLattice &) = default;
  LifetimeAnnotationsLattice &
  operator=(LifetimeAnnotationsLattice &&) = default;

  // TODO change comment
  // TODO change method
  // Now it does not care about:
  // - points-to-map
  // - lifetime constraints
  // TODO change following comment if points-to-map and/or lifetime constraints
  // TODO are not added
  // Creates a lattice containing the given points-to map,
  // single-valued object set, and empty constraints.
  explicit LifetimeAnnotationsLattice(ObjectSet single_valued_objects)
      : var_(std::move(single_valued_objects)) {}

  explicit LifetimeAnnotationsLattice(std::string err) : var_(err) {}

  // TODO if implementing points-to-map/constraints then create the respective
  // TODO functions (see crubit)

  // TODO see and maybe change comment
  // Returns the set of single-valued objects, i.e. objects that will be
  // guaranteed to be overwritten completely by a write operation.
  // For example, all local variables are single-valued unless they are
  // conditionally overwritten. Values that represent pointees of pointers are
  // not (as they could be arrays), but values that represent pointees of
  // references can be.
  // Precondition: !IsError().
  ObjectSet &SingleValuedObjects();
  const ObjectSet &SingleValuedObjects() const;

  // Returns whether the lattice is in the error state.
  bool IsError() const { return std::holds_alternative<std::string>(var_); }

  // Returns the error string.
  // Precondition: IsError().
  llvm::StringRef Error() const;

  // Returns a human-readable representation of the lattice.
  std::string ToString() const;

  // Sets the lattice to the result of the "join" operation with `other` and
  // returns the effect of the operation.
  // If either of the lattices contains an error, sets this lattice to the
  // first error encountered.
  clang::dataflow::LatticeJoinEffect
  join(const LifetimeAnnotationsLattice &other);

  // Compares for (in-)equality.
  // All error states are considered to be equal.
  bool operator==(const LifetimeAnnotationsLattice &other) const;
  bool operator!=(const LifetimeAnnotationsLattice &other) const {
    return !(*this == other);
  }

private:
  // variant: holds one of the values
  // TODO maybe hold a tuple instead of ObjectSet (see crubit)
  std::variant<ObjectSet, std::string> var_;
};

} // namespace lifetimes
} // namespace tidy
} // namespace clang

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_LIFETIME_LATTICE_H_
