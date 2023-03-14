// Part of the Crubit project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#include "LifetimeAnnotationsLattice.h"

#include <assert.h>

#include <string>
#include <tuple>
#include <utility>

#include "ObjectSet.h"
#include "clang/Analysis/FlowSensitive/DataflowLattice.h"

namespace clang {
namespace tidy {
namespace lifetimes {

std::string LifetimeAnnotationsLattice::ToString() const {
  if (IsError()) {
    return Error().str();
  }
  // TODO change this
  //   return PointsTo().DebugString();
  return "To be implemented";
}

ObjectSet &LifetimeAnnotationsLattice::SingleValuedObjects() {
  // TODO if tuple is added to .h
  //   return std::get<ObjectSet>(std::get<0>(var_));
  return std::get<ObjectSet>(var_);
}

const ObjectSet &LifetimeAnnotationsLattice::SingleValuedObjects() const {
  // TODO if tuple is added to .h
  //   return std::get<ObjectSet>(std::get<0>(var_));
  return std::get<ObjectSet>(var_);
}

llvm::StringRef LifetimeAnnotationsLattice::Error() const {
  return std::get<std::string>(var_);
}

// TODO check and change this
clang::dataflow::LatticeJoinEffect
LifetimeAnnotationsLattice::join(const LifetimeAnnotationsLattice &other) {
  if (IsError()) {
    return clang::dataflow::LatticeJoinEffect::Unchanged;
  }
  if (other.IsError()) {
    *this = other;
    return clang::dataflow::LatticeJoinEffect::Changed;
  }

  // TODO remove me
  auto effect = clang::dataflow::LatticeJoinEffect::Unchanged;

  // TODO in case of constraints
  //   auto effect = Constraints().join(other.Constraints());

  // TODO in case of points-to-map
  //   PointsToMap joined_points_to_map = PointsTo().Union(other.PointsTo());
  //   if (PointsTo() != joined_points_to_map) {
  //     PointsTo() = std::move(joined_points_to_map);
  //     effect = clang::dataflow::LatticeJoinEffect::Changed;
  //   }

  ObjectSet joined_single_valued_objects =
      SingleValuedObjects().Intersection(other.SingleValuedObjects());
  if (SingleValuedObjects() != joined_single_valued_objects) {
    SingleValuedObjects() = std::move(joined_single_valued_objects);
    auto effect = clang::dataflow::LatticeJoinEffect::Changed;
  }

  return effect;
}

bool LifetimeAnnotationsLattice::operator==(
    const LifetimeAnnotationsLattice &other) const {
  if (IsError() || other.IsError()) {
    // Any error compares equal to any other error.
    return IsError() && other.IsError();
  }
  // TODO change this
  //   return PointsTo() == other.PointsTo() && Constraints() ==
  //   other.Constraints();
  return true;
}

} // namespace lifetimes
} // namespace tidy
} // namespace clang
