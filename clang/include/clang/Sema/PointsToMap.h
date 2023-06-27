// Part of the Crubit project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef LIFETIMES_POINTS_TO_MAP_H_
#define LIFETIMES_POINTS_TO_MAP_H_

#include <string>

// #include "lifetime_analysis/object_set.h"
#include "clang/AST/Expr.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"

namespace clang {

// Maintains the points-to sets needed for the analysis of a function.
// A `PointsToMap` stores points-to sets for
// - Objects of reference-like type
// - Expressions that are prvalues of pointer type or glvalues (glvalues are,
//   in spirit, references to the object they refer to.)
// - The function's return value, if it is of reference-like type
//
// Note that the relationship between an expression's type and the type of the
// objects associated with it depends on whether the expression is a glvalue or
// prvalue:
// - glvalue expressions are associated with the object that is identified by
//   the glvalue. This means that the object has the same type as the glvalue
//   expression.
// - prvalue expressions of pointer type as are associated with the object that
//   the pointer points to. This means that if the prvalue expression has type
//   `T *`, the object has type `T`.
// The PointsToMap class does not enforce these type relationships because we
// intend to allow type punning (at least within the implementations of
// functions).
class PointsToMap {
 public:
  PointsToMap() = default;

  PointsToMap(const PointsToMap&) = default;
  PointsToMap(PointsToMap&&) = default;
  PointsToMap& operator=(const PointsToMap&) = default;
  PointsToMap& operator=(PointsToMap&&) = default;

  bool operator==(const PointsToMap& other) const;
  bool operator!=(const PointsToMap& other) const { return !(*this == other); }

  // Returns a human-readable representation of this object.
  std::string DebugString() const;
  size_t size() const { return ExprPointsTo.size(); }

  // TODO
  //   const llvm::DenseMap<const Object*, ObjectSet>& PointerPointsTos() const
  //   {
  //     return pointer_points_tos_;
  //   }

  // Returns a `PointsToMap` containing the union of mappings from this map and
  // `other`.
  // If both this map and `other` associate a points-to set with the same
  // entity, the returned map associates that entity with the union of the
  // corresponding points-to sets.
  PointsToMap Union(const PointsToMap& other) const;

  llvm::SmallSet<const clang::Expr*, 2> GetExprPointsTo(const clang::Expr* expr) {
    return ExprPointsTo[expr];
  }

  bool IsEmpty(const clang::Expr* expr) {
    return ExprPointsTo[expr].empty();
  }

  void InsertExprLifetimes(const clang::Expr* parent,
                           const clang::Expr* child) {
    ExprPointsTo[parent].insert(child);
    const auto child_points_to = ExprPointsTo[child];
    // propagate points-to
    // ? check if null?
    ExprPointsTo[parent].insert(child_points_to.begin(), child_points_to.end());
  }

 private:
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::Expr*, 2>>
      ExprPointsTo;
};

}  // namespace clang

#endif  // LIFETIMES__POINTS_TO_MAP_H_
