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

  // Returns the points-to set associated with `pointer`, or an empty set if
  // `pointer` is not associated with a points-to set.
  // TODO
  //   ObjectSet GetPointerPointsToSet(const Object* pointer) const;

  // Associates `pointer` with the given points-to set.
  // TODO
  //   void SetPointerPointsToSet(const Object* pointer, ObjectSet points_to);

  // Associates all `pointers` with the given points-to set.
  // TODO
  //   void SetPointerPointsToSet(const ObjectSet& pointers,
  //                              const ObjectSet& points_to);

  // Extends a single `pointer`'s points-to set with the given points-to set.
  // TODO
  //   void ExtendPointerPointsToSet(const Object* pointer,
  //                                 const ObjectSet& points_to);

  // Returns the union of the points-to sets associated with the given pointers,
  // or an empty set if none of the pointers is associated with a points-to set.
  // TODO
  //   ObjectSet GetPointerPointsToSet(const ObjectSet& pointers) const;

  // Returns the object set associated with `expr`.
  // `expr` must previously have been associated with an object set through
  // a call to SetExprObjectSet(), and the function asserts that this is the
  // case. We intentionally don't return an empty object set in this case
  // because we want to notice if we're not propagating object sets through
  // expressions.
  // TODO
  //   ObjectSet GetExprObjectSet(const clang::Expr* expr) const;

  // Associates `expr` with the given object set.
  // TODO
  //   void SetExprObjectSet(const clang::Expr* expr, ObjectSet objects);

  // Returns all the pointers (not objects) with the given `lifetime`.
  // TODO
  //   std::vector<const Object*> GetAllPointersWithLifetime(
  //       Lifetime lifetime) const;

  llvm::SmallSet<const clang::Expr*, 2> GetExprPoints(const clang::Expr* expr) {
    return expr_points_to_[expr];
  }

  void InsertExprLifetimes(const clang::Expr* parent,
                           const clang::Expr* child) {
    expr_points_to_[parent].insert(child);
    const auto child_points_to = expr_points_to_[child];
    // propagate points-to
    // ? check if null?
    expr_points_to_[parent].insert(child_points_to.begin(), child_points_to.end());
  }

 private:
  // * objects to which each object points to
  // TODO
  //   llvm::DenseMap<const Object*, ObjectSet> pointer_points_tos_;
  // * objects present in each expression
  // TODO
  //   llvm::DenseMap<const clang::Expr*, ObjectSet> expr_objects_;
  // TODO 2?
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::Expr*, 2>>
      expr_points_to_;
};

}  // namespace clang

#endif  // LIFETIMES__POINTS_TO_MAP_H_
