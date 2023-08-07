// Part of the Crubit project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef LIFETIMES_POINTS_TO_MAP_H_
#define LIFETIMES_POINTS_TO_MAP_H_

#include <string>

// #include "lifetime_analysis/object_set.h"
#include "clang/AST/Expr.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/ObjectLifetimes.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"

namespace clang {

typedef struct {
  // decl = nullptr -> lifetime $local
  const clang::Expr* arg;
  unsigned int num_indirections;
} CallExprInfo;

typedef struct {
  llvm::DenseSet<CallExprInfo> info;
  const clang::CallExpr* call_expr;
  bool is_local = false;
  bool is_static = false;
} Dependencies;

using TypeToSet = llvm::DenseMap<unsigned int, Dependencies>;
using CallExprInfoMap = llvm::DenseMap<const clang::Expr*, TypeToSet>;

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
  void DebugExprToType() const {
    for (const auto& [expr, type] : ExprToType) {
      debugLifetimes("Expr");
      expr->dump();
      debugLifetimes("Type", type.getAsString());
    }
  }
  size_t PointsToSize() const { return ExprPointsTo.size(); }

  llvm::SmallSet<const clang::Expr*, 2>& GetExprPointsTo(
      const clang::Expr* expr) {
    return ExprPointsTo[expr];
  }

  llvm::SmallSet<const clang::VarDecl*, 2>& GetExprDecls(
      const clang::Expr* expr) {
    return ExprToDecl[expr];
  }

  clang::QualType GetExprType(const clang::Expr* expr) {
    auto it = ExprToType.find(expr);
    if (it == ExprToType.end()) {
      return clang::QualType();
    } else {
      return it->second;
    }
  }

  char GetExprLifetime(const clang::Expr* expr) {
    auto it = ExprToLifetime.find(expr);
    if (it == ExprToLifetime.end()) {
      return NOTSET;
    } else {
      return it->second;
    }
  }

  TypeToSet& GetCallExprInfo(const clang::Expr* expr) {
    return CallExprToInfo[expr];
  }

  bool HasCallExprPointsTo(const clang::CallExpr* parent,
                           const clang::Expr* child) {
    return CallExprPointsTo[parent].find(child) !=
           CallExprPointsTo[parent].end();
  }

  bool HasExprDecl(const clang::Expr* expr, const clang::VarDecl* decl) {
    return ExprToDecl[expr].find(decl) != ExprToDecl[expr].end();
  }

  bool HasCallExprInfo(const clang::Expr* expr) {
    return CallExprToInfo.find(expr) != CallExprToInfo.end();
  }

  void RemoveExprType(const clang::Expr* expr) { ExprToType.erase(expr); }

  bool IsEmpty(const clang::Expr* expr) { return ExprPointsTo[expr].empty(); }

  void InsertExprPointsTo(const clang::Expr* parent, const clang::Expr* child) {
    ExprPointsTo[parent].insert(child);
    const auto child_points_to = ExprPointsTo[child];
    // propagate points-to
    ExprPointsTo[parent].insert(child_points_to.begin(), child_points_to.end());
  }

  void InsertCallExprPointsTo(const clang::CallExpr* parent,
                              const clang::Expr* child) {
    CallExprPointsTo[parent].insert(child);
    const auto child_points_to = ExprPointsTo[child];
    // propagate points-to
    CallExprPointsTo[parent].insert(child_points_to.begin(),
                                    child_points_to.end());
  }

  void InsertExprDecl(const clang::Expr* expr, const clang::VarDecl* decl) {
    ExprToDecl[expr].insert(decl);
  }

  void InsertExprDecl(const clang::Expr* parent, const clang::Expr* child) {
    const auto child_decls = ExprToDecl[child];
    if (!child_decls.empty()) {
      ExprToDecl[parent].insert(child_decls.begin(), child_decls.end());
    }
  }

  void InsertExprType(const clang::Expr* expr, clang::QualType type) {
    ExprToType[expr] = type.getCanonicalType();
  }

  void InsertExprLifetime(const clang::Expr* expr, char lifetime) {
    // TODO delete this
    assert(lifetime != NOTSET);
    ExprToLifetime[expr] = lifetime;
    // propagate downwards
    for (const auto& child : ExprPointsTo[expr]) {
      ExprToLifetime[child] = lifetime;
    }
  }

  bool InsertCallExprInfo(const clang::Expr* parent, const clang::Expr* child) {
    if (CallExprToInfo.find(parent) != CallExprToInfo.end()) {
      debugLifetimes("InsertCallExprInfo", "FOUND PARENT!");
      return true;
    }

    auto it = CallExprToInfo.find(child);
    if (it != CallExprToInfo.end()) {
      debugLifetimes("InsertCallExprInfo", "FOUND CHILD!");
      CallExprToInfo[parent] = it->second;
      return true;
    }
    debugLifetimes("InsertCallExprInfo", "NOT FOUND...");

    return false;
  }

 private:
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::Expr*, 2>>
      ExprPointsTo;
  llvm::DenseMap<const clang::CallExpr*, llvm::SmallSet<const clang::Expr*, 2>>
      CallExprPointsTo;
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::VarDecl*, 2>>
      ExprToDecl;
  llvm::DenseMap<const clang::Expr*, clang::QualType> ExprToType;
  llvm::DenseMap<const clang::Expr*, char> ExprToLifetime;
  CallExprInfoMap CallExprToInfo;
};

}  // namespace clang

namespace llvm {

template <>
struct DenseMapInfo<clang::CallExprInfo, void> {
  static clang::CallExprInfo getEmptyKey() {
    return clang::CallExprInfo{nullptr, 0};
  }

  static clang::CallExprInfo getTombstoneKey() {
    return clang::CallExprInfo{reinterpret_cast<const clang::Expr*>(-1), 0};
  }

  static unsigned getHashValue(const clang::CallExprInfo& info) {
    return llvm::hash_combine(info.arg, info.num_indirections);
  }

  static bool isEqual(const clang::CallExprInfo& lhs,
                      const clang::CallExprInfo& rhs) {
    return lhs.arg == rhs.arg && lhs.num_indirections == rhs.num_indirections;
  }
};

}  // namespace llvm

#endif  // LIFETIMES__POINTS_TO_MAP_H_
