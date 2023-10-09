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
class PointsToMap {
 public:
  PointsToMap() = default;

  PointsToMap(const PointsToMap&) = default;
  PointsToMap(PointsToMap&&) = default;
  PointsToMap& operator=(const PointsToMap&) = default;
  PointsToMap& operator=(PointsToMap&&) = default;

  bool operator==(const PointsToMap& other) const;
  bool operator!=(const PointsToMap& other) const { return !(*this == other); }

  size_t PointsToSize() const { return ExprPointsTo.size(); }

  llvm::SmallSet<const clang::Expr*, 2>& GetExprPointsTo(
      const clang::Expr* expr) {
    return ExprPointsTo[expr];
  }

  llvm::SmallSet<const clang::VarDecl*, 2>& GetExprDecls(
      const clang::Expr* expr) {
    return ExprToDecl[expr];
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

  bool InsertCallExprInfo(const clang::Expr* parent, const clang::Expr* child) {
    if (CallExprToInfo.find(parent) != CallExprToInfo.end()) {
      return true;
    }

    auto it = CallExprToInfo.find(child);
    if (it != CallExprToInfo.end()) {
      CallExprToInfo[parent] = it->second;
      return true;
    }
    return false;
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

  void InsertExprLifetime(const clang::Expr* expr, char lifetime) {
    ExprToLifetime[expr] = lifetime;
    // propagate downwards
    for (const auto& child : ExprPointsTo[expr]) {
      ExprToLifetime[child] = lifetime;
    }
  }

  void InsertPointsTo(const clang::Expr* parent, const clang::Expr* child) {
    InsertExprPointsTo(parent, child);
    InsertExprDecl(parent, child);
    InsertCallExprInfo(parent, child);
  }

 private:
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::Expr*, 2>>
      ExprPointsTo;
  llvm::DenseMap<const clang::CallExpr*, llvm::SmallSet<const clang::Expr*, 2>>
      CallExprPointsTo;
  llvm::DenseMap<const clang::Expr*, llvm::SmallSet<const clang::VarDecl*, 2>>
      ExprToDecl;
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
