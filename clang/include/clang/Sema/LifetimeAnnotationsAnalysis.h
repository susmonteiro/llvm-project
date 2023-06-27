#ifndef LIFETIME_ANNOTATIONS_ANALYSIS_H_
#define LIFETIME_ANNOTATIONS_ANALYSIS_H_

#include <iostream>

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
#include "clang/Sema/PointsToMap.h"
#include "llvm/ADT/DenseMap.h"
// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using VariableLifetimesVector =
    llvm::DenseMap<const clang::VarDecl *, ObjectLifetimes>;

using StmtVarDependenciesMap =
    llvm::DenseMap<const clang::Stmt *, llvm::DenseSet<const clang::VarDecl *>>;

struct VarTypeStruct {
  const clang::VarDecl *var_decl;
  clang::QualType lhs_type;
};

using VarStmtDependenciesMap =
    llvm::DenseMap<VarTypeStruct, llvm::DenseSet<const clang::Stmt *>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info);

  VariableLifetimesVector &GetVariableLifetimes();
  ObjectLifetimes &GetObjectLifetimes(const clang::VarDecl *var_decl);
  Lifetime &GetLifetime(const clang::VarDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetimeOrLocal(const clang::VarDecl *var_decl,
                               clang::QualType type);
  Lifetime &GetReturnLifetime(clang::QualType &type);

  bool IsLifetimeNotset(const clang::VarDecl *var_decl,
                        clang::QualType &type) const;

  PointsToMap &GetPointsTo() { return PointsTo; }

  const LifetimesVector &GetPossibleLifetimes(const clang::VarDecl *var_decl,
                                              clang::QualType type) {
    return GetLifetime(var_decl, type).GetPossibleLifetimes();
  }

  void PropagatePossibleLifetimes(const clang::VarDecl *target,
                                  const LifetimesVector &possible_lifetimes,
                                  clang::QualType type) {
    GetLifetime(target, type).InsertPossibleLifetimes(possible_lifetimes);
  }

  void PropagatePossibleLifetimes(const clang::VarDecl *to,
                                  clang::QualType &to_type,
                                  const clang::VarDecl *from,
                                  clang::QualType &from_type) {
    // TODO maybe same type is enough
    const auto &from_lifetimes = GetPossibleLifetimes(from, from_type);
    PropagatePossibleLifetimes(to, from_lifetimes, to_type);
  }

  void CreateVariable(const clang::VarDecl *var_decl, clang::QualType &type) {
    VariableLifetimes[var_decl] = ObjectLifetimes(Lifetime(type));
  }

  void CreateVariable(const clang::VarDecl *var_decl, Lifetime &lifetime) {
    VariableLifetimes[var_decl] = ObjectLifetimes(lifetime);
  }

  void CreateVariable(const clang::VarDecl *var_decl,
                      ObjectLifetimes objectsLifetime) {
    VariableLifetimes[var_decl] = objectsLifetime;
  }

  VarStmtDependenciesMap &GetLifetimeDependencies();
  StmtVarDependenciesMap &GetStmtDependencies();

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                clang::QualType from_type,
                                const clang::Stmt *to);

  void CreateStmtDependency(const clang::Stmt *from, const clang::VarDecl *to);

  void CreateDependency(const clang::VarDecl *from, clang::QualType from_type,
                        const clang::VarDecl *to, const clang::Stmt *loc);

  void SetDependencies(VarStmtDependenciesMap dependencies) {
    LifetimeDependencies = std::move(dependencies);
  }

  void ProcessPossibleLifetimes();

  llvm::DenseMap<VarTypeStruct, llvm::DenseSet<VarTypeStruct>>
  TransposeDependencies();
  std::vector<VarTypeStruct> InitializeWorklist() const;
  std::string DebugString();

 private:
  static VarTypeStruct InvalidEmpty();
  static VarTypeStruct InvalidTombstone();

  friend class llvm::DenseMapInfo<VarTypeStruct>;

  VariableLifetimesVector VariableLifetimes;
  VarStmtDependenciesMap LifetimeDependencies;
  StmtVarDependenciesMap StmtDependencies;
  PointsToMap PointsTo;
  ObjectLifetimes ReturnLifetime;

};
}  // namespace clang

namespace llvm {
template <>
struct DenseMapInfo<clang::VarTypeStruct> {
  static inline clang::VarTypeStruct getEmptyKey() {
    return clang::VarTypeStruct{nullptr, clang::QualType()};
  }

  static inline clang::VarTypeStruct getTombstoneKey() {
    return clang::VarTypeStruct{reinterpret_cast<clang::VarDecl *>(-1),
                                clang::QualType()};
  }

  static unsigned getHashValue(const clang::VarTypeStruct &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::VarDecl *>::getHashValue(pair.var_decl),
        llvm::DenseMapInfo<clang::QualType>::getHashValue(pair.lhs_type));
  }

  static bool isEqual(const clang::VarTypeStruct &lhs,
                      const clang::VarTypeStruct &rhs) {
    return lhs.var_decl == rhs.var_decl && lhs.lhs_type == rhs.lhs_type;
  }
};
}  // namespace llvm

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
