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

using VarTypePair = std::pair<const clang::VarDecl *, clang::QualType>;

struct VarTypePairInfo {
  static inline VarTypePair getEmptyKey() {
    return std::make_pair(nullptr, clang::QualType());
  }

  static inline VarTypePair getTombstoneKey() {
    return std::make_pair(reinterpret_cast<clang::VarDecl *>(-1),
                          clang::QualType());
  }

  static unsigned getHashValue(const VarTypePair &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::VarDecl *>::getHashValue(pair.first),
        llvm::DenseMapInfo<clang::QualType>::getHashValue(pair.second));
  }

  static bool isEqual(const VarTypePair &lhs, const VarTypePair &rhs) {
    return lhs.first == rhs.first && lhs.second == rhs.second;
  }
};

using VarStmtDependenciesMap =
    llvm::DenseMap<VarTypePair, llvm::DenseSet<const clang::Stmt *>,
                   VarTypePairInfo>;

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

  llvm::DenseMap<VarTypePair, llvm::DenseSet<VarTypePair>, VarTypePairInfo>
  TransposeDependencies();
  std::vector<VarTypePair> InitializeWorklist() const;
  std::string DebugString();

 private:
  VariableLifetimesVector VariableLifetimes;
  VarStmtDependenciesMap LifetimeDependencies;
  // ? can one stmt point to more than one var_decl?
  StmtVarDependenciesMap StmtDependencies;
  PointsToMap PointsTo;
  ObjectLifetimes ReturnLifetime;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;
};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
