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

using LifetimesMap = llvm::DenseMap<const clang::Stmt *, char>;

struct LHSTypeStruct {
  const clang::VarDecl *var_decl;
  clang::QualType type;
};

struct RHSTypeStruct {
  const clang::Stmt *stmt;
  clang::QualType type;
};

using VarStmtDependenciesMap =
    llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<RHSTypeStruct>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info, const clang::FunctionDecl *func);

  FunctionLifetimeFactory &GetLifetimeFactory() { return Factory; }

  VariableLifetimesVector &GetVariableLifetimes();
  ObjectLifetimes GetVarDeclLifetime(const clang::VarDecl *var_decl,
                                     FunctionLifetimeFactory &lifetime_factory);
  ObjectLifetimes &GetObjectLifetimes(const clang::VarDecl *var_decl);
  Lifetime &GetLifetime(const clang::VarDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetimeOrLocal(const clang::VarDecl *var_decl,
                               clang::QualType type);
  Lifetime &GetReturnLifetime(clang::QualType &type);

  bool IsLifetimeNotset(const clang::VarDecl *var_decl,
                        clang::QualType &type);
  
  void CreateVariableIfNotFound(const clang::VarDecl *var_decl,
                                clang::QualType type);

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
  char GetStmtLifetime(const clang::Stmt *stmt);

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                clang::QualType from_type,
                                const clang::Stmt *to, clang::QualType to_type);

  void CreateStmtDependency(const clang::Stmt *from, const clang::VarDecl *to);

  void CreateStmtLifetime(const clang::Stmt *from, char id);

  void CreateDependencySimple(const clang::VarDecl *from,
                              clang::QualType from_type,
                              const clang::VarDecl *to, clang::QualType to_type,
                              const clang::Stmt *loc);

  void CreateDependency(const clang::VarDecl *from, clang::QualType from_type,
                        const clang::VarDecl *to, clang::QualType to_type,
                        const clang::Stmt *loc);

  void SetDependencies(VarStmtDependenciesMap dependencies) {
    LifetimeDependencies = std::move(dependencies);
  }

  void ProcessPossibleLifetimes();

  llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<LHSTypeStruct>>
  TransposeDependencies();
  std::vector<LHSTypeStruct> InitializeWorklist() const;
  std::string DebugString();
  static std::string WorklistDebugString(std::vector<LHSTypeStruct> &worklist);

 private:
  static LHSTypeStruct InvalidEmpty();
  static LHSTypeStruct InvalidTombstone();

  friend class llvm::DenseMapInfo<LHSTypeStruct>;

  VariableLifetimesVector VariableLifetimes;
  VarStmtDependenciesMap LifetimeDependencies;
  StmtVarDependenciesMap StmtDependencies;
  LifetimesMap StmtLifetimes;
  PointsToMap PointsTo;
  ObjectLifetimes ReturnLifetime;
  FunctionLifetimeFactory Factory;

};
}  // namespace clang

namespace llvm {
template <>
struct DenseMapInfo<clang::LHSTypeStruct> {
  static inline clang::LHSTypeStruct getEmptyKey() {
    return clang::LHSTypeStruct{nullptr, clang::QualType()};
  }

  static inline clang::LHSTypeStruct getTombstoneKey() {
    return clang::LHSTypeStruct{reinterpret_cast<clang::VarDecl *>(-1),
                                clang::QualType()};
  }

  static unsigned getHashValue(const clang::LHSTypeStruct &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::VarDecl *>::getHashValue(pair.var_decl),
        llvm::DenseMapInfo<clang::QualType>::getHashValue(pair.type));
  }

  static bool isEqual(const clang::LHSTypeStruct &lhs,
                      const clang::LHSTypeStruct &rhs) {
    return lhs.var_decl == rhs.var_decl && lhs.type == rhs.type;
  }
};
template <>
struct DenseMapInfo<clang::RHSTypeStruct> {
  static inline clang::RHSTypeStruct getEmptyKey() {
    return clang::RHSTypeStruct{nullptr, clang::QualType()};
  }

  static inline clang::RHSTypeStruct getTombstoneKey() {
    return clang::RHSTypeStruct{reinterpret_cast<clang::Stmt *>(-1),
                                clang::QualType()};
  }

  static unsigned getHashValue(const clang::RHSTypeStruct &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::Stmt *>::getHashValue(pair.stmt),
        llvm::DenseMapInfo<clang::QualType>::getHashValue(pair.type));
  }

  static bool isEqual(const clang::RHSTypeStruct &lhs,
                      const clang::RHSTypeStruct &rhs) {
    return lhs.stmt == rhs.stmt && lhs.type == rhs.type;
  }
};
}  // namespace llvm

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
