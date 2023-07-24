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
  unsigned int num_indirections;
};

struct RHSTypeStruct {
  const clang::Stmt *stmt;
  unsigned int num_indirections;
  char extra_lifetime;
};

using VarStmtDependenciesMap =
    llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<RHSTypeStruct>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info,
                              const clang::FunctionDecl *func);

  FunctionLifetimeFactory &GetLifetimeFactory() { return Factory; }

  VariableLifetimesVector &GetVariableLifetimes();
  ObjectLifetimes GetVarDeclLifetime(const clang::VarDecl *var_decl,
                                     FunctionLifetimeFactory &lifetime_factory);
  ObjectLifetimes &GetObjectLifetimes(const clang::VarDecl *var_decl);
  Lifetime &GetLifetime(const clang::VarDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetime(const clang::VarDecl *var_decl,
                        unsigned int num_indirections);
  Lifetime &GetLifetimeWithSameNumberIndirections(
      const clang::VarDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetimeOrLocal(const clang::VarDecl *var_decl,
                               clang::QualType type);
  Lifetime &GetLifetimeOrLocal(const clang::VarDecl *var_decl,
                               unsigned int num_indirections);
  Lifetime &GetReturnLifetime(clang::QualType &type);
  Lifetime &GetReturnLifetime(unsigned int num_indirections);
  Lifetime &GetReturnLifetimeOrLocal(unsigned int num_indirections);

  bool IsLifetimeNotset(const clang::VarDecl *var_decl, clang::QualType &type);
  bool IsLifetimeNotset(const clang::VarDecl *var_decl,
                        unsigned int num_indirections);

  void CreateVariableIfNotFound(const clang::VarDecl *var_decl);

  PointsToMap &GetPointsTo() { return PointsTo; }

  const LifetimesVector &GetPossibleLifetimes(const clang::VarDecl *var_decl,
                                              clang::QualType type) {
    return GetLifetime(var_decl, type).GetPossibleLifetimes();
  }

  const LifetimesVector &GetPossibleLifetimes(const clang::VarDecl *var_decl,
                                              unsigned int num_indirections) {
    return GetLifetime(var_decl, num_indirections).GetPossibleLifetimes();
  }

  void PropagatePossibleLifetimes(const clang::VarDecl *target,
                                  const LifetimesVector &possible_lifetimes,
                                  unsigned int num_indirections) {
    GetLifetime(target, num_indirections)
        .InsertPossibleLifetimes(possible_lifetimes);
  }

  void CreateVariable(const clang::VarDecl *var_decl, clang::QualType type) {
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
                                const clang::Stmt *to, clang::QualType to_type,
                                char lifetime);

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                clang::QualType from_type,
                                const clang::Stmt *to, clang::QualType to_type);

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                unsigned int from_num_indirections,
                                const clang::Stmt *to,
                                unsigned int to_num_indirections,
                                char lifetime);

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                unsigned int from_num_indirections,
                                const clang::Stmt *to,
                                unsigned int to_num_indirections);

  void CreateStmtDependency(const clang::Stmt *from, const clang::VarDecl *to);

  void CreateStmtLifetime(const clang::Stmt *from, char id);

  void CreateDependencySimple(const clang::VarDecl *from,
                              clang::QualType from_type,
                              const clang::VarDecl *to, clang::QualType to_type,
                              const clang::Stmt *loc);
  void CreateDependencySimple(const clang::VarDecl *from,
                              unsigned int from_num_indirections,
                              const clang::VarDecl *to,
                              unsigned int to_num_indirections,
                              const clang::Stmt *loc);

  void CreateDependency(const clang::VarDecl *from, clang::QualType from_type,
                        const clang::VarDecl *to, clang::QualType to_type,
                        const clang::Stmt *loc);

  void CreateDependency(const clang::VarDecl *from,
                        unsigned int lhs_num_indirections,
                        const clang::VarDecl *to,
                        unsigned int rhs_num_indirections,
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
    return clang::LHSTypeStruct{nullptr, 0};
  }

  static inline clang::LHSTypeStruct getTombstoneKey() {
    return clang::LHSTypeStruct{reinterpret_cast<clang::VarDecl *>(-1), 0};
  }

  static unsigned getHashValue(const clang::LHSTypeStruct &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::VarDecl *>::getHashValue(pair.var_decl),
        llvm::DenseMapInfo<unsigned int>::getHashValue(pair.num_indirections));
  }

  static bool isEqual(const clang::LHSTypeStruct &lhs,
                      const clang::LHSTypeStruct &rhs) {
    return lhs.var_decl == rhs.var_decl &&
           lhs.num_indirections == rhs.num_indirections;
  }
};
template <>
struct DenseMapInfo<clang::RHSTypeStruct> {
  static inline clang::RHSTypeStruct getEmptyKey() {
    return clang::RHSTypeStruct{nullptr, 0};
  }

  static inline clang::RHSTypeStruct getTombstoneKey() {
    return clang::RHSTypeStruct{reinterpret_cast<clang::Stmt *>(-1), 0};
  }

  static unsigned getHashValue(const clang::RHSTypeStruct &pair) {
    return llvm::hash_combine(
        llvm::DenseMapInfo<const clang::Stmt *>::getHashValue(pair.stmt),
        llvm::DenseMapInfo<unsigned int>::getHashValue(pair.num_indirections));
  }

  static bool isEqual(const clang::RHSTypeStruct &lhs,
                      const clang::RHSTypeStruct &rhs) {
    return lhs.stmt == rhs.stmt && lhs.num_indirections == rhs.num_indirections;
  }
};
}  // namespace llvm

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
