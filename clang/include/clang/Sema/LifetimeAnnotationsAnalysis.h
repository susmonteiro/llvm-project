#ifndef LIFETIME_ANNOTATIONS_ANALYSIS_H_
#define LIFETIME_ANNOTATIONS_ANALYSIS_H_

#include <iostream>

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
#include "clang/Sema/PointsToMap.h"
// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using VariableLifetimesVector =
    llvm::DenseMap<const clang::VarDecl *, ObjectsLifetimes>;

using StmtVarDependenciesMap =
    llvm::DenseMap<const clang::Stmt *, llvm::DenseSet<const clang::VarDecl *>>;

using VarStmtDependenciesMap =
    llvm::DenseMap<const clang::VarDecl *, llvm::DenseSet<const clang::Stmt *>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info);

  VariableLifetimesVector &GetVariableLifetimes();
  ObjectsLifetimes &GetObjectsLifetimes(const clang::VarDecl *var_decl);
  Lifetime &GetLifetime(const clang::VarDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetimeOrLocal(const clang::VarDecl *var_decl,
                               clang::QualType type);
  Lifetime &GetReturnLifetime(clang::QualType &type);

  bool IsLifetimeNotset(const clang::VarDecl *var_decl,
                        clang::QualType &type) const;
  bool IsLifetimeNotset(const clang::VarDecl *var_decl) const;

  PointsToMap &GetPointsTo() { return PointsTo; }

  const LifetimesVector &GetShortestLifetimes(const clang::VarDecl *var_decl,
                                        clang::QualType &type) {
    return GetLifetime(var_decl, type).GetShortestLifetimes();
  }

  void PropagateShortestLifetimes(const clang::VarDecl *target,
                                  const LifetimesVector &shortest_lifetimes,
                                  clang::QualType &type) {
    GetLifetime(target, type).InsertShortestLifetimes(shortest_lifetimes);
  }

  void PropagateShortestLifetimes(const clang::VarDecl *to,
                                  clang::QualType &to_type,
                                  const clang::VarDecl *from,
                                  clang::QualType &from_type) {
    // TODO maybe same type is enough
    const auto &from_lifetimes = GetShortestLifetimes(from, from_type);
    PropagateShortestLifetimes(to, from_lifetimes, to_type);
  }

  void CreateVariable(const clang::VarDecl *var_decl, clang::QualType &type) {
    VariableLifetimes[var_decl] = ObjectsLifetimes(Lifetime(type));
  }

  void CreateVariable(const clang::VarDecl *var_decl, Lifetime &lifetime) {
    VariableLifetimes[var_decl] = ObjectsLifetimes(lifetime);
  }

  void CreateVariable(const clang::VarDecl *var_decl,
                      ObjectsLifetimes objectsLifetime) {
    VariableLifetimes[var_decl] = objectsLifetime;
  }

  VarStmtDependenciesMap &GetLifetimeDependencies();
  StmtVarDependenciesMap &GetStmtDependencies();

  void CreateLifetimeDependency(const clang::VarDecl *from,
                                const clang::Stmt *to);

  void CreateStmtDependency(const clang::Stmt *from, const clang::VarDecl *to);

  void CreateDependency(const clang::VarDecl *from, const clang::VarDecl *to,
                        const clang::Stmt *loc);

  void SetDependencies(VarStmtDependenciesMap dependencies) {
    LifetimeDependencies = std::move(dependencies);
  }

  void ProcessShortestLifetimes();

  llvm::DenseMap<const clang::VarDecl *, llvm::DenseSet<const clang::VarDecl *>>
  TransposeDependencies();
  std::vector<const clang::VarDecl *> InitializeWorklist() const;

  std::string DebugString();

 private:
  VariableLifetimesVector VariableLifetimes;
  VarStmtDependenciesMap LifetimeDependencies;
  // ? can one stmt point to more than one var_decl?
  StmtVarDependenciesMap StmtDependencies;
  PointsToMap PointsTo;
  ObjectsLifetimes ReturnLifetime;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;
};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
