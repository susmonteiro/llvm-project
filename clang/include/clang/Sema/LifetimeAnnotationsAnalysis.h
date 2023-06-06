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
    llvm::DenseMap<const clang::NamedDecl *, ObjectsLifetimes>;

// expr -> DeclRefExpr or UnaryOperator
// somehow merge the one above and this one
using ExprLifetimesVector = llvm::DenseMap<const clang::Expr *, Lifetime>;

using StmtVarDependenciesMap =
    llvm::DenseMap<const clang::Stmt *,
                   llvm::DenseSet<const clang::NamedDecl *>>;

using VarStmtDependenciesMap =
    llvm::DenseMap<const clang::NamedDecl *,
                   llvm::DenseSet<const clang::Stmt *>>;

using StmtExprDependenciesMap =
    llvm::DenseMap<const clang::Stmt *, llvm::DenseSet<const clang::Expr *>>;

using ExprStmtDependenciesMap =
    llvm::DenseMap<const clang::Expr *, llvm::DenseSet<const clang::Stmt *>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info);

  VariableLifetimesVector &GetVariableLifetimes();
  Lifetime &GetLifetime(const clang::NamedDecl *var_decl, clang::QualType type);
  Lifetime &GetLifetime(const clang::Expr *expr);
  Lifetime &GetReturnLifetime();

  bool IsLifetimeNotset(const clang::NamedDecl *var_decl, clang::QualType &type) const {
    auto it = VariableLifetimes.find(var_decl);
    if (it != VariableLifetimes.end()) {
      ObjectsLifetimes ol = std::move(it->second);
      return ol.GetLifetime(type).IsNotSet();
    } else {
      // TODO error?
      return false;
    }
  }

  bool IsLifetimeNotset(const clang::Expr *expr) const {
    auto it = ExprLifetimes.find(expr);
    if (it != ExprLifetimes.end()) {
      return it->second.IsNotSet();
    } else {
      // TODO error?
      return false;
    }
  }

  PointsToMap &GetPointsTo() { return PointsTo; }

  LifetimesVector GetShortestLifetimes(const clang::NamedDecl *var_decl, clang::QualType &type) {
    return VariableLifetimes[var_decl].GetLifetime(type).GetShortestLifetimes();
  }

  LifetimesVector GetShortestLifetimes(const clang::Expr *expr) {
    return ExprLifetimes[expr].GetShortestLifetimes();
  }

  void PropagateShortestLifetimes(const clang::NamedDecl *target,
                                  LifetimesVector shortest_lifetimes, clang::QualType &type) {
    VariableLifetimes[target].GetLifetime(type).InsertShortestLifetimes(shortest_lifetimes);
  }

  void PropagateShortestLifetimes(const clang::NamedDecl *to,
                                  clang::QualType &to_type,
                                  const clang::NamedDecl *from,
                                  clang::QualType &from_type) {
                                    // TODO maybe same type is enough
    const auto from_lifetimes = GetShortestLifetimes(from, from_type);
    PropagateShortestLifetimes(to, from_lifetimes, to_type);
  }

  void CreateVariable(const clang::NamedDecl *var_decl, clang::QualType &type) {
  
    VariableLifetimes[var_decl] = ObjectsLifetimes(Lifetime(), type);
  }

  void CreateVariable(const clang::NamedDecl *var_decl, Lifetime lifetime, clang::QualType &type) {
    VariableLifetimes[var_decl] = ObjectsLifetimes(lifetime, type);
  }

  void CreateDeclRef(const clang::Expr *expr) {
    ExprLifetimes[expr] = Lifetime();
  }

  void CreateDeclRef(const clang::Expr *expr, Lifetime lifetime) {
    ExprLifetimes[expr] = Lifetime(lifetime);
  }

  VarStmtDependenciesMap &GetLifetimeDependencies();
  StmtVarDependenciesMap &GetStmtDependencies();
  StmtExprDependenciesMap &GetStmtExprDependencies();

  void CreateLifetimeDependency(const clang::NamedDecl *from,
                                const clang::Stmt *to);

  void CreateStmtDependency(const clang::Stmt *from,
                            const clang::NamedDecl *to);

  void CreateStmtDependency(const clang::Stmt *from,
                            const clang::DeclRefExpr *to);

  void CreateStmtDependency(const clang::Stmt *from,
                            const clang::Expr *to);

  void CreateDependency(const clang::NamedDecl *from,
                        const clang::DeclRefExpr *to, const clang::Stmt *loc);

  void CreateDependency(const clang::NamedDecl *from, const clang::Expr *to,
                        const clang::Stmt *loc);

  void SetDependencies(VarStmtDependenciesMap dependencies) {
    LifetimeDependencies = std::move(dependencies);
  }

  void ProcessShortestLifetimes();

  llvm::DenseMap<const clang::NamedDecl *,
                 llvm::DenseSet<const clang::NamedDecl *>>
  TransposeDependencies();
  std::vector<const clang::NamedDecl *> InitializeWorklist() const;

  std::string DebugString();

 private:
  VariableLifetimesVector VariableLifetimes;
  // AddrOf -> $local (no need to store anything)
  ExprLifetimesVector ExprLifetimes;
  VarStmtDependenciesMap LifetimeDependencies;
  ExprStmtDependenciesMap ExprLifetimeDependencies;
  // ? can one stmt point to more than one var_decl?
  StmtVarDependenciesMap StmtDependencies;
  StmtExprDependenciesMap StmtExprDependencies;
  PointsToMap PointsTo;
  ObjectsLifetimes ReturnLifetime;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;
};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
