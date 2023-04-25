#ifndef LIFETIME_ANNOTATIONS_ANALYSIS_H_
#define LIFETIME_ANNOTATIONS_ANALYSIS_H_

#include <iostream>

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/FunctionLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
// #include "llvm/ADT/Optional.h"
// #include "llvm/ADT/SmallVector.h"
// #include "llvm/ADT/StringRef.h"
// #include "llvm/Support/Error.h"

// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using VariableLifetimesMap = llvm::DenseMap<const clang::NamedDecl *, Lifetime>;
using DependenciesMap = llvm::DenseMap<const clang::NamedDecl *,
                                    llvm::DenseSet<const clang::NamedDecl *>>;

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis();
  LifetimeAnnotationsAnalysis(FunctionLifetimes &function_info);

  VariableLifetimesMap &GetVariableLifetimes();
  DependenciesMap &GetDependencies();
  Lifetime &GetLifetime(const clang::NamedDecl *var_decl);
  Lifetime &GetReturnLifetime();

  bool IsLifetimeNotset(const clang::NamedDecl *var_decl) const {
    auto it = VariableLifetimes.find(var_decl);
    if (it != VariableLifetimes.end()) {
      return it->second.IsNotSet();
    } else {
      // TODO error?
      return false;
    }
  }

  llvm::DenseSet<char> GetShortestLifetimes(const clang::NamedDecl *var_decl) {
    return VariableLifetimes[var_decl].GetShortestLifetimes();
  }

  void InsertShortestLifetimes(const clang::NamedDecl *var_decl, Lifetime *l) {
    char id = l->GetId();
    VariableLifetimes[var_decl].InsertShortestLifetimes(id);
  }

  void PropagateShortestLifetimes(const clang::NamedDecl *target,
                                  llvm::DenseSet<char> shortest_lifetimes) {
    VariableLifetimes[target].InsertShortestLifetimes(shortest_lifetimes);
  }

  void PropagateShortestLifetimes(const clang::NamedDecl *to,
                                  const clang::NamedDecl *from) {
    const auto from_lifetimes = GetShortestLifetimes(from);
    PropagateShortestLifetimes(to, from_lifetimes);
  }

  void CreateVariable(const clang::NamedDecl *var_decl) {
    VariableLifetimes[var_decl] = Lifetime();
  }

  void CreateVariable(const clang::NamedDecl *var_decl, Lifetime lifetime) {
    VariableLifetimes[var_decl] = Lifetime(lifetime);
  }

  void CreateDependency(const clang::NamedDecl *from,
                        const clang::DeclRefExpr *to);
  void SetDependencies(DependenciesMap dependencies) {
    Dependencies = std::move(dependencies);
  }

  void ProcessShortestLifetimes();

  DependenciesMap TransposeDependencies() const;
  std::vector<const clang::NamedDecl *> InitializeWorklist() const;

  std::string DebugString();

 private:
  VariableLifetimesMap VariableLifetimes;
  DependenciesMap Dependencies;
  Lifetime ReturnLifetime;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_
