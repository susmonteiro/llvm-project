#ifndef LIFETIME_ANNOTATIONS_ANALYSIS_H_
#define LIFETIME_ANNOTATIONS_ANALYSIS_H_

#include <iostream>

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using VariableLifetimes = llvm::DenseMap<const clang::NamedDecl *, Lifetime>;
using Dependencies =
    llvm::DenseMap<const clang::NamedDecl *, llvm::DenseSet<const clang::NamedDecl *>>;

// TODO refactor this (copy from FunctionLifetimes)

// Holds the state and function used during the analysis of a function
class LifetimeAnnotationsAnalysis {
 public:
  LifetimeAnnotationsAnalysis() {}
  LifetimeAnnotationsAnalysis(
      llvm::DenseMap<const clang::ParmVarDecl *, Lifetime> params_lifetimes) {
    for (auto &pair : params_lifetimes) {
      variable_lifetimes_.insert({pair.first, pair.second});
    }
  }

  // TODO 2 options: remove or different structures for params and other vars
  // * Returns lifetimes for the `i`-th parameter.
  // * These are the same number and order as FunctionDecl::parameters()
  // const ValueLifetimes &GetParamLifetimes(size_t i) const {
  //   return param_lifetimes_[i];
  // }

  // Returns the number of function parameters (excluding the implicit `this).
  // size_t GetNumParams() const { return param_lifetimes_.size(); }

  VariableLifetimes GetVariableLifetimes() { return variable_lifetimes_; }

  Dependencies& GetDependencies() { return dependencies_; }

  Lifetime &GetLifetime(const clang::VarDecl *var_decl) {
    VariableLifetimes::iterator it = variable_lifetimes_.find(var_decl);
    if (it == variable_lifetimes_.end()) {
      // TODO error
      CreateVariable(var_decl);
    }
    Lifetime &l = variable_lifetimes_[var_decl];
    return l;
  }

  void CreateVariable(const clang::VarDecl *var_decl) {
    variable_lifetimes_[var_decl] = Lifetime();
  }

  void CreateVariable(const clang::VarDecl *var_decl, Lifetime lifetime) {
    variable_lifetimes_[var_decl] = Lifetime(lifetime);
  }

  void CreateDependency(const clang::VarDecl *from, const clang::DeclRefExpr *to);

  Dependencies TransposeDependencies() const;
  std::vector<const clang::NamedDecl*> InitializeWorklist() const;

  
  // llvm::DenseSet<char> GetDefinedLifetimes() { return lifetimes_id_set_; }

  // Lifetime GetReturnLifetimes() { return return_lifetime_; }

  // bool CheckIfLifetimeIsDefined(Lifetime l) {
  //   return lifetimes_id_set_.find(l.Id()) != lifetimes_id_set_.end();
  // }

  // void InsertVariableLifetime(const clang::Decl *decl, Lifetime l) {
  //   variable_lifetimes_[decl] = l;
  //   lifetimes_id_set_.insert(l.Id());
  // }

  // void DumpParameters() const {
  //   std::cout << "[FunctionLifetimes]: Parameters Lifetimes\n";
  //   for (const auto &pair : variable_lifetimes_) {
  //     const clang::Decl *key = pair.first;
  //     Lifetime value = pair.second;
  //     key->dump();
  //     debugLifetimes("Lifetime", value.getLifetimeName());
  //   }
  // }

  // void DumpReturn() const {
  //   std::cout << "[FunctionLifetimes]: Return lifetimes\n";
  //   if (return_lifetime_.IsUnset()) {
  //     debugLifetimes("Return value has no lifetime");
  //   } else {
  //     debugLifetimes("Lifetime", return_lifetime_.getLifetimeName());
  //   }
  // }

  // static llvm::Expected<FunctionLifetimes> CreateForDecl(
  //     const clang::FunctionDecl *function,
  //     const FunctionLifetimeFactory &lifetime_factory);

  std::string DebugString() {
    std::string str = "[LifetimeAnnotationsAnalysis]\n";
    str += ">> variable_lifetimes_\n";
    for (const auto &pair : variable_lifetimes_) {
      str += pair.first->getNameAsString() + ": " +
             pair.second.getLifetimeName() + '\n';
    }
    str += ">> dependencies_\n";
    for (const auto &pair : dependencies_) {
      str += pair.first->getNameAsString() + ": ";
      for (const auto &var : pair.second) {
        str += var->getNameAsString() + ' ';
      }
      str += '\n';
    }
    // TODO dependencies
    return str;
  }

 private:
  VariableLifetimes variable_lifetimes_;
  // llvm::DenseSet<char> lifetimes_id_set_;
  // ? needed or enough in FunctionLifetimes
  // Lifetime return_lifetime_;
  Dependencies dependencies_;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

  //   static llvm::Expected<FunctionLifetimes> Create(
  //       const clang::FunctionProtoType *type, clang::TypeLoc type_loc,
  //       const clang::QualType this_type,
  //       const FunctionLifetimeFactory &lifetime_factory);
};

// friend std::ostream& operator<<(std::ostream& os, LifetimeAnnotationsAnalysis
// analysis) {
//   os << "[LifetimeAnnotationsAnalysis]\n";
//   os << ""
// }

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_ANALYSIS_H_