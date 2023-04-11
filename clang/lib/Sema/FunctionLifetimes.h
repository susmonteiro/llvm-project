#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include <iostream>

#include "Lifetime.h"
#include "PointeeType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

// DEBUG
#include "DebugLifetimes.h"

namespace clang {

using LifetimeFactory =
    std::function<llvm::Expected<Lifetime>(const clang::Expr *)>;

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func)
      : /* elision_enabled(elision_enabled), */ func(func) {}

  //   virtual ~FunctionLifetimeFactory() {}

  llvm::Expected<Lifetime> CreateParamLifetimesNew(
      clang::QualType param_type, clang::TypeLoc param_type_loc) const;

  llvm::Expected<Lifetime> CreateReturnLifetimes(
      clang::QualType return_type, clang::TypeLoc return_type_loc) const;

  static llvm::Expected<Lifetime> CreateLifetime(
      clang::QualType type, clang::TypeLoc type_loc,
      LifetimeFactory lifetime_factory);

  //   // Note: The `type_loc` parameter passed into `CreateParamLifetimes` and
  //   // `CreateReturnLifetimes` may be null if no type location is available.

  llvm::Expected<Lifetime> LifetimeFromName(const clang::Expr *name) const;

  LifetimeFactory ParamLifetimeFactory() const;
  LifetimeFactory ReturnLifetimeFactory() const;

 private:
  bool elision_enabled;
  const clang::FunctionDecl *func;
};

// Lifetimes for the signature of a function.
class FunctionLifetimes {
 public:
  // TODO 2 options: remove or different structures for params and other vars
  // * Returns lifetimes for the `i`-th parameter.
  // * These are the same number and order as FunctionDecl::parameters()
  // const ValueLifetimes &GetParamLifetimes(size_t i) const {
  //   return param_lifetimes_[i];
  // }

  // Returns the number of function parameters (excluding the implicit `this).
  // size_t GetNumParams() const { return param_lifetimes_.size(); }

  llvm::DenseMap<const clang::Decl*, Lifetime> GetVariableLifetimes() {
    return variable_lifetimes_;
  }

  llvm::DenseSet<char> GetDefinedLifetimes() {
    return lifetimes_id_set_;
  }

  Lifetime GetReturnLifetimes() {
    return return_lifetime_;
  }

  bool CheckIfLifetimeIsDefined(Lifetime l) {
    return lifetimes_id_set_.find(l.Id()) != lifetimes_id_set_.end();
  }


  void InsertVariableLifetime(const clang::Decl* decl, Lifetime l) {
    variable_lifetimes_[decl] = l;
    lifetimes_id_set_.insert(l.Id());
  } 

  void DumpParameters() const {
    std::cout << "[FunctionLifetimes]: Parameters Lifetimes\n";
    for (const auto &pair : variable_lifetimes_) {
      const clang::Decl *key = pair.first;
      Lifetime value = pair.second;
      key->dump();
      debugLifetimes("Lifetime", value.getLifetimeName());
    }
  }

  void DumpReturn() const {
    std::cout << "[FunctionLifetimes]: Return lifetimes\n";
    if (return_lifetime_.IsInvalid()) {
      debugLifetimes("Return value has no lifetime");
    } else {
      debugLifetimes("Lifetime", return_lifetime_.getLifetimeName());
    }
  }

  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  // TODO 2 options: remove or different structures for params and other vars
  // llvm::SmallVector<ValueLifetimes> param_lifetimes_;
  // TODO is it enough to store the id?
  // TODO separate parameters from all variables?
  llvm::DenseMap<const clang::Decl *, Lifetime> variable_lifetimes_;
  llvm::DenseSet<char> lifetimes_id_set_;
  Lifetime return_lifetime_;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

  static llvm::Expected<FunctionLifetimes> Create(
      const clang::FunctionProtoType *type, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_