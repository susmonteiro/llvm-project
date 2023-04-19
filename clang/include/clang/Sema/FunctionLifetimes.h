#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include <iostream>

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using LifetimeFactory =
    std::function<llvm::Expected<Lifetime>(const clang::Expr *)>;

// TODO remove this
using MaybeLifetime = std::optional<Lifetime>;

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func)
      : /* elision_enabled(elision_enabled), */ func(func) {}

  //   virtual ~FunctionLifetimeFactory() {}

  llvm::Expected<Lifetime> CreateParamLifetimes(
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

// Holds information about each function
class FunctionLifetimes {
 public:
  FunctionLifetimes();
  FunctionLifetimes(int func_id) : func_id_(func_id) {}
  // * Returns lifetimes for the `i`-th parameter.
  // * These are the same number and order as FunctionDecl::parameters()

  int Id() { return func_id_; }

  // Returns the number of function parameters (excluding the implicit `this).
  size_t GetNumParams() const { return params_.size(); }

  llvm::DenseMap<const clang::ParmVarDecl*, Lifetime> GetParamsLifetimes() const {
    return params_lifetimes_;
  }

  Lifetime GetReturnLifetime() { return return_lifetime_; }
  void SetReturnLifetime(Lifetime l) { return_lifetime_ = l; }

  // FIXME
  // bool CheckIfLifetimeIsDefined(Lifetime l) {
  //   return lifetimes_id_set_.find(l.Id()) != lifetimes_id_set_.end();
  // }

  void InsertParamLifetime(const clang::ParmVarDecl *param, Lifetime l) { 
    params_.emplace_back(param);
    params_lifetimes_[param] = l;
   }

  void DumpParameters() const {
    std::cout << "[FunctionLifetimes]: Parameters Lifetimes\n";
    int i = 0;
    for (const auto &pair: params_lifetimes_) {
      debugLifetimes("Parameter ", i++);
      const Lifetime *l = &pair.second;
      debugLifetimes(l->DebugString());
    }
  }

  void DumpReturn() const {
    std::cout << "[FunctionLifetimes]: Return lifetimes\n";
    debugLifetimes("Lifetime", return_lifetime_.DebugString());
  }

  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  // ? is it better to have optionals or a Lifetime that basically means no
  // lifetime
  // stores param lifetimes in order
  // TODO choose
  // std::vector<MaybeLifetime> params_lifetimes_;
  std::vector<const clang::ParmVarDecl*> params_;
  llvm::DenseMap<const clang::ParmVarDecl*, Lifetime> params_lifetimes_;
  Lifetime return_lifetime_;
  int func_id_;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

  // TODO need DenseMapInfo?
  // static FunctionLifetimes InvalidEmpty();
  // static FunctionLifetimes InvalidTombstone();
  // friend class llvm::DenseMapInfo<FunctionLifetimes, void>;

  static llvm::Expected<FunctionLifetimes> Create(
      const clang::FunctionDecl *func, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

// TODO need DenseMapInfo?
// namespace llvm {

// template <>
// struct DenseMapInfo<clang::FunctionLifetimes, void> {
//   static clang::FunctionLifetimes getEmptyKey() {
//     return clang::FunctionLifetimes::InvalidEmpty();
//   }

//   static clang::FunctionLifetimes getTombstoneKey() {
//     return clang::FunctionLifetimes::InvalidTombstone();
//   }

//   static unsigned getHashValue(clang::FunctionLifetimes functionLifetimes) {
//     return llvm::hash_value(functionLifetimes.Id());
//   }

//   static bool isEqual(clang::FunctionLifetimes lhs,
//                       clang::FunctionLifetimes rhs) {
//     return lhs.Id() == rhs.Id();
//   }
// };

// }  // namespace llvm

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_