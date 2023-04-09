#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include <iostream>

#include "LifetimeNew.h"
#include "LifetimeSymbolTable.h"
#include "LifetimeTypes.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

// TODO remove this
#include "DebugLifetimes.h"

namespace clang {

using LifetimeFactory =
    std::function<llvm::Expected<LifetimeNew>(const clang::Expr *)>;

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func,
      LifetimeSymbolTable &symbol_table)
      : /* elision_enabled(elision_enabled), */ func(func),
        symbol_table(symbol_table) {}

  //   virtual ~FunctionLifetimeFactory() {}

  // llvm::Expected<ValueLifetimes> CreateParamLifetimes(
  //     clang::QualType param_type, clang::TypeLoc param_type_loc) const;

  llvm::Expected<LifetimeNew> CreateParamLifetimesNew(
      clang::QualType param_type, clang::TypeLoc param_type_loc) const;

    static llvm::Expected<LifetimeNew> CreateLifetime(
      clang::QualType type, clang::TypeLoc type_loc,
      LifetimeFactory lifetime_factory);

  //   virtual llvm::Expected<ValueLifetimes> CreateThisLifetimes(
  //       clang::QualType type, const clang::Expr* lifetime_name) const = 0;

  //   // Note: The `type_loc` parameter passed into `CreateParamLifetimes` and
  //   // `CreateReturnLifetimes` may be null if no type location is available.

  LifetimeSymbolTable &getLifetimeSymbolTable() const { return symbol_table; }

  llvm::Expected<LifetimeNew> LifetimeFromName(const clang::Expr *name) const;

  //   // * the method to create the return lifetime depends on the parameter
  //   // lifetimes

  //   virtual llvm::Expected<ValueLifetimes> CreateReturnLifetimes(
  //       clang::QualType type, clang::TypeLoc type_loc,
  //       const llvm::SmallVector<ValueLifetimes>& param_lifetimes,
  //       const std::optional<ValueLifetimes>& this_lifetimes) const = 0;

  LifetimeFactory ParamLifetimeFactory() const;

 private:
  bool elision_enabled;
  const clang::FunctionDecl *func;
  LifetimeSymbolTable &symbol_table;
};

// Lifetimes for the signature of a function.
class FunctionLifetimes {
 public:
  // * Returns lifetimes for the `i`-th parameter.
  // * These are the same number and order as FunctionDecl::parameters()
  // const ValueLifetimes &GetParamLifetimes(size_t i) const {
  //   return param_lifetimes_[i];
  // }

  // Returns the number of function parameters (excluding the implicit `this).
  // size_t GetNumParams() const { return param_lifetimes_.size(); }

  void DumpParameters() const {
    std::cout << "[FunctionLifetimes]: Parameters Lifetimes\n";
    for (const auto &pair : params_lifetimes_new) {
      const clang::Decl* key = pair.first;
      LifetimeNew value = pair.second;
      key->dump();
      debugLifetimes("Lifetime", value.Id());
    }
  }

  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  // llvm::SmallVector<ValueLifetimes> param_lifetimes_;
  // TODO is it enough to store the id?
  llvm::DenseMap<const clang::Decl *, LifetimeNew> params_lifetimes_new;
  // ValueLifetimes return_lifetimes_;
  // llvm::DenseMap<clang::Decl, LifetimeNew> initial_object_lifetimes_;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

  static llvm::Expected<FunctionLifetimes> Create(
      const clang::FunctionProtoType *type, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_