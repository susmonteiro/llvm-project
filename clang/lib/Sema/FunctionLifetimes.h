#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include "LifetimeTypes.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"

namespace clang {

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(/* bool elision_enabled, */ const clang::FunctionDecl *func/* ,
            LifetimeSymbolTable &symbol_table */)
        : /* elision_enabled(elision_enabled), */ func(func)/* ,
          symbol_table(symbol_table) */ {}

  /* virtual ~FunctionLifetimeFactory() {}

  virtual llvm::Expected<ValueLifetimes> CreateThisLifetimes(
      clang::QualType type, const clang::Expr* lifetime_name) const = 0;

  // Note: The `type_loc` parameter passed into `CreateParamLifetimes` and
  // `CreateReturnLifetimes` may be null if no type location is available.

  virtual llvm::Expected<ValueLifetimes> CreateParamLifetimes(
      clang::QualType type, clang::TypeLoc type_loc) const = 0;

  // * the method to create the return lifetime depends on the parameter
  // lifetimes

  virtual llvm::Expected<ValueLifetimes> CreateReturnLifetimes(
      clang::QualType type, clang::TypeLoc type_loc,
      const llvm::SmallVector<ValueLifetimes>& param_lifetimes,
      const std::optional<ValueLifetimes>& this_lifetimes) const = 0; */

 private:
  bool elision_enabled;
  const clang::FunctionDecl *func;
  /* LifetimeSymbolTable& symbol_table; */
};

class FunctionLifetimes {
 public:
  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  llvm::SmallVector<ValueLifetimes> param_lifetimes_;
  ValueLifetimes return_lifetimes_;
  // std::optional<ValueLifetimes> this_lifetimes_;
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_