#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include "LifetimeSymbolTable.h"
#include "LifetimeTypes.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

namespace clang {

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func,
      LifetimeSymbolTable &symbol_table)
      : /* elision_enabled(elision_enabled), */ func(func),
        symbol_table(symbol_table) {}

  //   virtual ~FunctionLifetimeFactory() {}

  //   virtual llvm::Expected<ValueLifetimes> CreateThisLifetimes(
  //       clang::QualType type, const clang::Expr* lifetime_name) const = 0;

  //   // Note: The `type_loc` parameter passed into `CreateParamLifetimes` and
  //   // `CreateReturnLifetimes` may be null if no type location is available.

  llvm::Expected<Lifetime> LifetimeFromName(const clang::Expr *name) const;

  llvm::Expected<ValueLifetimes> CreateParamLifetimes(
      clang::QualType param_type, clang::TypeLoc param_type_loc) const;

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
  static void /* llvm::Expected<FunctionLifetimes> */ CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  llvm::SmallVector<ValueLifetimes> param_lifetimes_;
  ValueLifetimes return_lifetimes_;
  // std::optional<ValueLifetimes> this_lifetimes_;

  static void /* llvm::Expected<FunctionLifetimes> */ Create(
      const clang::FunctionProtoType *type, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_