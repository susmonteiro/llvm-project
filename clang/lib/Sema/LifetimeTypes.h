#ifndef LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_

#include "clang/AST/Type.h"
#include "Lifetime.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"

namespace clang {

using LifetimeFactory =
    std::function<llvm::Expected<Lifetime>(const clang::Expr*)>;

// Strips any attributes off `type` and returns the result.
clang::QualType StripAttributes(clang::QualType type);

// Strips any attributes off `type_loc` and returns the result. The stripped
// attributes are added to `attrs`.
clang::TypeLoc StripAttributes(clang::TypeLoc type_loc,
                               llvm::SmallVector<const clang::Attr*>& attrs);

// Extracts and returns the arguments of any `annotate_type("lifetime", ...)`
// attributes in `attrs`.
// If `attrs` contains multiple such attributes, the arguments from all of
// these attributes are concatenated.
llvm::Expected<llvm::SmallVector<const clang::Expr*>> GetAttributeLifetimes(
    llvm::ArrayRef<const clang::Attr*> attrs);

// Extracts the lifetime parameters of the given type.
llvm::SmallVector<std::string> GetLifetimeParameters(clang::QualType type);

class ValueLifetimes {
 public:
  // Creates an invalid ValueLifetimes, which should not be used. This is
  // provided only for usage with functions with output parameters.

  ValueLifetimes() : ValueLifetimes(clang::QualType()) {}

  ValueLifetimes(const ValueLifetimes& other);

//   ValueLifetimes& operator=(const ValueLifetimes& other);

  ~ValueLifetimes();

  static void /* llvm::Expected<ValueLifetimes> */ Create(
      clang::QualType type, clang::TypeLoc type_loc,
      LifetimeFactory lifetime_factory);
  static void /* llvm::Expected<ValueLifetimes> */ Create(
      clang::QualType type, LifetimeFactory lifetime_factory) {
    return Create(type, clang::TypeLoc(), lifetime_factory);
  }

 private:
 explicit ValueLifetimes(clang::QualType type);

 clang::QualType type_;
};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_