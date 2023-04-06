#ifndef LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_

#include "clang/AST/Type.h"

namespace clang {
class ValueLifetimes {
 public:
  // Creates an invalid ValueLifetimes, which should not be used. This is
  // provided only for usage with functions with output parameters.

  ValueLifetimes() : ValueLifetimes(clang::QualType()) {}

  ValueLifetimes(const ValueLifetimes& other);

//   ValueLifetimes& operator=(const ValueLifetimes& other);

  ~ValueLifetimes();

 private:
 explicit ValueLifetimes(clang::QualType type);

 clang::QualType type_;
};
}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_TYPE_LIFETIMES_H_