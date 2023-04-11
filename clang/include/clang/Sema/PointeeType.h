#ifndef LIFETIME_ANNOTATIONS_POINTEE_TYPE_H_
#define LIFETIME_ANNOTATIONS_POINTEE_TYPE_H_

#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"

namespace clang {

// If `type` is a pointer or reference type, returns the type of its pointee.
// Otherwise, returns a null type.
// Unlike `type->getPointeeType()`, this returns a null type if `type`, though
// it has a pointee type, is not a type for which we infer lifetimes, such as
// a pointer-to-member type. In other words, this function can be used to
// succinctly answer the question "does `type` have pointee type and do we infer
// lifetimes for it".
clang::QualType PointeeType(clang::QualType type);

// Analogous to `PointeeType` but operates on a `TypeLoc`.
clang::TypeLoc PointeeTypeLoc(clang::TypeLoc type_loc);

}  // namespace lifetimes

#endif  // LIFETIME_ANNOTATIONS_POINTEE_TYPE_H_
