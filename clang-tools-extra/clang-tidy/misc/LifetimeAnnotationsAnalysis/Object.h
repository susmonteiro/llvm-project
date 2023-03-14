// TODO change me
#ifndef DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_H_
#define DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_H_

#include <atomic>
#include <functional>
#include <string>

#include "Lifetime.h"
// TODO uncomment
// #include "lifetime_annotations/function_lifetimes.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/Hashing.h"

namespace clang {
namespace tidy {
namespace lifetimes {

// TODO check class
// TODO implement class
// TODO create Object.cpp and implement functions

// Any object that has a lifetime. Multiple objects might have the same
// lifetime, but two equal objects always have the same lifetime.
// An object may also represent a known function (obtainable by GetFunc) or an
// unknown function whose lifetime signature is known (obtainable by
// GetFuncLifetimes), but not both.
class Object {
public:
  Object(const Object &) = delete;
  Object(Object &&) = delete;
  Object &operator=(const Object &) = delete;
  Object &operator=(Object &&) = delete;

  // Creates an object with the given lifetime and type.
  // This constructor should only be used in tests. Outside of tests, use
  // one of the ObjectRepository::CreateObject...() functions.
  Object(Lifetime lifetime, clang::QualType type);

  // Creates an object representing a declared function.
  // This constructor should only be used in tests. Outside of tests, use
  // one of the ObjectRepository::CreateObject...() functions.
  Object(const clang::FunctionDecl &func);

  // Returns the lifetime of the object.
  Lifetime GetLifetime() const { return lifetime_; }

  clang::QualType Type() const { return type_; }

  // Returns a textual representation of the object for debug logging.
  std::string DebugString() const;

  // TODO implement func
  // Returns the function that this object represents, if any.
  //   const clang::FunctionDecl *GetFunc() const { return func_; }

  // Returns the lifetimes of function that this object represents, if known;
  // note that lifetimes may not be known even if GetFunc() returns non-null.
  // TODO implement this
  //   const std::optional<FunctionLifetimes> &GetFuncLifetimes() const {
  //     return func_lifetimes_;
  //   }

  // Assigns the given function lifetimes to this object, declaring that this is
  // an object that represents a callable with these lifetimes.
  // TODO implement this
  //   void SetFuncLifetimes(const FunctionLifetimes &func_lifetimes) {
  //     func_lifetimes_ = func_lifetimes;
  //   }

private:
  Lifetime lifetime_;
  clang::QualType type_;
  // TODO implement function
  //   const clang::FunctionDecl *func_;
  // TODO uncomment this (or erase)
  //   std::optional<FunctionLifetimes> func_lifetimes_;
};

std::ostream &operator<<(std::ostream &os, Object object);

} // namespace lifetimes
} // namespace tidy
} // namespace clang

#endif // DEVTOOLS_RUST_CC_INTEROP_LIFETIME_ANALYSIS_OBJECT_H_
