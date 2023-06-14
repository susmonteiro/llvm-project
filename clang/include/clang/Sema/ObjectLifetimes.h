#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Error.h"

namespace clang {

class ObjectLifetimes {
 public:
  ObjectLifetimes() {}
  ObjectLifetimes(Lifetime lifetime) { InsertPointeeObject(lifetime); }

  Lifetime& GetLifetime(clang::QualType& type) {
    type = type.getCanonicalType();
    for (auto& pointee : PointeeObjects) {
      if (pointee.GetType() == type) {
        return pointee;
      }
    }
    // TODO error?
    return InsertPointeeObject(type);
  }

  Lifetime& GetLifetimeOrLocal(clang::QualType& type) {
    type = type.getCanonicalType();
    for (auto& pointee : PointeeObjects) {
      if (pointee.GetType() == type) {
        return pointee;
      }
    }
    Lifetime& lifetime = InsertPointeeObject(Lifetime(LOCAL, type));
    return lifetime;
  }

  llvm::SmallVector<Lifetime>& GetLifetimes() { return PointeeObjects; }

  Lifetime& InsertPointeeObject(Lifetime lifetime) {
    clang::QualType type = lifetime.GetType();
    debugLifetimes("Before emplace", DebugString());
    PointeeObjects.emplace_back(lifetime);
    debugLifetimes("After emplace", DebugString());
    return GetLifetime(type);
  }

  Lifetime& InsertPointeeObject(clang::QualType type) {
    debugLifetimes("Before emplace", DebugString());
    PointeeObjects.emplace_back(type);
    debugLifetimes("After emplace", DebugString());
    return GetLifetime(type);

  }

  std::string DebugString() const {
    std::string res = "[ObjectLifetimes]:\n";
    for (auto& pointee : PointeeObjects) {
      res += pointee.DebugString();
    }
    return res;
  }

  // bool IsLifetimeNotSet() {
  //   for (auto& pointee : PointeeObjects) {
  //     if (pointee.IsNotSet()) {
  //       return true;
  //     }
  //   }
  //   return false;
  // }

 private:
  llvm::SmallVector<Lifetime> PointeeObjects;
};
}  // namespace clang

#endif
