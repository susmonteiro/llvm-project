#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Error.h"

namespace clang {

class ObjectsLifetimes {
 public:
  ObjectsLifetimes() {}
  ObjectsLifetimes(Lifetime lifetime) { InsertPointeeObject(lifetime); }

  Lifetime& GetLifetime(clang::QualType& type) {
    type = type.getCanonicalType();
    // debugLifetimes("The type we want is " + type.getAsString() + '\n');
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto& tmp_type = tmp.value();
      // debugLifetimes("The type found is ", tmp_type.getAsString());
      if (tmp_type == type) {
        // debugLifetimes("They are the same!");
        return pointee;
      }
    }
    // TODO error?
    return InsertPointeeObject(type);
  }

  Lifetime& GetLifetimeOrLocal(clang::QualType& type) {
    type = type.getCanonicalType();
    // debugLifetimes("The type we want is " + type.getAsString() + '\n');
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto& tmp_type = tmp.value();
      // debugLifetimes("The type found is ", tmp_type.getAsString());
      if (tmp_type == type) {
        // debugLifetimes("They are the same!");
        return pointee;
      }
    }
    debugLifetimes(
        "Before creating local lifetime for type" + type.getAsString(),
        DebugString());
    Lifetime& lifetime = InsertPointeeObject(Lifetime(LOCAL, type));
    debugLifetimes(
        "After creating local lifetime for type" + type.getAsString(),
        DebugString());
    return lifetime;
  }

  llvm::SmallVector<Lifetime>& GetLifetimes() { return PointeeObjects; }

  Lifetime& InsertPointeeObject(Lifetime lifetime) {
    assert(lifetime.GetType().has_value());
    debugLifetimes("Before insert lifetime", DebugString());
    debugLifetimes("And its size is", PointeeObjects.size());
    PointeeObjects.push_back(lifetime);
    debugLifetimes("After insert lifetime", DebugString());
    debugLifetimes("And its size is", PointeeObjects.size());

    // return new_lifetime;
    clang::QualType type = lifetime.GetType().value();
    return GetLifetime(type);
  }

  Lifetime& InsertPointeeObject(clang::QualType type) {
    type = type.getCanonicalType();
    PointeeObjects.push_back(type);
    return GetLifetime(type);
  }

  std::string DebugString() const {
    std::string res = "[ObjectsLifetimes]:\n";
    for (auto& pointee : PointeeObjects) {
      res += pointee.DebugString();
    }
    return res;
  }

  bool IsLifetimeNotSet() {
    for (auto& pointee : PointeeObjects) {
      if (pointee.IsNotSet()) {
        return true;
      }
    }
    return false;
  }

 private:
  llvm::SmallVector<Lifetime> PointeeObjects;
};
}  // namespace clang

#endif
