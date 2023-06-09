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
  ObjectsLifetimes(Lifetime lifetime, clang::QualType &type) {
    InsertPointeeObject(lifetime, type);
  }

  Lifetime& GetLifetime(clang::QualType &type) {
    type = type.getCanonicalType();
    // debugLifetimes("The type we want is " + type.getAsString() + '\n');
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto &tmp_type = tmp.value();
      // debugLifetimes("The type found is ", tmp_type.getAsString());
      if (tmp_type == type) {
        // debugLifetimes("They are the same!");
        return pointee;
      }
    }
    // TODO error?
    return InsertPointeeObject(type);
  }

  llvm::SmallVector<Lifetime>& GetLifetimes() { return PointeeObjects; }

  void InsertPointeeObject(Lifetime& lifetime, clang::QualType& type) {
    lifetime.SetType(type);
    lifetime.DebugString();
    PointeeObjects.emplace_back(lifetime);
  }

  Lifetime& InsertPointeeObject(clang::QualType& type) {
    PointeeObjects.emplace_back(type);
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
    for (auto &pointee : PointeeObjects) {
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
