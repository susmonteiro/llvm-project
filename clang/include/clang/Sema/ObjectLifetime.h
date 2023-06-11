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
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto& tmp_type = tmp.value();
      if (tmp_type == type) {
        return pointee;
      }
    }
    // TODO error?
    return InsertPointeeObject(type);
  }

  Lifetime& GetLifetimeOrLocal(clang::QualType& type) {
    type = type.getCanonicalType();
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto& tmp_type = tmp.value();
      if (tmp_type == type) {
        return pointee;
      }
    }
    Lifetime& lifetime = InsertPointeeObject(Lifetime(LOCAL, type));
    return lifetime;
  }

  llvm::SmallVector<Lifetime>& GetLifetimes() { return PointeeObjects; }

  Lifetime& InsertPointeeObject(Lifetime lifetime) {
    assert(lifetime.GetType().has_value());
    return PointeeObjects.emplace_back(lifetime);
  }

  Lifetime& InsertPointeeObject(clang::QualType type) {
    return PointeeObjects.emplace_back(type);
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
