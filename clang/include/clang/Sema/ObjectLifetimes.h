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

  Lifetime& GetLifetimeWithSameNumberIndirections(clang::QualType type) {
    return GetLifetime(Lifetime::GetNumIndirections(type));
  }

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

  Lifetime& GetLifetime(unsigned int num_indirections) {
    for (auto& pointee : PointeeObjects) {
      if (pointee.GetNumIndirections() == num_indirections) {
        return pointee;
      }
    }

    if (num_indirections == 0) {
      return InsertPointeeObject(Lifetime(LOCAL, num_indirections));
    }
    return InsertPointeeObject(Lifetime(NOTSET, num_indirections));
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

  bool HasLifetimeLocal() {
    for (Lifetime& lifetime : PointeeObjects) {
      if (lifetime.IsLocal()) return true;
    }
    return false;
  }

  bool HasLifetime(char id) {
    for (Lifetime& lifetime : PointeeObjects) {
      if (lifetime.GetId() == id) return true;
    }
    return false;
  }

  Lifetime& InsertPointeeObject(Lifetime lifetime) {
    PointeeObjects.emplace_back(lifetime);
    clang::QualType type = lifetime.GetType();
    if (type.isNull()) {
      return GetLifetime(lifetime.GetNumIndirections());
    }
    return GetLifetime(type);
  }

  Lifetime& InsertPointeeObject(clang::QualType type) {
    PointeeObjects.emplace_back(type);
    return GetLifetime(type);
  }

  std::string DebugString() const {
    std::string res = "[ObjectLifetimes]:\n";
    for (auto& pointee : PointeeObjects) {
      res += pointee.DebugString();
    }
    return res;
  }

 private:
  llvm::SmallVector<Lifetime> PointeeObjects;
};
}  // namespace clang

#endif
