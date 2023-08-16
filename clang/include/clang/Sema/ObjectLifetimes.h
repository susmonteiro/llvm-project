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
  ObjectLifetimes() { InsertPointeeObject(Lifetime(LOCAL, 0)); }
  ObjectLifetimes(Lifetime lifetime) {
    InsertPointeeObject(Lifetime(LOCAL, 0));
    InsertPointeeObject(lifetime);
  }
  ObjectLifetimes(bool is_static, unsigned int num_indirections)
      : IsStatic(is_static) {
    assert(is_static);
    while (num_indirections > 0) {
      InsertPointeeObject(Lifetime(STATIC, --num_indirections));
    }
  }

  Lifetime& GetLifetime(unsigned int num_indirections, char id) {
    if (num_indirections >= PointeeObjects.size()) {
      PointeeObjects.resize(num_indirections + 1);
    }
    Lifetime& lifetime = PointeeObjects[num_indirections];
    if (lifetime.IsNull()) {
      if (num_indirections == 0 && id == NOTSET) {
        return InsertPointeeObject(Lifetime(LOCAL, num_indirections));
      } else {
        return InsertPointeeObject(Lifetime(id, num_indirections));
      }
    }
    return lifetime;
  }

  Lifetime& GetLifetime(unsigned int num_indirections) {
    return GetLifetime(num_indirections, NOTSET);
  }

  Lifetime& GetLifetimeOrLocal(unsigned int num_indirections) {
    return GetLifetime(num_indirections, LOCAL);
  }

  Lifetime& GetLifetime(clang::QualType& type) {
    unsigned int num_indirections = Lifetime::GetNumIndirections(type);
    return GetLifetime(num_indirections);
  }

  Lifetime& GetLifetimeOrLocal(clang::QualType& type) {
    unsigned int num_indirections = Lifetime::GetNumIndirections(type);
    return GetLifetimeOrLocal(num_indirections);
  }

  llvm::SmallVector<Lifetime>& GetLifetimes() { return PointeeObjects; }

  bool HasLifetimeLocal() {
    for (Lifetime& lifetime : PointeeObjects) {
      if (lifetime.IsLocal() && lifetime.GetNumIndirections() != 0) return true;
    }
    return false;
  }

  bool HasLifetimeStatic() {
    for (Lifetime& lifetime : PointeeObjects) {
      if (lifetime.IsStatic() && lifetime.GetNumIndirections() != 0)
        return true;
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
    unsigned int num_indirections = lifetime.GetNumIndirections();
    if (num_indirections >= PointeeObjects.size()) {
      PointeeObjects.resize(num_indirections + 1);
    }
    if (IsStatic) {
      PointeeObjects[num_indirections] = Lifetime(STATIC, num_indirections);

    } else {
      PointeeObjects[num_indirections] = lifetime;
    }
    return PointeeObjects[num_indirections];
  }

  Lifetime& InsertPointeeObject(clang::QualType type) {
    unsigned num_indirections = Lifetime::GetNumIndirections(type);
    if (num_indirections >= PointeeObjects.size()) {
      PointeeObjects.resize(num_indirections + 1);
    }
    if (IsStatic) {
      PointeeObjects[num_indirections] = Lifetime(STATIC, num_indirections);
    } else {
      PointeeObjects[num_indirections] = Lifetime(NOTSET, type);
    }
    return PointeeObjects[num_indirections];
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
  bool IsStatic = false;
};
}  // namespace clang

#endif
