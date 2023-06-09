#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Error.h"

namespace clang {

// the lifetime of a variable can be $static, $local or $c, where c is a char
class ObjectsLifetimes {
 public:
  ObjectsLifetimes() {}
  ObjectsLifetimes(Lifetime lifetime) {
    InsertPointeeObject(lifetime);
  }
  // ObjectsLifetimes(Lifetime lifetime, clang::QualType& type) {
  //   InsertPointeeObject(lifetime, type);
  // }

  Lifetime& GetLifetime(clang::QualType& type) {
    type = type.getCanonicalType();
    for (auto& pointee : IndirectionLifetimes) {
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

  llvm::SmallVector<Lifetime>& GetLifetimes() { return IndirectionLifetimes; }

  void InsertPointeeObject(Lifetime& lifetime) {
    IndirectionLifetimes.emplace_back(lifetime);
  }

  // void InsertPointeeObject(Lifetime& lifetime, clang::QualType& type) {
  //   IndirectionLifetimes.emplace_back(lifetime, type);
  // }

  void InsertPointeeObject(char id, clang::QualType& type) {
    IndirectionLifetimes.emplace_back(id, type);
  }

  Lifetime& InsertPointeeObject(clang::QualType& type) {
    IndirectionLifetimes.emplace_back(type);
    return GetLifetime(type);
  }

  std::string DebugString() const {
    std::string res = "[ObjectsLifetimes]:\n";
    for (auto& pointee : IndirectionLifetimes) {
      res += pointee.DebugString();
    }
    return res;
  }

  bool IsLifetimeNotSet() {
    for (auto& pointee : IndirectionLifetimes) {
      if (pointee.IsNotSet()) {
        return true;
      }
    }
    return false;
  }

 private:
  llvm::SmallVector<Lifetime> IndirectionLifetimes;
};
}  // namespace clang

#endif
