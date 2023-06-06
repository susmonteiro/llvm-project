#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Error.h"

namespace clang {

// the lifetime of a variable can be $static, $local or $c, where c is a char
class ObjectLifetime {
 public:
  ObjectLifetime()
      : ThisLifetime(), ThisType(std::nullopt), PointeeObject(nullptr) {}
  ObjectLifetime(clang::QualType& type)
      : ThisLifetime(),
        ThisType(std::optional<clang::QualType>(type)),
        PointeeObject(nullptr) {}
  ObjectLifetime(Lifetime& lifetime, clang::QualType& type)
      : ThisLifetime(lifetime),
        ThisType(std::optional<clang::QualType>(type)),
        PointeeObject(nullptr) {}
  ObjectLifetime(Lifetime& lifetime, clang::QualType& type,
                 ObjectLifetime* pointee)
      : ThisLifetime(lifetime),
        ThisType(std::optional<clang::QualType>(type)),
        PointeeObject(pointee) {}

  // ObjectLifetime(const Lifetime& thisLifetime) : ThisLifetime(thisLifetime)
  // {} ObjectLifetime(llvm::StringRef name) : ThisLifetime(name) {}
  // ObjectLifetime(char id) : ThisLifetime(id) {}

  bool HasPointeeObject() { return PointeeObject != nullptr; }

  ObjectLifetime& GetPointeeObject() { return *PointeeObject; }

  Lifetime& GetLifetime() { return ThisLifetime; }
  Lifetime& GetPointeeObjectLifetime() { return PointeeObject->GetLifetime(); }
  std::optional<clang::QualType> GetType() { return ThisType; }
  std::optional<clang::QualType> GetPointeeType() {
    return HasPointeeObject() ? PointeeObject->GetType() : std::nullopt;
  }

  void SetLifetime(Lifetime& lifetime) { ThisLifetime = lifetime; }
  void SetPointeeLifetime(Lifetime& lifetime) {
    PointeeObject->SetLifetime(lifetime);
  }
  void SetType(std::optional<clang::QualType> type) { ThisType = type; }

  void SetPointee(ObjectLifetime& objectLifetime) {
    SetPointeeLifetime(objectLifetime.GetLifetime());
    PointeeObject->SetType(objectLifetime.GetType());
    if (objectLifetime.HasPointeeObject()) {
      PointeeObject->SetPointee(objectLifetime.GetPointeeObject());
    }
  }

  std::string DebugString() const {
    std::string res = "\t[Type]: ";
    res +=
        ThisType.has_value() ? ThisType.value().getAsString() : "unknown type";
    res += "\n\t[Lifetime]: " + ThisLifetime.DebugString() + "\n";
    return res;
  }

 private:
  Lifetime ThisLifetime;
  std::optional<clang::QualType> ThisType;
  ObjectLifetime* PointeeObject;
};

class ObjectsLifetimes {
 public:
  ObjectsLifetimes() : ThisLifetime() {}
  ObjectsLifetimes(Lifetime lifetime, clang::QualType &type) {
    InsertPointeeObject(lifetime, type);
  }

  Lifetime& GetLifetime() { return ThisLifetime.GetLifetime(); }

  Lifetime& GetLifetime(clang::QualType &type) {
    type = type.getCanonicalType();
    debugLifetimes("The type we want is " + type.getAsString() + '\n');
    for (auto& pointee : PointeeObjects) {
      auto tmp = pointee.GetType();
      if (!tmp.has_value()) {
        debugWarn("The type is not set\n");
        continue;
      }
      auto &tmp_type = tmp.value();
      debugLifetimes("The type found is ", tmp_type.getAsString());
      if (tmp_type == type) {
        debugLifetimes("They are the same!");
        return pointee.GetLifetime();
      }
    }
    // TODO error?
    return InsertPointeeObject(type);
  }

  llvm::SmallVector<ObjectLifetime>& GetLifetimes() { return PointeeObjects; }


  void SetLifetime(Lifetime& lifetime, clang::QualType& type) {
    ThisLifetime = ObjectLifetime(lifetime, type);
  }

  void InsertPointeeObject(Lifetime& lifetime, clang::QualType& type) {
    PointeeObjects.emplace_back(lifetime, type);
  }

  Lifetime& InsertPointeeObject(clang::QualType& type) {
    PointeeObjects.emplace_back(type);
    return GetLifetime(type);
  }

  std::string DebugString() const {
    std::string res = "[ObjectsLifetimes]";
    res += var_decl != nullptr ? " of " + var_decl->getNameAsString() : "";
    res += ":\n";

    for (auto& pointee : PointeeObjects) {
      res += pointee.DebugString();
    }
    return res;
  }

  bool IsLifetimeNotSet() {
    for (auto &pointee : PointeeObjects) {
      if (pointee.GetLifetime().IsNotSet()) {
        return true;
      }
    }
    return false;
  }

 private:
  llvm::SmallVector<ObjectLifetime> PointeeObjects;
  // "main" lifetime -> remove this
  ObjectLifetime ThisLifetime;
  // TODO needed?
  const clang::VarDecl* var_decl;
};
}  // namespace clang

#endif
