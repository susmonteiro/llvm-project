#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"

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
  ObjectLifetime(Lifetime &lifetime, clang::QualType& type)
      : ThisLifetime(lifetime),
        ThisType(std::optional<clang::QualType>(type)),
        PointeeObject(nullptr) {}
  ObjectLifetime(Lifetime &lifetime, clang::QualType& type, ObjectLifetime* pointee)
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
    std::string res = "Type: ";
    res +=
        ThisType.has_value() ? ThisType.value().getAsString() : "unknown type";
    res += "\t\t\t[Lifetime]: " + ThisLifetime.DebugString() + "\n";
    if (PointeeObject) {
      res += PointeeObject->DebugString();
    }
    return res;
  }

 private:
  Lifetime ThisLifetime;
  std::optional<clang::QualType> ThisType;
  ObjectLifetime* PointeeObject;
};
}  // namespace clang

#endif
