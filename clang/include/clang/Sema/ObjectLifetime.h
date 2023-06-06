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
  ObjectLifetime() : ThisLifetime() {}
  ObjectLifetime(const Lifetime& thisLifetime) : ThisLifetime(thisLifetime) {}
  ObjectLifetime(llvm::StringRef name) : ThisLifetime(name) {}
  ObjectLifetime(char id) : ThisLifetime(id) {}

  std::optional<ObjectLifetime> GetPointeeObject() {
    return PointeeObject;
  }

  Lifetime& GetLifetime() { return ThisLifetime; }
  std::optional<Lifetime> GetPointeeObjectLifetime() { return HasPointeeObject() ? PointeeObject.value().GetLifetime() : std::nullopt; }
  clang::QualType& GetType() { return ThisType; }
  clang::QualType& GetPointeeType() { return PointeeObject.GetType(); }

  void SetLifetime(Lifetime& lifetime) { ThisLifetime = lifetime; }
  void SetPointeeLifetime(Lifetime& lifetime) {
    PointeeObject.SetLifetime(lifetime);
  }
  void SetPointee(ObjectLifetime& objectLifetime) {
    PointeeObject = objectLifetime;
  }

  std::string DebugString() const {
    std::string res = "Type: " + ThisType.getAsString() +
                      "\t\t\t[Lifetime]: " + ThisLifetime.DebugString() + "\n";
    if (PointeeObject) {
      res += PointeeObject->DebugString();
    }
  }

 private:
  Lifetime ThisLifetime;
  ObjectLifetime PointeeObject;
  clang::QualType ThisType;
  bool HasPointeeObject = false;
};
}  // namespace clang

#endif
