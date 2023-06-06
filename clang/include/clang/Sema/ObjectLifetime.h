#ifndef OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define OBJECT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "clang/Sema/Lifetime.h"
#include "llvm/ADT/DenseSet.h"

namespace clang {

using PointeesLifetimesVector = llvm::DenseMap<clang::QualType*, Lifetime>;

// the lifetime of a variable can be $static, $local or $c, where c is a char
class ObjectLifetime {
 public:
  ObjectLifetime() : VarLifetime() {}
  ObjectLifetime(const Lifetime& varLifetime) : VarLifetime(varLifetime) {}
  ObjectLifetime(llvm::StringRef name) : VarLifetime(name) {}
  ObjectLifetime(char id) : VarLifetime(id) {}

  Lifetime& GetVarLifetime() { return VarLifetime; }
  PointeesLifetimesVector& GetPointeesLifetimes() { return PointeesLifetimes; }

  void SetVarLifetime(Lifetime &lifetime) { VarLifetime = lifetime; }


  void InsertPointeeLifetime(clang::QualType type, Lifetime& lifetime) {
    PointeesLifetimes[&type] = lifetime;
  }

  void InsertPointeesLifetimes(PointeesLifetimesVector pointeesLifetimes) {
    PointeesLifetimes.insert(pointeesLifetimes.begin(),
                             pointeesLifetimes.end());
  }

  // std::string DebugString() const {
  //   std::string res = "[Object lifetime]\n\nMain lifetime: " + VarLifetime.DebugString() + "\n\nPointees lifetimes:\n";
  //   for (auto &pair : PointeesLifetimes) {
  //     if (pair.first->isNull()) res += "null\n";
  //     res += pair.first->getAsString();
  //   }
  //   return res;
  // }

  std::string DebugString() const {
    std::string res = "YO";
    debugLifetimes("[Object lifetime]\n\nMain lifetime: " + VarLifetime.DebugString() + "\n\nPointees lifetimes:\n");
    for (auto &pair : PointeesLifetimes) {
      if (pair.first->isNull()) debugLifetimes("null\n");
      else debugLifetimes(pair.first->getAsString());
    }
    debugLifetimes("Before return");
    return res;
  }

 private:
  Lifetime VarLifetime;
  PointeesLifetimesVector PointeesLifetimes;
};
}  // namespace clang

#endif
