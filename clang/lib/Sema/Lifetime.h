#ifndef LIFETIME_ANNOTATIONS_LIFETIME_NEW_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_NEW_H_

#include "DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"

namespace clang {

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime();

  Lifetime(llvm::StringRef);

  // Returns whether this lifetime is a static lifetime.
  bool IsStatic() const { return isStatic; }

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const { return isLocal; }

  // Returns a unique numeric ID for the lifetime.
  int Id() const { return id_; }

 private:
  explicit Lifetime(int id);

  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  // ? store this here or in the structure "above"
  llvm::DenseSet<const clang::Decl*> parents;
  llvm::DenseSet<const clang::Decl*> children;
  llvm::DenseSet<int> shortest_lifetimes;
  int id_;
  bool isStatic = false;
  bool isLocal = false;
};
}  // namespace clang

namespace llvm {

template <>
struct DenseMapInfo<clang::Lifetime, void> {
  static clang::Lifetime getEmptyKey() {
    return clang::Lifetime::InvalidEmpty();
  }

  static clang::Lifetime getTombstoneKey() {
    return clang::Lifetime::InvalidTombstone();
  }

  static unsigned getHashValue(clang::Lifetime lifetime) {
    return llvm::hash_value(lifetime.id_);
  }

  static bool isEqual(clang::Lifetime lhs, clang::Lifetime rhs) {
    return lhs.id_ == rhs.id_;
  }
};

}  // namespace llvm

#endif