#ifndef LIFETIME_ANNOTATIONS_LIFETIME_NEW_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_NEW_H_

#include "Lifetime.h"
#include "DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"

namespace clang {

// TODO change name of file and class to just "Lifetime"

// the lifetime of a variable
// can be $static, $local or $c, where c is a char
class LifetimeNew {
 public:
  LifetimeNew();

  LifetimeNew(llvm::StringRef);

  // Returns whether this lifetime is a constant lifetime.
  bool IsConstant() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;

  // Returns a unique numeric ID for the lifetime.
  int Id() const { return id_; }

 private:
  explicit LifetimeNew(int id);

  static LifetimeNew InvalidEmpty();
  static LifetimeNew InvalidTombstone();

  friend class llvm::DenseMapInfo<LifetimeNew, void>;

  // TODO store this in the lifetime or in the structure "above"
  // llvm::DenseSet<std::string> dependencies;
  int id_;
  bool isStatic = false;
  bool isLocal = false;
};
}  // namespace clang

namespace llvm {

template <>
struct DenseMapInfo<clang::LifetimeNew, void> {
  static clang::LifetimeNew getEmptyKey() {
    return clang::LifetimeNew::InvalidEmpty();
  }

  static clang::LifetimeNew getTombstoneKey() {
    return clang::LifetimeNew::InvalidTombstone();
  }

  static unsigned getHashValue(clang::LifetimeNew lifetime) {
    return llvm::hash_value(lifetime.id_);
  }

  static bool isEqual(clang::LifetimeNew lhs, clang::LifetimeNew rhs) {
    return lhs.id_ == rhs.id_;
  }
};

}  // namespace llvm

#endif