// TODO change me
#ifndef CRUBIT_LIFETIME_ANNOTATIONS_LIFETIME_H_
#define CRUBIT_LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <atomic>
#include <cassert>
#include <functional>
#include <ostream>
#include <string>
#include <utility>

#include "llvm/ADT/Hashing.h"

namespace clang {
namespace tidy {
namespace lifetimes {

// A lifetime variable or constant lifetime.
class Lifetime {
public:
  // TODO change class
  // TODO create lifetime.cpp and implement functions
  // Creates an invalid lifetime.
  //
  // This is provided because containers need default constructors. It is not
  // legal to perform any operations on an invalid lifetime except to copy or
  // delete it.
  //
  // Use one of the static member functions below to create a valid lifetime.
  // TODO need this?
  //   Lifetime();

  Lifetime(const Lifetime &) = default;
  Lifetime &operator=(const Lifetime &) = default;

  // Creates a new lifetime variable.
  static Lifetime CreateVariable(std::string name);

  // Returns the 'static lifetime constant.
  static Lifetime Static();

  // Creates a new local lifetime constant.
  static Lifetime CreateLocal();

  // Returns whether this lifetime is a lifetime variable.
  bool IsVariable() const;

  // Returns whether this lifetime is a constant lifetime.
  bool IsConstant() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;

  std::string GetName() { return lifetime_; }

  int GetType() { return type_; }

  // Returns a textual representation of the lifetime
  std::string DebugString() const;

  bool operator==(Lifetime other) const {
    assert(IsValid());
    assert(other.IsValid());
    if (IsVariable()) {
      return type_ == other.GetType() && !lifetime_.compare(other.GetName());
    }
    return !lifetime_.compare(other.GetName());
  }

  bool operator!=(Lifetime other) const { return !(*this == other); }

private:
  explicit Lifetime(int type);
  explicit Lifetime(std::string name, int type);

  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  bool IsValid() const;

  // TODO need this?
  //   friend class llvm::DenseMapInfo<Lifetime, void>;
  //   friend class std::less<Lifetime>;

  int type_; // variable, static, local
  // TODO another option could be to keep a dict with all lifetimes and their
  // TODO respective id and then assign the correct id given a lifetime name
  std::string lifetime_ = ""; // lifetime name
  // TODO check if local lifetimes need any kind of id
  // TODO do I need this?
  //   static std::atomic<int> next_variable_id_;
  //   static std::atomic<int> next_local_id_;
};

std::ostream &operator<<(std::ostream &os, Lifetime lifetime);

} // namespace lifetimes
} // namespace tidy
} // namespace clang

// TODO do I need this?
// namespace llvm {

// template <> struct DenseMapInfo<clang::tidy::lifetimes::Lifetime, void> {
//   static clang::tidy::lifetimes::Lifetime getEmptyKey() {
//     return clang::tidy::lifetimes::Lifetime::InvalidEmpty();
//   }

//   static clang::tidy::lifetimes::Lifetime getTombstoneKey() {
//     return clang::tidy::lifetimes::Lifetime::InvalidTombstone();
//   }

//   static unsigned getHashValue(clang::tidy::lifetimes::Lifetime lifetime) {
//     return llvm::hash_value(lifetime.id_);
//   }

//   static bool isEqual(clang::tidy::lifetimes::Lifetime lhs,
//                       clang::tidy::lifetimes::Lifetime rhs) {
//     return lhs.id_ == rhs.id_;
//   }
// };

// } // namespace llvm

// TODO do I need this?
// namespace std {

// template <> struct less<clang::tidy::lifetimes::Lifetime> {
//   bool operator()(const clang::tidy::lifetimes::Lifetime &l1,
//                   const clang::tidy::lifetimes::Lifetime &l2) const {
//     return l1.id_ < l2.id_;
//   }
// };

// } // namespace std

#endif // CRUBIT_LIFETIME_ANNOTATIONS_LIFETIME_H_
