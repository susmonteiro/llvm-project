#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"

namespace clang {

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime();
  Lifetime(const Lifetime &other) : id_(other.Id()) {}
  Lifetime(llvm::StringRef name);

  Lifetime &operator=(const Lifetime &other) {
    id_ = other.Id();
    return *this;
  }

  // Returns whether this lifetime is valid
  bool IsNotSet() const;

  // Returns whether this lifetime is a static lifetime.
  bool IsStatic() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;

  // Sets the id_ to $static
  void SetStatic();

  // Sets the id_ to $local
  void SetLocal();

  // Returns the numeric ID for the lifetime.
  char Id() const { return id_; }

  std::string DebugString() const {
    std::string res;
    res += "[Lifetime] -> " + GetLifetimeName() + "; ";
    if (IsNotSet()) {
      res += "Shortest Lifetimes of this variable: { ";
      for (char l : shortest_lifetimes_) {
        res += GetLifetimeName(l) + ' ';
      }
      res += "}";
    }
    return res;
  }

  // Returns the name of the lifetime
  std::string GetLifetimeName(char id) const;
  std::string GetLifetimeName() const { return GetLifetimeName(id_); }

  llvm::DenseSet<char> GetShortestLifetimes() const { return shortest_lifetimes_; }
  void InsertShortestLifetimes(char id) { shortest_lifetimes_.insert(id); }
  void InsertShortestLifetimes(llvm::DenseSet<char> shortest_lifetimes) { shortest_lifetimes_.insert(shortest_lifetimes.begin(), shortest_lifetimes.end()); }
  void SetShortestLifetimes(llvm::DenseSet<char> shortest_lifetimes) {
    shortest_lifetimes_ = std::move(shortest_lifetimes);
  }

 private:
  explicit Lifetime(char id);

  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  // TODO maybe store clang::Decl in this class
  // TODO do we want this?
  llvm::DenseSet<char> shortest_lifetimes_;
  char id_;
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
