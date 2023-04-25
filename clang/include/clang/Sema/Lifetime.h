#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"

namespace clang {

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime();
  Lifetime(const Lifetime &other) : Id(other.GetId()) {}
  Lifetime(llvm::StringRef name);

  // Returns whether this lifetime is valid
  bool IsNotSet() const;

  // Returns whether this lifetime is a static lifetime.
  bool IsStatic() const;
  bool ContainsStatic() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;
  bool ContainsLocal() const;

  // Sets the Id to $static
  void SetStatic();

  // Sets the Id to $local
  void SetLocal();

  // Returns the numeric ID for the lifetime.
  char GetId() const { return Id; }
  void SetId(char id) { Id = id; }

  void ProcessShortestLifetimes();

  std::string DebugString() const;

  // Returns the name of the lifetime
  std::string GetLifetimeName(char id) const;
  std::string GetLifetimeName() const { return GetLifetimeName(Id); }

  llvm::DenseSet<char> GetShortestLifetimes() const {
    return ShortestLifetimes;
  }
  void InsertShortestLifetimes(char id) { ShortestLifetimes.insert(id); }

  void SetShortestLifetimes(llvm::DenseSet<char> shortest_lifetimes) {
    ShortestLifetimes = shortest_lifetimes;
  }

  void InsertShortestLifetimes(llvm::DenseSet<char> shortest_lifetimes) {
    ShortestLifetimes.insert(shortest_lifetimes.begin(),
                             shortest_lifetimes.end());
  }

  void RemoveFromShortestLifetimes(char id) {
    auto it = ShortestLifetimes.find(id);
    if (it != ShortestLifetimes.end()) {
      ShortestLifetimes.erase(it);
    }
  }

  Lifetime &operator=(const Lifetime &other);
  bool operator==(const Lifetime &Other) const;
  bool operator!=(const Lifetime &Other) const;
  bool operator>=(const Lifetime &Other) const;
  bool operator<(const Lifetime &Other) const;

 private:
  explicit Lifetime(char id);

  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  // TODO maybe store clang::Decl in this class
  llvm::DenseSet<char> ShortestLifetimes;
  char Id;
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
    return llvm::hash_value(lifetime.Id);
  }

  static bool isEqual(clang::Lifetime lhs, clang::Lifetime rhs) {
    return lhs.Id == rhs.Id;
  }
};

}  // namespace llvm

#endif
