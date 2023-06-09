#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"

namespace clang {

using StmtDenseSet = llvm::DenseSet<const clang::Stmt *>;
using LifetimesVector = llvm::SmallVector<StmtDenseSet>;

constexpr char NOTSET = 0;
constexpr char LOCAL = 1;
constexpr char STATIC = 2;
constexpr char INVALID_ID_TOMBSTONE = 3;
constexpr char INVALID_EMPTY = 4;
constexpr char OFFSET = 5;

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime();
  Lifetime(const Lifetime &other) : Id(other.GetId()) {}
  Lifetime(llvm::StringRef name);
  Lifetime(char id);


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

  static char CharToId(char id) {
    return id < OFFSET ? id : id - 'a' + OFFSET;
  }
  static char IdToChar(char id) {
    return id < OFFSET ? id : id + 'a' - OFFSET;
  }

  // Returns the numeric ID for the lifetime.
  char GetId() const { return Id; }
  void SetId(char id) { Id = id; }

  void ProcessShortestLifetimes();

  std::string DebugString() const;

  // Returns the name of the lifetime
  std::string GetLifetimeName(char id) const;
  std::string GetLifetimeName() const { return GetLifetimeName(Id); }

  static StmtDenseSet *GetAndResizeShortestLifetime(
      char id, LifetimesVector &shortest_lifetimes) {
    ResizeShortestLifetimes(id, shortest_lifetimes);
    return &shortest_lifetimes[id];
  }

  StmtDenseSet *GetShortestLifetime(char id) {
    return GetShortestLifetime(id, ShortestLifetimes);
  }

  static StmtDenseSet *GetShortestLifetime(
      char id, LifetimesVector &shortest_lifetimes) {
    return &shortest_lifetimes[id];
  }

  StmtDenseSet *GetAndResizeShortestLifetime(char id) {
    return GetShortestLifetime(id, ShortestLifetimes);
  }

  LifetimesVector GetShortestLifetimes() const { return ShortestLifetimes; }
  std::optional<StmtDenseSet> GetStmts(char id);

  static void ResizeShortestLifetimes(char id,
                                      LifetimesVector &shortest_lifetimes) {
    if ((unsigned int)id >= shortest_lifetimes.size())
      shortest_lifetimes.resize(id + 1);
  }

  static void InsertShortestLifetimes(char id, const clang::Stmt *stmt,
                                      LifetimesVector &shortest_lifetimes) {
    Lifetime::GetAndResizeShortestLifetime(id, shortest_lifetimes)
        ->insert(stmt);
  }

  static void InsertShortestLifetimes(char id,
                                      llvm::DenseSet<const clang::Stmt *> stmts,
                                      LifetimesVector &shortest_lifetimes) {
    Lifetime::GetAndResizeShortestLifetime(id, shortest_lifetimes)
        ->insert(stmts.begin(), stmts.end());
  }

  void InsertShortestLifetimes(char id, const clang::Stmt *stmt) {
    InsertShortestLifetimes(id, stmt, ShortestLifetimes);
  }

  void InsertShortestLifetimes(char id,
                               llvm::DenseSet<const clang::Stmt *> stmts) {
    InsertShortestLifetimes(id, stmts, ShortestLifetimes);
  }

  void InsertShortestLifetimes(LifetimesVector shortest_lifetimes) {
    if (shortest_lifetimes.size() > ShortestLifetimes.size())
      ShortestLifetimes.resize(shortest_lifetimes.size());
    for (unsigned int i = 0; i < shortest_lifetimes.size(); i++) {
      GetShortestLifetime(i)->insert(
          GetShortestLifetime(i, shortest_lifetimes)->begin(),
          GetShortestLifetime(i, shortest_lifetimes)->end());
    }
  }

  void SetShortestLifetimes(LifetimesVector shortest_lifetimes) {
    ShortestLifetimes = shortest_lifetimes;
  }

  void RemoveFromShortestLifetimes(char id) {
    GetShortestLifetime(id)->clear();
  }

  bool CompareShortestLifetimes(const Lifetime &other) const;

  Lifetime &operator=(const Lifetime &other);
  bool operator==(const Lifetime &Other) const;
  bool operator!=(const Lifetime &Other) const;
  bool operator>=(const Lifetime &Other) const;
  bool operator<(const Lifetime &Other) const;

 private:
  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  // TODO also store a vector with the lifetime ids that it depends on
  LifetimesVector ShortestLifetimes;
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
