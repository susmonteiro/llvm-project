#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <set>
#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"

namespace clang {

using StmtDenseSet = llvm::DenseSet<const clang::Stmt *>;
using LifetimesVector = llvm::SmallVector<StmtDenseSet>;

constexpr char NOTSET = 0;
constexpr char INVALID_ID_TOMBSTONE = 1;
constexpr char INVALID_EMPTY = 2;
constexpr char LOCAL = 3;
constexpr char STATIC = 4;
constexpr char OFFSET = 5;

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime() = default;
  Lifetime(llvm::StringRef name, clang::QualType type);
  Lifetime(char id);
  Lifetime(char id, clang::QualType type);
  Lifetime(clang::QualType &type) : LifetimeType(type) {}

  // Returns whether this lifetime is valid
  bool IsSet() const;
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

  static char CharToId(char id) { return id < OFFSET ? id : id - 'a' + OFFSET; }
  static char IdToChar(char id) { return id < OFFSET ? id : id + 'a' - OFFSET; }
  static unsigned int GetNumberIndirections(clang::QualType type);
  static clang::QualType GetTypeFromNumberIndirections(
      clang::QualType type, unsigned int number_indirections);

  // Returns the numeric ID for the lifetime.
  char GetId() const { return Id; }
  void SetId(char id) { Id = id; }
  clang::QualType GetType() const { return LifetimeType; }
  void SetType(clang::QualType type) { LifetimeType = type; }

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

  const LifetimesVector &GetShortestLifetimes() const {
    return ShortestLifetimes;
  }
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

  void InsertShortestLifetimes(char id, const clang::Stmt *stmt) {
    InsertShortestLifetimes(id, stmt, ShortestLifetimes);
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

  bool operator==(const Lifetime &Other) const;
  bool operator!=(const Lifetime &Other) const;
  bool operator<(const Lifetime &Other) const;

 private:
  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  LifetimesVector ShortestLifetimes;
  clang::QualType LifetimeType;
  char Id = NOTSET;
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
