#ifndef LIFETIME_ANNOTATIONS_LIFETIME_H_
#define LIFETIME_ANNOTATIONS_LIFETIME_H_

#include <set>
#include <string>

#include "clang/Sema/DebugLifetimes.h"
#include "llvm/ADT/DenseSet.h"

#define is_in_shortest(set, e) (set & (1 << e)) != 0
#define shortest_remove(set, e) (set &= ~(1 << e))
#define shortest_insert(set, e) (set |= 1 << e)

namespace clang {

using StmtDenseSet = llvm::DenseSet<const clang::Stmt *>;
using LifetimesVector = llvm::SmallVector<StmtDenseSet>;

constexpr char NOTSET = 0;
constexpr char INVALID_ID_TOMBSTONE = 1;
constexpr char INVALID_EMPTY = 2;
constexpr char NULL_LIFETIME = 3;
constexpr char DEAD = 4;
constexpr char LOCAL = 5;
constexpr char STATIC = 6;
constexpr char OFFSET = 7;

// the lifetime of a variable can be $static, $local or $c, where c is a char
class Lifetime {
 public:
  Lifetime() = default;
  Lifetime(llvm::StringRef name, clang::QualType type);
  Lifetime(char id);
  Lifetime(char id, clang::QualType type);
  Lifetime(clang::QualType &type)
      : LifetimeType(type),
        NumIndirections(Lifetime::GetNumIndirections(type)),
        Id(NOTSET) {}
  Lifetime(unsigned int num_indirections)
      : NumIndirections(num_indirections), Id(NOTSET) {}
  Lifetime(char id, unsigned int num_indirections);

  void InitializeId(char id);
  // Returns whether this lifetime is valid
  bool IsSet() const;
  bool IsNotSet() const;

  // Returns whether this lifetime is a static lifetime.
  bool IsStatic() const;
  bool ContainsStatic() const;

  // Returns whether this lifetime is a local lifetime.
  bool IsLocal() const;
  bool ContainsLocal() const;

  // Returns whether this lifetime is a dead lifetime.
  bool IsDead() const;
  bool ContainsDead() const;

  // Returns whether this lifetime is null
  bool IsNull() const;

  // Sets the Id to $static
  void SetStatic();

  // Sets the Id to $local
  void SetLocal();

  // Sets the Id to $dead
  void SetDead();

  static char CharToId(char id) { return id < OFFSET ? id : id - 'a' + OFFSET; }
  static char IdToChar(char id) { return id < OFFSET ? id : id + 'a' - OFFSET; }
  static unsigned int GetNumIndirections(clang::QualType type);
  unsigned int GetNumIndirections() const { return NumIndirections; }
  static clang::QualType GetTypeFromNumberIndirections(
      clang::QualType type, unsigned int number_indirections);

  // Returns the numeric ID for the lifetime.
  char GetId() const { return Id; }
  void SetId(char id) { Id = id; }
  clang::QualType GetType() const { return LifetimeType; }
  void SetNumIndirections(clang::QualType type) {
    NumIndirections = Lifetime::GetNumIndirections(type);
  }
  void SetNumIndirections(unsigned int num_indirections) {
    NumIndirections = num_indirections;
  }
  void SetType(clang::QualType type) {
    LifetimeType = type;
    SetNumIndirections(type);
  }

  bool EmptyPossibleLifetimes() const;
  void ProcessPossibleLifetimes();

  std::string DebugString() const;

  // Returns the name of the lifetime
  static std::string GetLifetimeName(char id);
  std::string GetLifetimeName() const { return GetLifetimeName(Id); }

  static StmtDenseSet *GetAndResizePossibleLifetime(
      char id, LifetimesVector &possible_lifetimes) {
    ResizePossibleLifetimes(id, possible_lifetimes);
    return &possible_lifetimes[id];
  }

  StmtDenseSet *GetPossibleLifetime(char id) {
    return GetPossibleLifetime(id, PossibleLifetimes);
  }

  static StmtDenseSet *GetPossibleLifetime(
      char id, LifetimesVector &possible_lifetimes) {
    return &possible_lifetimes[id];
  }

  StmtDenseSet *GetAndResizePossibleLifetime(char id) {
    return GetPossibleLifetime(id, PossibleLifetimes);
  }

  const LifetimesVector &GetPossibleLifetimes() const {
    return PossibleLifetimes;
  }

  int GetShortestLifetimes() const { return ShortestLifetimes; }

  std::optional<StmtDenseSet> GetStmts(char id);

  static void ResizePossibleLifetimes(char id,
                                      LifetimesVector &possible_lifetimes) {
    if ((unsigned int)id >= possible_lifetimes.size())
      possible_lifetimes.resize(id + 1);
  }

  static void InsertPossibleLifetimes(char id, const clang::Stmt *stmt,
                                      LifetimesVector &possible_lifetimes) {
    Lifetime::GetAndResizePossibleLifetime(id, possible_lifetimes)
        ->insert(stmt);
  }

  void InsertPossibleLifetimes(char id, const clang::Stmt *stmt) {
    InsertPossibleLifetimes(id, stmt, PossibleLifetimes);
  }

  void InsertPossibleLifetimes(LifetimesVector possible_lifetimes) {
    if (possible_lifetimes.size() > PossibleLifetimes.size())
      PossibleLifetimes.resize(possible_lifetimes.size());
    for (unsigned int i = 0; i < possible_lifetimes.size(); i++) {
      GetPossibleLifetime(i)->insert(
          GetPossibleLifetime(i, possible_lifetimes)->begin(),
          GetPossibleLifetime(i, possible_lifetimes)->end());
    }
  }

  void SetPossibleLifetimes(LifetimesVector possible_lifetimes) {
    PossibleLifetimes = possible_lifetimes;
  }

  void RemoveFromPossibleLifetimes(char id) {
    GetPossibleLifetime(id)->clear();
  }

  bool ContainsShortestLifetime(unsigned int id) const {
    return is_in_shortest(ShortestLifetimes, id);
  }

  bool operator==(const Lifetime &Other) const;
  bool operator!=(const Lifetime &Other) const;
  bool operator<(const Lifetime &Other) const;

 private:
  static Lifetime InvalidEmpty();
  static Lifetime InvalidTombstone();

  friend class llvm::DenseMapInfo<Lifetime, void>;

  LifetimesVector PossibleLifetimes;
  clang::QualType LifetimeType;
  unsigned int NumIndirections = 0;
  int ShortestLifetimes = 0;
  char Id = NULL_LIFETIME;
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
