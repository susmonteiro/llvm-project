#include "clang/Sema/Lifetime.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {

constexpr char NOTSET = 1;
constexpr char LOCAL = 2;
constexpr char STATIC = 3;
constexpr char INVALID_ID_TOMBSTONE = 4;
constexpr char INVALID_EMPTY = 5;

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime() : id_(NOTSET) {}

Lifetime::Lifetime(char id) : id_(id) {}

Lifetime::Lifetime(llvm::StringRef name) {
  if (name.equals(STATIC_NAME)) {
    *this = Lifetime(STATIC);
  } else if (name.equals(LOCAL_NAME)) {
    *this = Lifetime(LOCAL);
    // TODO is this check ok?
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    *this = Lifetime(name.front());
  } else {
    // TODO error
    // TODO change this
    *this = Lifetime(NOTSET);
  }
}

std::string Lifetime::GetLifetimeName(char id) const {
  switch (id) {
    case NOTSET:
    case INVALID_ID_TOMBSTONE:
    case INVALID_EMPTY:
      return "not-set";
      break;
    case STATIC:
      return "$static";
      break;
    case LOCAL:
      return "$local";
      break;
    default:
      if (id >= 'a' && id <= 'z') return "$" + std::string(1, id);
      // TODO error
      else
        return "error";
  }
}

void Lifetime::ProcessShortestLifetimes() {
  // should only reach this if lifetime is not set
  assert(IsNotSet());

  if (ContainsLocal()) {
    // lifetime $local is the shortest possible
    SetLocal();
    return;
  }

  if (shortest_lifetimes_.size() > 1 && ContainsStatic()) {
    // static outlives all others; if there are others lifetimes, static should
    // not belong to shortest lifetimes
    RemoveFromShortestLifetimes(STATIC);
  }

  if (shortest_lifetimes_.size() == 1) {
    SetId(*shortest_lifetimes_.begin());
    return;
  }

  // in all other cases, the id_ of the lifetime remains NOTSET
  // if shortest_lifetimes_ is empty, that the lifetime of this variable is
  // undefined if shortest_lifetimes_ contains multiple lifetimes, then the
  // lifetime is the shortest among them since we cannot now which lifetimes
  // outlives which, then all of them are considered the shortest
}

bool Lifetime::IsNotSet() const { return id_ == NOTSET; }
bool Lifetime::IsStatic() const { return id_ == STATIC; }
bool Lifetime::ContainsStatic() const {
  return shortest_lifetimes_.count(STATIC);
}
bool Lifetime::IsLocal() const { return id_ == LOCAL; }
bool Lifetime::ContainsLocal() const {
  return shortest_lifetimes_.count(LOCAL);
}
void Lifetime::SetStatic() { id_ = STATIC; }
void Lifetime::SetLocal() { id_ = LOCAL; }

Lifetime Lifetime::InvalidEmpty() { return Lifetime(INVALID_EMPTY); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }
}  // namespace clang
