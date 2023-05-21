#include "clang/Sema/Lifetime.h"

namespace clang {

constexpr char NOTSET = 1;
constexpr char LOCAL = 2;
constexpr char STATIC = 3;
constexpr char INVALID_ID_TOMBSTONE = 4;
constexpr char INVALID_EMPTY = 5;

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime() : Id(NOTSET) {}

Lifetime::Lifetime(char id) : Id(id) {}

Lifetime::Lifetime(llvm::StringRef name) {
  if (name.equals(STATIC_NAME)) {
    *this = Lifetime(STATIC);
  } else if (name.equals(LOCAL_NAME)) {
    *this = Lifetime(LOCAL);
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    *this = Lifetime(name.front());
  } else {
    // TODO error
    // TODO change this
    *this = Lifetime(NOTSET);
  }
}

bool Lifetime::IsNotSet() const { return Id == NOTSET; }
bool Lifetime::IsStatic() const { return Id == STATIC; }
bool Lifetime::ContainsStatic() const {
  return ShortestLifetimes.count(STATIC);
}
bool Lifetime::IsLocal() const { return Id == LOCAL; }
bool Lifetime::ContainsLocal() const { return ShortestLifetimes.count(LOCAL); }
void Lifetime::SetStatic() { Id = STATIC; }
void Lifetime::SetLocal() { Id = LOCAL; }

Lifetime Lifetime::InvalidEmpty() { return Lifetime(INVALID_EMPTY); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }

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
      if (id >= 'a' && id <= 'z')
        return "$" + std::string(1, id);
      else
        // TODO error
        return "error";
  }
}

std::string Lifetime::DebugString() const {
  std::string res;
  res += "[Lifetime] -> " + GetLifetimeName() + "; ";
  if (IsNotSet()) {
    res += "Shortest Lifetimes of this variable: { ";
    for (auto &pair : ShortestLifetimes) {
      res += GetLifetimeName(pair.first) + ' ';
    }
    res += "}";
  }
  return res;
}

void Lifetime::ProcessShortestLifetimes() {
  // should only reach this if lifetime is not set
  assert(IsNotSet());

  if (ContainsLocal()) {
    // lifetime $local is the shortest possible
    SetLocal();
    return;
  }

  if (ShortestLifetimes.size() > 1 && ContainsStatic()) {
    // static outlives all others; if there are others lifetimes, static should
    // not belong to shortest lifetimes
    RemoveFromShortestLifetimes(STATIC);
  }

  if (ShortestLifetimes.size() == 1) {
    SetId((*ShortestLifetimes.begin()).first);
    return;
  }

  // in all other cases, the Id of the lifetime remains NOTSET
  // - if ShortestLifetimes is empty, the lifetime of this variable is
  // undefined
  // - if ShortestLifetimes contains multiple lifetimes, then the
  // lifetime is the shortest among them since we cannot now which lifetimes
  // outlives which, then all of them are considered the shortest
}

bool Lifetime::CompareShortestLifetimes(const Lifetime &Other) const {
  const auto &OtherShortestLifetimes = Other.GetShortestLifetimes();
  for (const auto &pair : ShortestLifetimes) {
    if (!OtherShortestLifetimes.count(pair.first)) {
      return false;
    }
  }
  return true;
}

Lifetime &Lifetime::operator=(const Lifetime &Other) {
  Id = Other.GetId();
  return *this;
}

bool Lifetime::operator==(const Lifetime &Other) const {
  if (!IsNotSet()) {
    return Id == Other.GetId();
  }
  return Id == Other.GetId() &&
         ShortestLifetimes == Other.GetShortestLifetimes();
}

bool Lifetime::operator!=(const Lifetime &Other) const {
  return !operator==(Other);
}

bool Lifetime::operator>=(const Lifetime &Other) const {
  // $static outlives all lifetimes
  // all lifetimes outlive $local
  if (IsStatic() || Other.IsLocal()) return true;
  if (IsLocal() || Other.IsStatic()) return false;

  if (!IsNotSet()) return Id == Other.GetId();

  // TODO == or subset or shortest lifetimes?
  return Id == Other.GetId() && CompareShortestLifetimes(Other);
}

bool Lifetime::operator<(const Lifetime &Other) const {
  return !operator>=(Other);
}

}  // namespace clang
