#include "clang/Sema/Lifetime.h"

namespace clang {

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime() : ObjectType(std::nullopt), Id(NOTSET) {}
Lifetime::Lifetime(clang::QualType &type) : ObjectType(type), Id(NOTSET) {}

Lifetime::Lifetime(char id, clang::QualType &type)
    : ObjectType(std::optional<clang::QualType>(type)),
      Id(GenerateLifetimeId(id)) {}

// TODO delete this
Lifetime::Lifetime(char id) : Id(GenerateLifetimeId(id)) {}

Lifetime::Lifetime(llvm::StringRef name, clang::QualType &type)
    : ObjectType(std::optional<clang::QualType>(type)) {
  if (name.equals(STATIC_NAME)) {
    *this = Lifetime(STATIC, type);
  } else if (name.equals(LOCAL_NAME)) {
    *this = Lifetime(LOCAL, type);
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    *this = Lifetime(name.front(), type);
  } else {
    // TODO error
    // TODO change this
    *this = Lifetime(NOTSET, type);
  }
}

char Lifetime::GenerateLifetimeId(char id) const {
  if (id == NOTSET || id == LOCAL || id == STATIC ||
      id == INVALID_ID_TOMBSTONE || id == INVALID_EMPTY) {
    return id;
  } else if (id >= 'a' && id <= 'z') {
    return CharToId(id);
  } else {
    // TODO error
    // TODO change this
    return NOTSET;
  }
}

bool Lifetime::IsNotSet() const { return Id == NOTSET; }
bool Lifetime::IsStatic() const { return Id == STATIC; }
bool Lifetime::ContainsStatic() const {
  return (unsigned int)STATIC < ShortestLifetimes.size() &&
         !ShortestLifetimes[STATIC].empty();
}

bool Lifetime::IsLocal() const { return Id == LOCAL; }
bool Lifetime::ContainsLocal() const {
  return (unsigned int)LOCAL < ShortestLifetimes.size() &&
         !ShortestLifetimes[LOCAL].empty();
}
void Lifetime::SetStatic() { Id = STATIC; }
void Lifetime::SetLocal() { Id = LOCAL; }

Lifetime Lifetime::InvalidEmpty() { return Lifetime(INVALID_EMPTY); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }

std::string Lifetime::GetLifetimeName(char id) const {
  id = IdToChar(id);
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
  std::string res = "[Type]: ";
  res += ObjectType.has_value() ? ObjectType.value().getAsString()
                                : "unknown type";
  res += "\t\t[Lifetime] -> " + GetLifetimeName() + "; ";

  if (IsNotSet()) {
    res += "Shortest Lifetimes of this variable: { ";
    for (unsigned int i = 0; i < ShortestLifetimes.size(); i++) {
      if (!ShortestLifetimes[i].empty()) {
        res += GetLifetimeName(i) + ' ';
      }
    }
    res += "}";
  }
  return res + '\n';
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
    for (unsigned int i = 0; i < ShortestLifetimes.size(); i++) {
      if (!ShortestLifetimes[i].empty()) {
        SetId(i);
        return;
      }
    }
  }

  // in all other cases, the Id of the lifetime remains NOTSET
  // - if ShortestLifetimes is empty, the lifetime of this variable is
  // undefined
  // - if ShortestLifetimes contains multiple lifetimes, then the
  // lifetime is the shortest among them since we cannot now which lifetimes
  // outlives which, then all of them are considered the shortest
}

std::optional<StmtDenseSet> Lifetime::GetStmts(char id) {
  assert(id != NOTSET);
  return (unsigned int)id >= ShortestLifetimes.size() ||
                 ShortestLifetimes[id].empty()
             ? std::nullopt
             : std::optional(ShortestLifetimes[id]);
}

bool Lifetime::CompareShortestLifetimes(const Lifetime &Other) const {
  const auto &OtherShortestLifetimes = Other.GetShortestLifetimes();
  for (unsigned int i = 0; i < ShortestLifetimes.size(); i++) {
    if (ShortestLifetimes[i].empty()) return false;
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
