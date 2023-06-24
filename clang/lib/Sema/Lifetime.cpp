#include "clang/Sema/Lifetime.h"

namespace clang {

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime(char id) {
  if (id == NOTSET || id == LOCAL || id == STATIC) {
    Id = id;
  } else if (id >= 'a' && id <= 'z') {
    Id = CharToId(id);
  } else {
    // TODO error
    // TODO change this
    Id = NOTSET;
  }
}

// TODO change this
Lifetime::Lifetime(char id, clang::QualType type) : LifetimeType(type) {
  if (id == NOTSET || id == LOCAL || id == STATIC) {
    Id = id;
  } else if (id >= 'a' && id <= 'z') {
    Id = CharToId(id);
  } else {
    // TODO error
    // TODO change this
    Id = NOTSET;
  }
}

Lifetime::Lifetime(llvm::StringRef name, clang::QualType type)
    : LifetimeType(type) {
  if (name.equals(STATIC_NAME)) {
    Id = STATIC;
  } else if (name.equals(LOCAL_NAME)) {
    Id = LOCAL;
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    Id = CharToId(name.front());
  } else {
    // TODO error
    // TODO change this
    Id = NOTSET;
  }
}

bool Lifetime::IsSet() const { return Id >= LOCAL; }
bool Lifetime::IsNotSet() const { return Id == NOTSET; }
bool Lifetime::IsStatic() const { return Id == STATIC; }
bool Lifetime::ContainsStatic() const {
  return (unsigned int)STATIC < PossibleLifetimes.size() &&
         !PossibleLifetimes[STATIC].empty();
}

bool Lifetime::IsLocal() const { return Id == LOCAL; }
bool Lifetime::ContainsLocal() const {
  return (unsigned int)LOCAL < PossibleLifetimes.size() &&
         !PossibleLifetimes[LOCAL].empty();
}
void Lifetime::SetStatic() { Id = STATIC; }
void Lifetime::SetLocal() { Id = LOCAL; }

unsigned int Lifetime::GetNumberIndirections(clang::QualType type) {
  unsigned int num_indirections_lhs = 0;
  type = type.getCanonicalType();
  while (type->isPointerType() || type->isReferenceType()) {
    ++num_indirections_lhs;
    type = type->getPointeeType();
  }
  return num_indirections_lhs;
}

clang::QualType Lifetime::GetTypeFromNumberIndirections(
    clang::QualType type, unsigned int number_indirections) {
  while (number_indirections > 0) {
    assert(type->isPointerType() || type->isReferenceType());
    type = type->getPointeeType();
    --number_indirections;
  }
  return type;
}

Lifetime Lifetime::InvalidEmpty() { return Lifetime(INVALID_EMPTY); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }

std::string Lifetime::GetLifetimeName(char id) {
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
  res += LifetimeType.getAsString();
  res += "\t\t[Lifetime] -> " + GetLifetimeName() + "; ";

  if (IsNotSet()) {
    res += "Shortest Lifetimes of this variable: { ";
    for (unsigned int i = 0; i < PossibleLifetimes.size(); i++) {
      if (!PossibleLifetimes[i].empty()) {
        res += GetLifetimeName(i) + ' ';
      }
    }
    res += "}";
  }
  return res + '\n';
}

void Lifetime::ProcessPossibleLifetimes() {
  // should only reach this if lifetime is not set
  assert(IsNotSet());
  if (ContainsLocal()) {
    // lifetime $local is the shortest possible
    SetLocal();
    return;
  }

  if (PossibleLifetimes.size() > 1 && ContainsStatic()) {
    // static outlives all others; if there are others lifetimes, static should
    // not belong to shortest lifetimes
    RemoveFromPossibleLifetimes(STATIC);
  }
  char unique_id = NOTSET;
  for (unsigned int i = 0; i < PossibleLifetimes.size(); i++) {
    if (!PossibleLifetimes[i].empty()) {
      if (unique_id != NOTSET)
        return;  // there are more than one shortest lifetimes
      unique_id = i;
    }
  }
  SetId(unique_id);

  // in all other cases, the Id of the lifetime remains NOTSET
  // - if PossibleLifetimes is empty, the lifetime of this variable is
  // undefined
  // - if PossibleLifetimes contains multiple lifetimes, then the
  // lifetime is the shortest among them since we cannot now which lifetimes
  // outlives which, then all of them are considered the shortest
}

std::optional<StmtDenseSet> Lifetime::GetStmts(char id) {
  assert(id != NOTSET);
  return (unsigned int)id >= PossibleLifetimes.size() ||
                 PossibleLifetimes[id].empty()
             ? std::nullopt
             : std::optional(PossibleLifetimes[id]);
}

bool Lifetime::operator==(const Lifetime &Other) const {
  if (Id != Other.GetId()) return false;
  if (IsSet()) return true;

  unsigned int min_size =
      std::min(PossibleLifetimes.size(), Other.GetPossibleLifetimes().size());
  const auto &larger_vec = PossibleLifetimes.size() > min_size
                               ? PossibleLifetimes
                               : Other.GetPossibleLifetimes();
  unsigned int max_size = larger_vec.size();
  unsigned int i = -1;

  while (++i < min_size) {
    if (PossibleLifetimes[i].empty() != Other.GetPossibleLifetimes()[i].empty())
      return false;
  }
  i--;
  while (++i < max_size) {
    if (!larger_vec[i].empty()) return false;
  }
  return true;
}

bool Lifetime::operator!=(const Lifetime &Other) const {
  return !operator==(Other);
}

bool Lifetime::operator<(const Lifetime &Other) const {
  // $static outlives all lifetimes and all lifetimes outlive $local
  if (IsStatic() || Other.IsLocal()) return false;
  if (IsLocal() || Other.IsStatic()) return true;

  const auto &other_possible_lifetimes = Other.GetPossibleLifetimes();

  if (IsSet() && Other.IsSet()) {
    return Id != Other.GetId();
  } else if (IsSet()) {
    return (unsigned int)Id >= other_possible_lifetimes.size() ||
           other_possible_lifetimes[Id].empty();
  } else if (Other.IsSet()) {
    return true;
  }

  // if both this and other are not set
  unsigned int min_size =
      std::min(PossibleLifetimes.size(), other_possible_lifetimes.size());
  unsigned int i = -1;
  while (++i < min_size) {
    if (!PossibleLifetimes[i].empty() && other_possible_lifetimes[i].empty())
      return true;
  }

  if (PossibleLifetimes.size() > other_possible_lifetimes.size()) {
    i--;
    while (++i < PossibleLifetimes.size()) {
      if (!PossibleLifetimes[i].empty()) return true;
    }
  }

  return false;
}

}  // namespace clang
