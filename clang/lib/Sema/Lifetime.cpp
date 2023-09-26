#include "clang/Sema/Lifetime.h"

namespace clang {

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime(char id) { InitializeId(id); }

Lifetime::Lifetime(char id, clang::QualType type)
    : LifetimeType(type), NumIndirections(Lifetime::GetNumIndirections(type)) {
  InitializeId(id);
}

Lifetime::Lifetime(llvm::StringRef name, clang::QualType type)
    : LifetimeType(type), NumIndirections(Lifetime::GetNumIndirections(type)) {
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

Lifetime::Lifetime(char id, unsigned int num_indirections) {
  InitializeId(id);
  SetNumIndirections(num_indirections);
}

void Lifetime::InitializeId(char id) {
  if (id == NOTSET || id == LOCAL || id == STATIC || id == DEAD) {
    Id = id;
  } else if (id >= 'a' && id <= 'z') {
    Id = CharToId(id);
  } else {
    // TODO error
    // TODO change this
    Id = NOTSET;
  }
}

bool Lifetime::IsSet() const { return Id >= DEAD; }
bool Lifetime::IsNotSet() const { return Id == NOTSET; }
bool Lifetime::IsStatic() const { return Id == STATIC; }
bool Lifetime::ContainsStatic() const {
  return (unsigned int)STATIC < Dependencies.size() &&
         !Dependencies[STATIC].empty();
}

bool Lifetime::IsLocal() const { return Id == LOCAL; }
bool Lifetime::ContainsLocal() const {
  return (unsigned int)LOCAL < Dependencies.size() &&
         !Dependencies[LOCAL].empty();
}

bool Lifetime::IsDead() const { return Id == DEAD; }
bool Lifetime::ContainsDead() const {
  return (unsigned int)DEAD < Dependencies.size() &&
         !Dependencies[DEAD].empty();
}

void Lifetime::SetStatic() { Id = STATIC; }
void Lifetime::SetLocal() { Id = LOCAL; }
void Lifetime::SetDead() { Id = DEAD; }

bool Lifetime::IsNull() const { return Id == NULL_LIFETIME; }

unsigned int Lifetime::GetNumIndirections(clang::QualType type) {
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
    case DEAD:
      return "$dead";
      break;
    case NULL_LIFETIME:
      return "NULL";
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
  if (LifetimeType.isNull())
    res += std::string(NumIndirections, '*');
  else
    res += LifetimeType.getAsString();
  res += "\t\t[Lifetime] -> " + GetLifetimeName() + "; ";

  if (IsNotSet()) {
    res += "Shortest Lifetimes of this variable: { ";
    for (unsigned int i = 0; i < Dependencies.size(); i++) {
      if (ContainsShortestLifetime(i)) {
        res += GetLifetimeName(i) + ' ';
      }
    }
    res += "}";
  }
  return res + '\n';
}

bool Lifetime::EmptyDependencies() const {
  unsigned idx = DEAD;
  unsigned size = Dependencies.size();
  while (++idx < size && Dependencies[idx].empty()) {
  }
  return idx >= size;
}

void Lifetime::ProcessDependencies() {
  // should only reach this if lifetime is not set
  assert(IsNotSet());
  if (ContainsDead()) {
    SetDead();
    return;
  }

  if (ContainsLocal()) {
    // lifetime $local is the shortest possible
    SetLocal();
    return;
  }

  char unique_id = NOTSET;
  for (unsigned int i = 0; i < Dependencies.size(); i++) {
    if (!Dependencies[i].empty()) {
      if (unique_id != NOTSET) {
        // set all possible lifetimes as shortest
        for (unsigned int j = OFFSET; j < Dependencies.size(); j++) {
          if (!Dependencies[j].empty() && j != STATIC) {
            shortest_insert(ShortestLifetimes, j);
          }
        }
        return;  // there are more than one shortest lifetimes
      }
      unique_id = i;
    }
  }

  SetId(unique_id);

  // in all other cases, the Id of the lifetime remains NOTSET
  // - if Dependencies is empty, the lifetime of this variable is
  // undefined
  // - if Dependencies contains multiple lifetimes, then the
  // lifetime is the shortest among them since we cannot know which lifetimes
  // outlives which, then all of them are considered the shortest
}

std::optional<StmtDenseSet> Lifetime::GetStmts(char id) {
  assert(id != NOTSET);
  return (unsigned int)id >= Dependencies.size() ||
                 Dependencies[id].empty()
             ? std::nullopt
             : std::optional(Dependencies[id]);
}

bool Lifetime::operator==(const Lifetime &Other) const {
  if (IsNull() || Other.IsNull()) return false;
  
  if (Id != Other.GetId()) {
    return (Id == NOTSET && EmptyDependencies()) ||
           (Other.GetId() == NOTSET && Other.EmptyDependencies());
  }

  if (IsSet()) return true;

  unsigned int min_size =
      std::min(Dependencies.size(), Other.GetDependencies().size());
  const Lifetime &larger_lifetime =
      Dependencies.size() > min_size ? *this : Other;
  unsigned int max_size = larger_lifetime.GetDependencies().size();
  unsigned int i = -1;

  while (++i < min_size) {
    if (ContainsShortestLifetime(i) != Other.ContainsShortestLifetime(i))
      return false;
  }
  i--;
  while (++i < max_size) {
    if (larger_lifetime.ContainsShortestLifetime(i)) {
      return false;
    }
  }
  return true;
}

bool Lifetime::operator!=(const Lifetime &Other) const {
  if (IsDead() && Other.IsDead()) {
    // TODO delete this
    assert(true && "Should not reach this");
    return GetPossibleLifetime(DEAD) == Other.GetPossibleLifetime(DEAD);
  }
  return !operator==(Other);
}

bool Lifetime::operator<(const Lifetime &Other) const {
  // $static outlives all lifetimes and all lifetimes outlive $local
  if (IsDead() && Other.IsDead()) {
    // TODO delete this
    const auto &this_dependencies = GetPossibleLifetime(DEAD);
    const auto &other_dependencies = Other.GetPossibleLifetime(DEAD);
    if (this_dependencies.size() < other_dependencies.size()) {
      return false;
    }
    for (const auto *stmt : this_dependencies) {
      if (other_dependencies.find(stmt) == other_dependencies.end())
        return true;
    }
    return false;
  } else if (IsDead() || Other.IsDead()) {
    return IsDead();
  }
  if (IsStatic() || Other.IsLocal()) return false;
  if (IsLocal() || Other.IsStatic()) return true;

  const auto &other_dependenciess = Other.GetDependencies();

  if (IsSet() && Other.IsSet()) {
    return Id != Other.GetId();
  } else if (IsSet()) {
    return (unsigned int)Id >= other_dependenciess.size() ||
           other_dependenciess[Id].empty();
  } else if (Other.IsSet()) {
    return true;
  }

  // if both this and other are not set
  unsigned int min_size =
      std::min(Dependencies.size(), other_dependenciess.size());
  unsigned int i = -1;
  while (++i < min_size) {
    if (ContainsShortestLifetime(i) && !Other.ContainsShortestLifetime(i))
      return true;
  }

  if (Dependencies.size() > other_dependenciess.size()) {
    i--;
    while (++i < Dependencies.size()) {
      if (ContainsShortestLifetime(i)) return true;
    }
  }

  return false;
}

}  // namespace clang
