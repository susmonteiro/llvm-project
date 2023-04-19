#include "clang/Sema/Lifetime.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {

constexpr char NOTSET = -1;
constexpr char STATIC = -2;
constexpr char LOCAL = -3;
constexpr char INVALID_ID_TOMBSTONE = -4;
constexpr char TODO = 100;

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
    *this = Lifetime(name.front() - 'a');
  } else {
    // TODO error
    // TODO change this
    *this = Lifetime(TODO);
  }
}

std::string Lifetime::GetLifetimeName(char id) const {
  switch(id) {
    case NOTSET:
    case INVALID_ID_TOMBSTONE:
    case TODO:
      return "not-set";
      break;
    case STATIC:
      return "static";
      break;
    case LOCAL:
      return "local";
      break;
    default:
      if (id >= 0 && id <= 25) return std::string(1, id + 'a'); 
      // TODO error
      else return "error";
  }
}

bool Lifetime::IsNotSet() const { return id_ == NOTSET; }
bool Lifetime::IsStatic() const { return id_ == STATIC; }
bool Lifetime::IsLocal() const { return id_ == LOCAL; }
void Lifetime::SetStatic() { id_ = STATIC; }
void Lifetime::SetLocal() { id_ = LOCAL; }

Lifetime Lifetime::InvalidEmpty() { return Lifetime(NOTSET); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }
}  // namespace clang
