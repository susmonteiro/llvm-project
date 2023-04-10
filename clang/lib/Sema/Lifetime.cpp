#include "Lifetime.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {
constexpr int UNSET = 0;
constexpr int STATIC = -1;
constexpr int LOCAL = -2;
constexpr int INVALID_ID_TOMBSTONE = -3;
constexpr int TODO = 100;

constexpr llvm::StringRef STATIC_NAME = "static";
constexpr llvm::StringRef LOCAL_NAME = "local";

Lifetime::Lifetime() : id_(UNSET) {}

Lifetime::Lifetime(int id) : id_(id) {}

Lifetime::Lifetime(llvm::StringRef name) {
  debugLifetimes("=== Lifetime Name ===", name.str());
  if ((isStatic = name.equals(STATIC_NAME))) {
    // TODO is this correct?
    Lifetime STATIC;
  } else if ((isLocal = name.equals(LOCAL_NAME))) {
    Lifetime LOCAL;
  // TODO is this check ok?
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    Lifetime(name.front() - 'a' + 1);
  } else {
    // TODO error
    // TODO change this
    Lifetime TODO;
  }
}

Lifetime Lifetime::InvalidEmpty() { return Lifetime(UNSET); }

Lifetime Lifetime::InvalidTombstone() {
  return Lifetime(INVALID_ID_TOMBSTONE);
}
}  // namespace clang