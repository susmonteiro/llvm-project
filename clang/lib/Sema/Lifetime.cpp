#include "Lifetime.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {

Lifetime::Lifetime() : id_(UNSET) {}

Lifetime::Lifetime(char id) : id_(id) {}

Lifetime::Lifetime(llvm::StringRef name) {
  if ((isStatic = name.equals(STATIC_NAME))) {
    *this = Lifetime(STATIC);
  } else if ((isLocal = name.equals(LOCAL_NAME))) {
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

std::string Lifetime::getLifetimeName() const {
  return std::string(1, id_ + 'a');
}

Lifetime Lifetime::InvalidEmpty() { return Lifetime(UNSET); }

Lifetime Lifetime::InvalidTombstone() { return Lifetime(INVALID_ID_TOMBSTONE); }
}  // namespace clang