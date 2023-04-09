#include "LifetimeNew.h"

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

LifetimeNew::LifetimeNew() : id_(UNSET) {}

LifetimeNew::LifetimeNew(int id) : id_(id) {}

LifetimeNew::LifetimeNew(llvm::StringRef name) {
  debugLifetimes("Lifetime name", name.str());
  // TODO change this
  LifetimeNew(TODO);
}

LifetimeNew LifetimeNew::InvalidEmpty() { return LifetimeNew(UNSET); }

LifetimeNew LifetimeNew::InvalidTombstone() {
  return LifetimeNew(INVALID_ID_TOMBSTONE);
}
}  // namespace clang