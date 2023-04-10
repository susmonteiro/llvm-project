#include "Lifetime.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {

Lifetime::Lifetime() : id_(UNSET) {}

Lifetime::Lifetime(char id) : id_(id) {
  debugLifetimes("In constructor, value of id", id);
  debugLifetimes("In constructor, value of id_", id_);
}

Lifetime::Lifetime(llvm::StringRef name) {
  debugLifetimes("=== Lifetime Name ===", name.str());  
  if ((isStatic = name.equals(STATIC_NAME))) {
    *this = Lifetime(STATIC);
  } else if ((isLocal = name.equals(LOCAL_NAME))) {
    *this = Lifetime(LOCAL);
  // TODO is this check ok?
  } else if (name.size() == 1 && name.front() >= 'a' && name.front() <= 'z') {
    char res = name.front() - 'a';
    debugLifetimes("Lifetime value", res);
    *this = Lifetime(res);
  } else {
    // TODO error
    // TODO change this
    Lifetime TODO;
  }
}

// TODO
// what is wrong here?
std::string Lifetime::getLifetimeName() const { 
  debugLifetimes("Letter 'a' has value", 'a');
  debugLifetimes("Variable 'id_' has value", id_);
  debugLifetimes("Get lifetime name", id_ + 'a');
  return std::string(1, id_ + 'a'); 
}

Lifetime Lifetime::InvalidEmpty() { return Lifetime(UNSET); }

Lifetime Lifetime::InvalidTombstone() {
  return Lifetime(INVALID_ID_TOMBSTONE);
}
}  // namespace clang