#include <iostream>
#include <string>

#include "clang/AST/ASTContext.h"

namespace clang {

class LifetimeAnnotationsChecker {
 public:
  void debug(std::string txt) { std::cout << txt << '\n'; }

  // TODO probably not void
  void GetLifetimes(FunctionDecl* func);
};
}