#include <iostream>
#include <string>

#include "clang/AST/ASTContext.h"

using namespace clang;
using namespace llvm;

class LifetimeAnnotationsChecker {
 public:
  void debug(std::string txt) { std::cout << txt << '\n'; }

  // TODO probably not void
  void GetLifetimes(FunctionDecl* func);
};