#include "clang/Sema/DebugLifetimes.h"

#include <iostream>

// TODO remove this file

void debugLifetimes(std::string txt) { std::cout << txt << '\n'; }

void debugLifetimes(std::string txt1, std::string txt2) {
  std::cout << txt1 << ": " << txt2 << '\n';
}

void debugLifetimes(std::string txt, int i) {
  std::cout << txt << ": " << i << '\n';
}

void debugLifetimes(llvm::SmallVector<const clang::Expr *> vec) {
  for (const auto &el : vec) {
    el->dump();
  }
}

void debugLifetimes(llvm::SmallVector<const clang::Attr *> vec) {
  for (const auto &el : vec) {
    el->getNormalizedFullName();
  }
}

void debugLifetimes(llvm::SmallVector<std::string> vec) {
  for (const auto &el : vec) {
    debugLifetimes(el);
  }
}

void debugLifetimes(std::vector<const clang::NamedDecl *> vec) {
  std::string res;
  for (const auto &el : vec) {
    res += el->getNameAsString() + ' ';
  }
  res += '\n';
  debugLifetimes(res);
}

void debugLifetimes(llvm::DenseSet<const clang::NamedDecl *> vec) {
  for (const auto &el : vec) {
    debugLifetimes(el->getNameAsString());
  }
}

void debugLifetimes(llvm::DenseSet<char> vec) {
  std::string res = "DenseSet contains: ";
  for (const auto el : vec) {
    res += el + ' ';
  }
  debugLifetimes(res);
}

void debugLifetimes(llvm::DenseMap<const clang::NamedDecl *,
                                   llvm::DenseSet<const clang::NamedDecl *>>
                        m) {
  std::string res;
  for (const auto &pair : m) {
    res += "Dependencies of " + pair.first->getNameAsString() + ": ";
    for (const auto &var : pair.second) {
      res += var->getNameAsString() + ' ';
    }
    res += '\n';
  }
  debugLifetimes(res);
}

void debugImportant(std::string txt) {
  std::cout << "\033[1;96m======== " << txt << " ========\033[0m\n";
}
void debugImportant(std::string txt1, std::string txt2) {
  std::cout << "\033[1;96m======== " << txt1 << ": " << txt2 << " ========\033[0m\n";
}
void debugInfo(std::string txt) {
  std::cout << "\033[1;34m" << txt << "\033[0m\n";
}
void debugInfo2(std::string txt) {
  std::cout << "\033[1;35m" << txt << "\033[0m\n";
}
void debugInfo(std::string txt, int i) {
  std::cout << "\033[1;92m" << txt << ' ' << i << "\033[0m\n\n";
}
void debugWarn(std::string txt) {
  std::cout << "\033[1;31m" << txt << "\033[0m\n";
}
void debugWarn2(std::string txt) {
  std::cout << "\033[1;36m" << txt << "\033[0m\n";
}
void debugLight(std::string txt) {
  std::cout << "\033[1;90m" << txt << "\033[0m\n";
}
