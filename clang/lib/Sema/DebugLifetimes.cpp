#include "clang/Sema/DebugLifetimes.h"

#include <iostream>

// TODO remove this file

bool stop_debug = false;

void debugLifetimes(std::string txt) {
  if (stop_debug) return;
  std::cout << txt << '\n';
}

void debugLifetimes(std::string txt1, std::string txt2) {
  if (stop_debug) return;
  std::cout << txt1 << ": " << txt2 << '\n';
}

void debugLifetimes(std::string txt, int i) {
  if (stop_debug) return;
  std::cout << txt << ": " << i << '\n';
}

void debugLifetimes(llvm::SmallVector<const clang::Expr *> vec) {
  if (stop_debug) return;
  for (const auto &el : vec) {
    el->dump();
  }
}

void debugLifetimes(llvm::SmallVector<const clang::Attr *> vec) {
  if (stop_debug) return;
  for (const auto &el : vec) {
    el->getNormalizedFullName();
  }
}

void debugLifetimes(llvm::SmallVector<std::string> vec) {
  if (stop_debug) return;
  for (const auto &el : vec) {
    debugLifetimes(el);
  }
}

void debugLifetimes(std::vector<const clang::VarDecl *> vec) {
  if (stop_debug) return;
  std::string res;
  for (const auto &el : vec) {
    res += el->getNameAsString() + ' ';
  }
  res += '\n';
  debugLifetimes(res);
}

void debugLifetimes(
    std::vector<std::pair<const clang::VarDecl *, clang::QualType>> vec) {
  if (stop_debug) return;
  std::string res;
  for (const auto &el : vec) {
    res += '{' + el.second.getAsString() + ' ' + el.first->getNameAsString() +
           "}, ";
  }
  res += '\n';
  debugLifetimes(res);
}

void debugLifetimes(llvm::DenseSet<const clang::VarDecl *> vec) {
  if (stop_debug) return;
  for (const auto &el : vec) {
    debugLifetimes(el->getNameAsString());
  }
}

void debugLifetimes(llvm::DenseSet<char> vec) {
  if (stop_debug) return;
  std::string res = "DenseSet contains: ";
  for (const auto el : vec) {
    res += el + ' ';
  }
  debugLifetimes(res);
}

void debugLifetimes(
    const clang::VarDecl *var, clang::QualType type,
    llvm::DenseSet<const clang::Stmt *> var_stmt,
    llvm::DenseMap<const clang::Stmt *, llvm::DenseSet<const clang::VarDecl *>>
        stmt_var) {
  if (stop_debug) return;
  std::string res;
  res += "Dependencies of " + var->getNameAsString() + ": ";
  res += "[Type]: " + type.getAsString() + "\t[vars]: ";
  for (const auto &stmt : var_stmt) {
    for (const auto &rhs_var : stmt_var[stmt]) {
      if (var == rhs_var) continue;
      res += rhs_var->getNameAsString() + ' ';
    }
  }
  res += '\n';
  debugLifetimes(res);
}

void debugImportant(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;96m======== " << txt << " ========\033[0m\n";
}

void debugImportant(std::string txt1, std::string txt2) {
  if (stop_debug) return;
  std::cout << "\033[1;96m======== " << txt1 << ": " << txt2
            << " ========\033[0m\n";
}

void debugInfo(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;34m" << txt << "\033[0m\n";
}

void debugInfo(std::string txt, int i) {
  if (stop_debug) return;
  std::cout << "\033[1;92m" << txt << ' ' << i << "\033[0m\n\n";
}

void debugInfo2(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;35m" << txt << "\033[0m\n";
}

void debugWarn(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;31m" << txt << "\033[0m\n";
}

void debugWarn2(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;36m" << txt << "\033[0m\n";
}

void debugLight(std::string txt) {
  if (stop_debug) return;
  std::cout << "\033[1;90m" << txt << "\033[0m\n";
}
