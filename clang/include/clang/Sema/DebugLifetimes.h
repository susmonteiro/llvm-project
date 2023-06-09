#ifndef LIFETIME_ANNOTATIONS_DEBUG_H_
#define LIFETIME_ANNOTATIONS_DEBUG_H_

#include <iostream>
#include <string>

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"

// TODO delete this file

void debugLifetimes(std::string txt);
void debugLifetimes(std::string txt1, std::string txt2);
void debugLifetimes(std::string txt, int i);
void debugLifetimes(llvm::SmallVector<const clang::Expr *> vec);
void debugLifetimes(llvm::SmallVector<const clang::Attr *> vec);
void debugLifetimes(llvm::SmallVector<std::string> vec);
void debugLifetimes(std::vector<const clang::VarDecl *> vec);
void debugLifetimes(llvm::DenseSet<const clang::VarDecl *> vec);
void debugLifetimes(llvm::DenseSet<char> vec);
void debugLifetimes(
    llvm::DenseMap<const clang::VarDecl *, llvm::DenseSet<const clang::Stmt *>>
        var_stmt,
    llvm::DenseMap<const clang::Stmt *, llvm::DenseSet<const clang::VarDecl *>>
        stmt_var);
void debugImportant(std::string txt);
void debugImportant(std::string txt1, std::string txt2);
void debugInfo(std::string txt);
void debugInfo(std::string txt, int i);
void debugInfo2(std::string txt);
void debugWarn(std::string txt);
void debugWarn2(std::string txt);
void debugLight(std::string txt);

#endif  // LIFETIME_ANNOTATIONS_DEBUG_H_
