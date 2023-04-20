#ifndef LIFETIME_ANNOTATIONS_DEBUG_H_
#define LIFETIME_ANNOTATIONS_DEBUG_H_

#include <iostream>
#include <string>

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseSet.h"

// TODO delete this file

void debugLifetimes(std::string txt);
void debugLifetimes(std::string txt1, std::string txt2);
void debugLifetimes(std::string txt, int i);
void debugLifetimes(llvm::SmallVector<const clang::Expr*> vec);
void debugLifetimes(llvm::SmallVector<const clang::Attr*> vec);
void debugLifetimes(llvm::SmallVector<std::string> vec);
void debugLifetimes(std::vector<const clang::NamedDecl*> vec);
void debugLifetimes(llvm::DenseSet<const clang::NamedDecl*> vec);
void debugLifetimes(llvm::DenseMap<const clang::NamedDecl *, llvm::DenseSet<const clang::NamedDecl *>> m);

void debugInfo(std::string txt);
void debugInfo(std::string txt, int i);
void debugWarn(std::string txt);
void debugLight(std::string txt);

#endif  // LIFETIME_ANNOTATIONS_DEBUG_H_
