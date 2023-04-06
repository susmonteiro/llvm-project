#include "FunctionLifetimes.h"

namespace clang {
//     llvm::Expected<FunctionLifetimes> FunctionLifetimes::CreateForDecl(
//     const clang::FunctionDecl* func,
//     const FunctionLifetimeFactory& lifetime_factory) {
//   // + represents a type with its qualifiers: const, volatile, restrict, etc
//   // + the clang AST contains QualTypes to describe the types of variables, expressions, etc
//   clang::QualType this_type;
//   if (auto method = clang::dyn_cast<clang::CXXMethodDecl>(func);
//       method && !method->isStatic()) {
//     this_type = method->getThisType();
//   }
//   clang::TypeLoc type_loc;
//   if (func->getTypeSourceInfo()) {
//     type_loc = func->getTypeSourceInfo()->getTypeLoc();
//   }
//   return Create(func->getType()->getAs<clang::FunctionProtoType>(), type_loc,
//                 this_type, lifetime_factory);
// }


}