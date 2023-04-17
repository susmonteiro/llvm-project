#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

void LifetimeAnnotationsAnalysis::CreateDependency(
    const clang::VarDecl *from, const clang::DeclRefExpr *to) {
  // TODO implement
  // clang::QualType type = to->getType().getCanonicalType();
  // // TODO necessary?
  // if (type->isArrayType()) {
  //   type = type->castAsArrayTypeUnsafe()->getElementType();
  // }

  // if (type->isRecordType()) {
  //   // TODO implement
  //   return;
  // }

  // if (type->isPointerType() || type->isReferenceType() ||
  //     type->isStructureOrClassType()) {
  //   // TODO implement
  // }
  const clang::NamedDecl *decl = to->getFoundDecl();
  dependencies_[from].insert(decl);
}

Dependencies LifetimeAnnotationsAnalysis::TransposeDependencies() const {
  Dependencies result;
  for (const auto &el : dependencies_) {
    for (const auto &child : el.second) {
      result[child].insert(el.first);
    }
  }
  return result;
}

std::vector<const clang::NamedDecl *>
LifetimeAnnotationsAnalysis::InitializeWorklist() const {
  std::vector<const clang::NamedDecl *> worklist;
  for (const auto &pair : dependencies_) {
    worklist.emplace_back(pair.first);
  }
  return worklist;
}

}  // namespace clang
