#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

void LifetimeAnnotationsAnalysis::CreateDependency(
    const clang::NamedDecl *from, const clang::DeclRefExpr *to) {
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
  for (const auto &pair : dependencies_) {
    for (const auto &child : pair.second) {
      // don't insert annotated variables into the parents graph
      if (IsLifetimeNotset(child))
        result[child].insert(pair.first);
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

void LifetimeAnnotationsAnalysis::ProcessShortestLifetimes() {
  // iterate over variables with no fixed lifetime
  for (const auto &pair : dependencies_) {
    auto &lifetime = GetLifetime(pair.first);
    if (lifetime.IsNotSet()) {
      lifetime.ProcessShortestLifetimes();
    } else {
      // TODO error
    }
  }
}


}  // namespace clang
