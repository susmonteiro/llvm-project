#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

void LifetimeAnnotationsAnalysis::CreateDependency(
    const clang::VarDecl *var_decl) {
  clang::QualType type = var_decl->getType().getCanonicalType();
  // TODO necessary?
  if (type->isArrayType()) {
    type = type->castAsArrayTypeUnsafe()->getElementType();
  }

  if (type->isRecordType()) {
    // TODO implement
    return;
  }

  if (type->isPointerType() || type->isReferenceType() ||
      type->isStructureOrClassType()) {
    // TODO implement
  }
}
}  // namespace clang
