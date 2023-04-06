#include "FunctionLifetimes.h"
// TODO remove this
#include "Debug.h"
#include <iostream>

namespace clang {

clang::TypeLoc StripAttributes(clang::TypeLoc type_loc,
                               llvm::SmallVector<const clang::Attr*>& attrs) {
  while (true) {
    if (auto macro_qualified_type_loc =
            type_loc.getAs<clang::MacroQualifiedTypeLoc>()) {
      type_loc = macro_qualified_type_loc.getInnerLoc();
    } else if (auto attr_type_loc =
                   type_loc.getAs<clang::AttributedTypeLoc>()) {
      attrs.push_back(attr_type_loc.getAttr());
      type_loc = attr_type_loc.getModifiedLoc();
    } else {
      // The last attribute in a chain of attributes will appear as the
      // outermost AttributedType, e.g. `int $a $b` will be represented as
      // follows (in pseudocode):
      //
      // AttributedType(annotate_type("lifetime", "b"),
      //   AttributedType(annotate_type("lifetime", "a"),
      //     BuiltinType("int")
      //   )
      // )
      //
      // We reverse the attributes so that we obtain the more intuitive ordering
      // "a, b".
      std::reverse(attrs.begin(), attrs.end());
      return type_loc;
    }
  }
}

// llvm::Expected<llvm::SmallVector<const clang::Expr*>> GetAttributeLifetimes(
//     llvm::ArrayRef<const clang::Attr*> attrs) {
//   llvm::SmallVector<const clang::Expr*> result;

//   bool saw_annotate_type = false;

//   for (const clang::Attr* attr : attrs) {
//     auto annotate_type_attr = clang::dyn_cast<clang::AnnotateTypeAttr>(attr);
//     if (!annotate_type_attr ||
//         annotate_type_attr->getAnnotation() != "lifetime")
//       continue;

//     if (saw_annotate_type) {
//       return llvm::createStringError(
//           llvm::inconvertibleErrorCode(),
//           "Only one `[[annotate_type(\"lifetime\", ...)]]` attribute may be "
//           "placed on a type");
//     }
//     saw_annotate_type = true;

//     for (const clang::Expr* arg : annotate_type_attr->args()) {
//       result.push_back(arg);
//     }
//   }

//   return result;
// }


void /* llvm::Expected<FunctionLifetimes> */ FunctionLifetimes::CreateForDecl(
    const clang::FunctionDecl* func,
    const FunctionLifetimeFactory& lifetime_factory) {
  // + represents a type with its qualifiers: const, volatile, restrict, etc
  // + the clang AST contains QualTypes to describe the types of variables,
  // expressions, etc
  clang::QualType this_type;
  if (auto method = clang::dyn_cast<clang::CXXMethodDecl>(func);
      method && !method->isStatic()) {
    this_type = method->getThisType();
  }
  clang::TypeLoc type_loc;
  if (func->getTypeSourceInfo()) {
    type_loc = func->getTypeSourceInfo()->getTypeLoc();
  }
  /* return  */Create(func->getType()->getAs<clang::FunctionProtoType>(), type_loc,
                this_type, lifetime_factory);
}

void /* llvm::Expected<FunctionLifetimes> */ FunctionLifetimes::Create(
    const clang::FunctionProtoType* type, clang::TypeLoc type_loc,
    const clang::QualType this_type,
    const FunctionLifetimeFactory& lifetime_factory) {
  FunctionLifetimes ret;

  llvm::SmallVector<const clang::Attr*> attrs;
  if (type_loc) {
    debug("type_loc is defined and the attributes are:");
    StripAttributes(type_loc, attrs);
    for (const auto &attr : attrs) {
        std::cout << attr << '\n';
    }
  }

  llvm::SmallVector<const clang::Expr*> lifetime_names;
    // if (llvm::Error err =
    // GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
    //   return std::move(err);
    // }

  /* if (this_type.isNull() && !lifetime_names.empty()) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          absl::StrCat("Encountered a `this` lifetime on a function with no "
                       "`this` parameter"));
    } */

  /* if (!this_type.isNull()) {
  ValueLifetimes tmp;
  const clang::Expr* lifetime_name = nullptr;
  if (!lifetime_names.empty()) {
    if (lifetime_names.size() != 1) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          absl::StrCat("Expected a single lifetime but ",
                       lifetime_names.size(), " were given"));
    }
    lifetime_name = lifetime_names.front();
  }
  if (llvm::Error err =
          lifetime_factory.CreateThisLifetimes(this_type, lifetime_name)
              .moveInto(tmp)) {
    return std::move(err);
  }
  ret.this_lifetimes_ = std::move(tmp);
} */

  clang::FunctionTypeLoc func_type_loc;
  if (type_loc) {
    func_type_loc = type_loc.getAsAdjusted<clang::FunctionTypeLoc>();
  }

//   ret.param_lifetimes_.reserve(type->getNumParams());

  for (size_t i = 0; i < type->getNumParams(); i++) {
    clang::TypeLoc param_type_loc;
    if (type_loc) {
      const clang::ParmVarDecl* param = func_type_loc.getParam(i);
      if (param && param->getTypeSourceInfo()) {
        param_type_loc = param->getTypeSourceInfo()->getTypeLoc();
      }
    }
    // ValueLifetimes tmp;
    /* if (llvm::Error err =
            lifetime_factory
                .CreateParamLifetimes(type->getParamType(i), param_type_loc)
                .moveInto(tmp)) {
      return std::move(err);
    } */
    // ret.param_lifetimes_.push_back(std::move(tmp));
  }
  clang::TypeLoc return_type_loc;
  if (func_type_loc) {
    return_type_loc = func_type_loc.getReturnLoc();
  }

  /* if (llvm::Error err =
          lifetime_factory
              .CreateReturnLifetimes(type->getReturnType(), return_type_loc,
                                     ret.param_lifetimes_, ret.this_lifetimes_)
              .moveInto(ret.return_lifetimes_)) {
    return std::move(err);
  } */

//   return ret;
}

}  // namespace clang