#include "FunctionLifetimes.h"

#include <iostream>

// TODO remove this
#include "Debug.h"
#include "Lifetime.h"
#include "LifetimeTypes.h"
#include "clang/AST/Attr.h"
#include "llvm/Support/Error.h"

namespace clang {

llvm::Expected<Lifetime> FunctionLifetimeFactory::LifetimeFromName(
    const clang::Expr* name) const {
  llvm::StringRef name_str;
  if (llvm::Error err = EvaluateAsStringLiteral(name, func->getASTContext())
                            .moveInto(name_str)) {
    return std::move(err);
  }
  return symbol_table.LookupNameAndMaybeDeclare(name_str);
}

llvm::Expected<ValueLifetimes>
FunctionLifetimeFactory::CreateParamLifetimes(
    clang::QualType param_type, clang::TypeLoc param_type_loc) const {
  // TODO pendurado
  return ValueLifetimes::Create(param_type, param_type_loc,
                                ParamLifetimeFactory());
}

LifetimeFactory FunctionLifetimeFactory::ParamLifetimeFactory() const {
  return [this](const clang::Expr* name) -> llvm::Expected<Lifetime> {
    if (name) {
      Lifetime lifetime;
      if (llvm::Error err = LifetimeFromName(name).moveInto(lifetime)) {
        return std::move(err);
      }
      return lifetime;
    }

    // As a special-case, lifetime is always inferred for the `this`
    // parameter for destructors. The obvious lifetime is definitionally
    // correct in this case: the object must be valid for the duration
    // of the call, or else the behavior is undefined. So we can infer
    // safely even if elision is disabled.
    if (!elision_enabled && func->getDeclName().getNameKind() !=
                                clang::DeclarationName::CXXDestructorName) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Lifetime elision not enabled for function"
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
          );
    }

    Lifetime lifetime = Lifetime::CreateVariable();
    symbol_table.LookupLifetimeAndMaybeDeclare(lifetime);
    return lifetime;
  };
}

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
  /* return  */ Create(func->getType()->getAs<clang::FunctionProtoType>(),
                       type_loc, this_type, lifetime_factory);
}

void /* llvm::Expected<FunctionLifetimes> */ FunctionLifetimes::Create(
    const clang::FunctionProtoType* type, clang::TypeLoc type_loc,
    const clang::QualType this_type,
    const FunctionLifetimeFactory& lifetime_factory) {
  FunctionLifetimes ret;

  llvm::SmallVector<const clang::Attr*> attrs;
  if (type_loc) {
    debug("type_loc is defined as: ", type_loc.getType().getAsString());
    debug("and the attributes are:");
    StripAttributes(type_loc, attrs);
    for (const auto& attr : attrs) {  // DEBUG
      std::cout << attr << '\n';
    }
  }

  llvm::SmallVector<const clang::Expr*> lifetime_names;
  if (llvm::Error err = GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
    // TODO uncomment return
    // return std::move(err);
  }

  debug("lifetime names");
  for (const auto& name : lifetime_names) {  // DEBUG
    name->dump();
  }

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

  ret.param_lifetimes_.reserve(type->getNumParams());
  debug("Num of parameters", type->getNumParams());

  for (size_t i = 0; i < type->getNumParams(); i++) {
    clang::TypeLoc param_type_loc;
    if (type_loc) {
      const clang::ParmVarDecl* param = func_type_loc.getParam(i);
      if (param && param->getTypeSourceInfo()) {
        param_type_loc = param->getTypeSourceInfo()->getTypeLoc();
      }
    }
    ValueLifetimes tmp;
    if (llvm::Error err =
            lifetime_factory
                .CreateParamLifetimes(type->getParamType(i), param_type_loc)
                .moveInto(tmp)) {
      // TODO pendurado
      // return std::move(err);
    }
    ret.param_lifetimes_.push_back(std::move(tmp));
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