#include "FunctionLifetimes.h"

#include <iostream>

#include "clang/AST/Attr.h"
#include "llvm/Support/Error.h"

namespace clang {

llvm::Expected<llvm::StringRef> EvaluateAsStringLiteral(
    const clang::Expr* expr, const clang::ASTContext& ast_context) {
  auto error = []() {
    return llvm::createStringError(
        llvm::inconvertibleErrorCode(),
        "cannot evaluate argument as a string literal");
  };

  clang::Expr::EvalResult eval_result;
  if (!expr->EvaluateAsConstantExpr(eval_result, ast_context) ||
      !eval_result.Val.isLValue()) {
    return error();
  }

  const auto* eval_result_expr =
      eval_result.Val.getLValueBase().dyn_cast<const clang::Expr*>();
  if (!eval_result_expr) {
    return error();
  }

  const auto* strlit = clang::dyn_cast<clang::StringLiteral>(eval_result_expr);
  if (!strlit) {
    return error();
  }

  return strlit->getString();
}

clang::QualType StripAttributes(clang::QualType type) {
  while (true) {
    if (auto macro_qualified_type = type->getAs<clang::MacroQualifiedType>()) {
      type = macro_qualified_type->getUnderlyingType();
    } else if (auto attr_type = type->getAs<clang::AttributedType>()) {
      type = attr_type->getModifiedType();
    } else {
      return type;
    }
  }
}

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

llvm::Expected<llvm::SmallVector<const clang::Expr*>> GetAttributeLifetimes(
    llvm::ArrayRef<const clang::Attr*> attrs) {
  llvm::SmallVector<const clang::Expr*> result;

  bool saw_annotate_type = false;

  for (const clang::Attr* attr : attrs) {
    auto annotate_type_attr = clang::dyn_cast<clang::AnnotateTypeAttr>(attr);
    if (!annotate_type_attr ||
        annotate_type_attr->getAnnotation() != "lifetime")
      continue;

    if (saw_annotate_type) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Only one `[[annotate_type(\"lifetime\", ...)]]` attribute may be "
          "placed on a type");
    }
    saw_annotate_type = true;

    for (const clang::Expr* arg : annotate_type_attr->args()) {
      result.push_back(arg);
    }
  }

  return result;
}

llvm::SmallVector<std::string> GetLifetimeParameters(clang::QualType type) {
  auto record = type->getAs<clang::RecordType>();
  if (!record) {
    return {};
  }

  auto cxx_record = record->getAsCXXRecordDecl();
  if (!cxx_record) {
    return {};
  }

  const clang::AnnotateAttr* lifetime_params_attr = nullptr;
  for (auto annotate : cxx_record->specific_attrs<clang::AnnotateAttr>()) {
    if (annotate->getAnnotation() == "lifetime_params") {
      if (lifetime_params_attr) {
        llvm::report_fatal_error("repeated lifetime annotation");
      }
      lifetime_params_attr = annotate;
    }
  }

  llvm::SmallVector<std::string> ret;

  if (cxx_record->hasDefinition()) {
    for (const clang::CXXBaseSpecifier& base : cxx_record->bases()) {
      if (lifetime_params_attr) {
        llvm::report_fatal_error(
            "derived classes may not add lifetime parameters");
      }
      if (!ret.empty()) {
        llvm::report_fatal_error(
            "only one base class may have lifetime parameters");
      }
      ret = GetLifetimeParameters(base.getType());
    }
  }

  if (!lifetime_params_attr) {
    return ret;
  }

  for (const auto& arg : lifetime_params_attr->args()) {
    llvm::StringRef lifetime;
    if (llvm::Error err =
            EvaluateAsStringLiteral(arg, cxx_record->getASTContext())
                .moveInto(lifetime)) {
      llvm::report_fatal_error(llvm::StringRef(toString(std::move(err))));
    }
    ret.push_back(lifetime.str());
  }

  return ret;
}

QualType Undecay(clang::QualType type) {
  if (auto decayed = type->getAs<clang::DecayedType>()) {
    return decayed->getOriginalType();
  }
  return type;
}

bool SameType(clang::QualType type1, clang::QualType type2) {
  // If both types are an AutoType, ignore the actual type and assume theyc're
  // the same.
  // An `AutoType` that came from a TypeLoc will have type `auto` (i.e. as
  // written), whereas an `AutoType` that didn't come from a `TypeLoc` will be
  // the actual deduced type. We still want these to compare equal though.
  if (type1->getAs<clang::AutoType>() && type2->getAs<clang::AutoType>()) {
    return true;
  }
  return Undecay(type1) == Undecay(type2);
}

llvm::Expected<Lifetime> FunctionLifetimeFactory::LifetimeFromName(
    const clang::Expr* name) const {
  llvm::StringRef name_str;
  if (llvm::Error err = EvaluateAsStringLiteral(name, func->getASTContext())
                            .moveInto(name_str)) {
    return std::move(err);
  }
  return Lifetime(name_str);
}

llvm::Expected<Lifetime> FunctionLifetimeFactory::CreateParamLifetimesNew(
    clang::QualType param_type, clang::TypeLoc param_type_loc) const {
  return CreateLifetime(param_type, param_type_loc, ParamLifetimeFactory());
}

llvm::Expected<Lifetime> FunctionLifetimeFactory::CreateReturnLifetimes(
    clang::QualType return_type, clang::TypeLoc return_type_loc,
    llvm::DenseMap<const clang::Decl*, Lifetime>) const {
  // TODO is this needed?
  // std::optional<Lifetime> input_lifetime =
  // GetSingleInputLifetime(param_lifetimes, this_lifetimes);
  return CreateLifetime(return_type, return_type_loc, ReturnLifetimeFactory());
}

llvm::Expected<Lifetime> FunctionLifetimeFactory::CreateLifetime(
    clang::QualType type, clang::TypeLoc type_loc,
    LifetimeFactory lifetime_factory) {
  assert(!type.isNull());
  if (type_loc) {
    assert(SameType(type_loc.getType(), type));
  }

  // DEBUG
  // debugLifetimes("Type");
  // type.dump();

  type = type.IgnoreParens();
  type = StripAttributes(type);

  llvm::SmallVector<const clang::Attr*> attrs;
  if (!type_loc.isNull()) {
    type_loc = StripAttributes(type_loc, attrs);
    // DEBUG
    // debugLifetimes("Attributes");
    // debugLifetimes(attrs);
  }

  llvm::SmallVector<const clang::Expr*> lifetime_names;
  if (llvm::Error err = GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
    return std::move(err);
  }

  // DEBUG
  // debugLifetimes("Lifetime names");
  // debugLifetimes(lifetime_names);

  llvm::SmallVector<std::string> lifetime_params = GetLifetimeParameters(type);

  // DEBUG
  // debugLifetimes("Lifetime parameters");
  // debugLifetimes(lifetime_params);

  if (!lifetime_params.empty() && !lifetime_names.empty() &&
      lifetime_names.size() != lifetime_params.size()) {
    return llvm::createStringError(
        llvm::inconvertibleErrorCode(),
        "Number of types and lifetimes not the same"
        // TODO abseil
        /* absl::StrCat("Type has ", lifetime_params.size(),
                     " lifetime parameters but ", lifetime_names.size(),
                     " lifetime arguments were given") */);
  }

  for (size_t i = 0; i < lifetime_params.size(); ++i) {
    debugLifetimes("Inside loop of parameters");
    Lifetime l;
    const clang::Expr* lifetime_name = nullptr;
    if (i < lifetime_names.size()) {
      lifetime_name = lifetime_names[i];
    }
    if (llvm::Error err = lifetime_factory(lifetime_name).moveInto(l)) {
      return std::move(err);
    }
  }

  debugLifetimes("After for");

  // TODO what?
  return Lifetime();
}


LifetimeFactory FunctionLifetimeFactory::ParamLifetimeFactory() const {
  return [this](const clang::Expr* name) -> llvm::Expected<Lifetime> {
    debugLifetimes(">> Expression name:");
    name->dump();
    if (name) {
      debugLifetimes(">>> The lifetime has a name");
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

    return Lifetime();
  };
}

LifetimeFactory FunctionLifetimeFactory::ReturnLifetimeFactory() const {
  return [this](const clang::Expr* name/*,
                  &input_lifetime */) -> llvm::Expected<Lifetime> {
    if (name) {
      Lifetime lifetime;
      if (llvm::Error err = LifetimeFromName(name).moveInto(lifetime)) {
        return std::move(err);
      }
      return lifetime;
    }

    if (!elision_enabled) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Lifetime elision not enabled for function"
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
      );
    }

    // TODO uncomment this
    // If we have a single input lifetime, its lifetime is assigned to
    // all output lifetimes.
    // if (input_lifetime.has_value()) {
    //   return *input_lifetime;
    // } else {
    //   // Otherwise, we don't know how to elide the output lifetime.
    //   return llvm::createStringError(
    //       llvm::inconvertibleErrorCode(),
    //       absl::StrCat("Cannot elide output lifetimes for '",
    //                    func->getNameAsString(),
    //                    "' because it is a non-member function that "
    //                    "does not have "
    //                    "exactly one input lifetime"));
    // }
    // });
  // TODO remove this
  return Lifetime();
  };
}

llvm::Expected<FunctionLifetimes> FunctionLifetimes::CreateForDecl(
    const clang::FunctionDecl* func,
    const FunctionLifetimeFactory& lifetime_factory) {
  // + represents a type with its qualifiers: const, volatile, restrict, etc
  // + the clang AST contains QualTypes to describe the types of variables,
  // + expressions, etc
  clang::QualType this_type;
  if (auto method = clang::dyn_cast<clang::CXXMethodDecl>(func);
      method && !method->isStatic()) {
    this_type = method->getThisType();
  }
  clang::TypeLoc type_loc;
  if (func->getTypeSourceInfo()) {
    type_loc = func->getTypeSourceInfo()->getTypeLoc();
  }
  return Create(func->getType()->getAs<clang::FunctionProtoType>(), type_loc,
                this_type, lifetime_factory);
}

llvm::Expected<FunctionLifetimes> FunctionLifetimes::Create(
    const clang::FunctionProtoType* type, clang::TypeLoc type_loc,
    const clang::QualType this_type,
    const FunctionLifetimeFactory& lifetime_factory) {
  FunctionLifetimes ret;

  // llvm::SmallVector<const clang::Attr*> attrs;
  if (type_loc) {
    // debugInfo("Has type_loc");
    // debugLifetimes("type_loc is defined as: ",
    //                type_loc.getType().getAsString());
    // debugLifetimes("and the attributes are:");
    // StripAttributes(type_loc, attrs);
    // for (const auto& attr : attrs) {  // DEBUG
    //   std::cout << attr << '\n';
    // }
  }

  // TODO remove this
  if (!this_type.isNull()) debugWarn("Has this type");

  // llvm::SmallVector<const clang::Expr*> lifetime_names;
  // if (llvm::Error err =
  // GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
  //   return std::move(err);
  // }

  // debugLifetimes("lifetime names");
  // debugLifetimes("Is lifetime names empty?",
  // std::to_string(lifetime_names.empty())); for (const auto& name :
  // lifetime_names) {  // DEBUG
  //   name->dump();
  // }

  // TODO this
  // if (this_type.isNull() && !lifetime_names.empty()) {
  //     return llvm::createStringError(
  //         llvm::inconvertibleErrorCode(),
  //         "Encountered a `this` lifetime on a function with no `this`
  //         parameter"
  //         /* absl::StrCat("Encountered a `this` lifetime on a function with
  //         no "
  //                      "`this` parameter") */);
  //   }

  // TODO this
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

  // ** Get parameters lifetimes
  clang::FunctionTypeLoc func_type_loc;
  if (type_loc) {
    func_type_loc = type_loc.getAsAdjusted<clang::FunctionTypeLoc>();
  }

  // TODO implement for Lifetime
  // ret.param_lifetimes_.reserve(type->getNumParams());
  // DEBUG
  // debugLifetimes("Num of parameters", std::to_string(type->getNumParams()));

  for (size_t i = 0; i < type->getNumParams(); i++) {
    clang::TypeLoc param_type_loc;
    if (type_loc) {
      const clang::ParmVarDecl* param = func_type_loc.getParam(i);
      // TODO remove this
      // const clang::Decl* paramDecl = func_type_loc.getParam(i);
      // debug
      // debugLifetimes("Parameter", i);
      // param->dump();
      // debugLifetimes("Parameter name", param->getNameAsString());
      // debugLifetimes("Parameter id", param->getID());
      if (param && param->getTypeSourceInfo()) {
        param_type_loc = param->getTypeSourceInfo()->getTypeLoc();
      }
      Lifetime tmp;
      if (llvm::Error err = lifetime_factory
                                .CreateParamLifetimesNew(type->getParamType(i),
                                                         param_type_loc)
                                .moveInto(tmp)) {
        return std::move(err);
      }
      ret.variable_lifetimes[param] = tmp;
    }
  }

  clang::TypeLoc return_type_loc;
  if (func_type_loc) {
    return_type_loc = func_type_loc.getReturnLoc();
  }

  if (llvm::Error err =
          lifetime_factory
              .CreateReturnLifetimes(
                  type->getReturnType(), return_type_loc, ret.variable_lifetimes
                  /*, ret.this_lifetimes_ */)
              .moveInto(ret.return_lifetime_)) {
    return std::move(err);
  }

  return ret;
}

}  // namespace clang