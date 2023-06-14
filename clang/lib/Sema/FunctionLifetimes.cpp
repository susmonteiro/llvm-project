#include "clang/Sema/FunctionLifetimes.h"

#include "clang/AST/Attr.h"
#include "llvm/Support/Error.h"

namespace clang {

constexpr int INVALID = -1;

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
    // DEBUG
    // debugInfo("Not a record");
    return {};
  }

  auto cxx_record = record->getAsCXXRecordDecl();
  if (!cxx_record) {
    // DEBUG
    // debugInfo("Not a cxxrecord");
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

// === FunctionLifetimeFactory ===

llvm::Expected<Lifetime> FunctionLifetimeFactory::LifetimeFromName(
    const clang::Expr* name, clang::QualType type) const {
  clang::QualType canonical_type = type.getCanonicalType();
  llvm::StringRef name_str;
  if (llvm::Error err = EvaluateAsStringLiteral(name, Func->getASTContext())
                            .moveInto(name_str)) {
    return std::move(err);
  }
  return Lifetime(name_str, canonical_type);
}

llvm::Expected<ObjectLifetimes> FunctionLifetimeFactory::CreateParamLifetimes(
    clang::QualType param_type, clang::TypeLoc param_type_loc) const {
  return CreateLifetime(param_type, param_type_loc, ParamLifetimeFactory());
}

llvm::Expected<ObjectLifetimes> FunctionLifetimeFactory::CreateReturnLifetimes(
    clang::QualType return_type, clang::TypeLoc return_type_loc) const {
  return CreateLifetime(return_type, return_type_loc, ReturnLifetimeFactory());
}

llvm::Expected<ObjectLifetimes> FunctionLifetimeFactory::CreateVarLifetimes(
    clang::QualType var_type, clang::TypeLoc var_type_loc) const {
  return CreateLifetime(var_type, var_type_loc, VarLifetimeFactory());
}

llvm::Expected<ObjectLifetimes> FunctionLifetimeFactory::CreateLifetime(
    clang::QualType qualtype, clang::TypeLoc type_loc,
    LifetimeFactory lifetime_factory) {
  assert(!qualtype.isNull());
  if (type_loc) {
    assert(SameType(type_loc.getType(), qualtype));
  }

  qualtype = qualtype.IgnoreParens();
  qualtype = StripAttributes(qualtype);
  clang::QualType canonical_type = qualtype.getCanonicalType();

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

  llvm::SmallVector<std::string> lifetime_params =
      GetLifetimeParameters(qualtype);

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

  // FIXME cannot remove this line
  // debugLifetimes("Size of lifetime_params", lifetime_params.size());

  for (size_t i = 0; i < lifetime_params.size(); ++i) {
    Lifetime l(canonical_type);
    const clang::Expr* lifetime_name = nullptr;
    if (i < lifetime_names.size()) {
      lifetime_name = lifetime_names[i];
    }
    if (llvm::Error err =
            lifetime_factory(lifetime_name, canonical_type).moveInto(l)) {
      return std::move(err);
    }
  }

  // TODO this is already done in the previous function
  clang::QualType pointee = PointeeType(qualtype);
  // TODO change this
  if (pointee.isNull()) return ObjectLifetimes();

  clang::TypeLoc pointee_type_loc;
  if (type_loc) {
    pointee_type_loc = PointeeTypeLoc(type_loc);
    // Note: We can't assert that `pointee_type_loc` is non-null here. If
    // `type_loc` is a `TypedefTypeLoc`, then there will be no `TypeLoc` for
    // the pointee type because the pointee type never got spelled out at the
    // location of the original `TypeLoc`.
  }

  // TODO is this ok?
  // recursive call
  ObjectLifetimes retObjectLifetimes;
  if (llvm::Error err =
          CreateLifetime(pointee, pointee_type_loc, lifetime_factory)
              .moveInto(retObjectLifetimes)) {
    return std::move(err);
  }

  // debugLifetimes("Created the ObjectLifetime with type " +
  // pointee.getAsString() + " and with lifetime " +
  // retObjectLifetimes.GetLifetime().DebugString());

  const clang::Expr* lifetime_name = nullptr;
  if (!lifetime_names.empty()) {
    if (lifetime_names.size() != 1) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          // TODO abseil
          "Expected a single lifetime but multiple were given"
          /* absl::StrCat("Expected a single lifetime but ", lifetime_names.size(),
                       " were given") */);
    }
    lifetime_name = lifetime_names.front();
  }

  Lifetime lifetime(canonical_type);
  if (llvm::Error err =
          lifetime_factory(lifetime_name, canonical_type).moveInto(lifetime)) {
    return std::move(err);
  }

  retObjectLifetimes.InsertPointeeObject(lifetime);

  // debugLifetimes("The outer pointee has lifetime",
  // retObjectLifetimes.GetLifetime().DebugString());

  return retObjectLifetimes;
}

LifetimeFactory FunctionLifetimeFactory::ParamLifetimeFactory() const {
  return [this](const clang::Expr* name,
                clang::QualType type) -> llvm::Expected<Lifetime> {
    // clang::QualType expr_type = name->getType().getCanonicalType();
    // Lifetime lifetime(expr_type);
    clang::QualType canonical_type = type.getCanonicalType();
    Lifetime lifetime(canonical_type);
    if (name) {
      if (llvm::Error err =
              LifetimeFromName(name, canonical_type).moveInto(lifetime)) {
        return std::move(err);
      }
      return lifetime;
    }

    // As a special-case, lifetime is always inferred for the `this`
    // parameter for destructors. The obvious lifetime is definitionally
    // correct in this case: the object must be valid for the duration
    // of the call, or else the behavior is undefined. So we can infer
    // safely even if elision is disabled.
    if (/* !elision_enabled && */ Func->getDeclName().getNameKind() !=
        clang::DeclarationName::CXXDestructorName) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Lifetime elision not enabled for function"
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
      );
    }
    // TODO change me
    return lifetime;
  };
}

LifetimeFactory FunctionLifetimeFactory::ReturnLifetimeFactory() const {
  return [this](const clang::Expr* name, clang::QualType type
                /*,&input_lifetime */) -> llvm::Expected<Lifetime> {
    // TODO change me
    // clang::QualType expr_type = name->getType().getCanonicalType();
    // Lifetime lifetime(expr_type);
    clang::QualType canonical_type = type.getCanonicalType();
    Lifetime lifetime(canonical_type);
    if (name) {
      if (llvm::Error err =
              LifetimeFromName(name, canonical_type).moveInto(lifetime)) {
        return std::move(err);
      }
      return lifetime;
    }

    // TODO elision
    // if (!elision_enabled) {
    //   return llvm::createStringError(
    //       llvm::inconvertibleErrorCode(),
    //       "Lifetime elision not enabled for function"
    // TODO abseil
    // absl::StrCat("Lifetime elision not enabled for '",
    //              func->getNameAsString(), "'")
    // );
    // }

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
    return lifetime;
  };
}

LifetimeFactory FunctionLifetimeFactory::VarLifetimeFactory() const {
  return [this](const clang::Expr* name,
                clang::QualType type) -> llvm::Expected<Lifetime> {
    // TODO change me
    // clang::QualType expr_type = name->getType().getCanonicalType();
    // Lifetime lifetime(expr_type);
    clang::QualType canonical_type = type.getCanonicalType();
    Lifetime lifetime(canonical_type);
    if (name) {
      if (llvm::Error err =
              LifetimeFromName(name, canonical_type).moveInto(lifetime)) {
        return std::move(err);
      }
      return lifetime;
    }
    // TODO error?
    return lifetime;
  };
}

// === FunctionLifetimes ===

FunctionLifetimes::FunctionLifetimes() : FuncId(INVALID) {}

void FunctionLifetimes::ProcessParams() {
  unsigned int idx = 0;
  for (const auto& param : Params) {
    clang::QualType type = param->getType().getCanonicalType();
    unsigned int num_indirections = Lifetime::GetNumberIndirections(type);
    if (num_indirections > 0) {
      if (num_indirections >= ParamsByType.size()) {
        ParamsByType.resize(num_indirections);
      }
      ParamsByType[num_indirections - 1].emplace_back(std::pair(param, idx++));
    }
  }
  debugLifetimes(DebugParamsByType());
}

std::string FunctionLifetimes::DebugParams() {
  std::string res;
  res += "> Parameters Lifetimes:\n";
  int i = 0;
  for (const auto& pair : ParamsLifetimes) {
    res += "Parameter " + std::to_string(i) + " -> " +
           pair.second.DebugString() + '\n';
  }
  return res;
}

std::string FunctionLifetimes::DebugParamsByType() {
  std::string res;
  res += "> Parameters by Type:\n";
  if (ParamsByType.empty()) {
    res += "Empty\n";
    return res;
  }

  unsigned int num_indirections = 1;
  for (const auto& params : ParamsByType) {
    res += "[" + std::to_string(num_indirections++) + " indirections]: ";
    for (const auto& pair : params) {
      res += pair.first->getNameAsString() + ", ";
    }
    res += '\n';
  }
  return res + '\n';
}

std::string FunctionLifetimes::DebugReturn() {
  return "> Return Lifetime: " + ReturnLifetime.DebugString() + '\n';
}

std::string FunctionLifetimes::DebugString() {
  return "[FunctionLifetimes]\n" + DebugParams() + DebugReturn();
}

llvm::Expected<FunctionLifetimes> FunctionLifetimes::CreateForDecl(
    const clang::FunctionDecl* func,
    const FunctionLifetimeFactory& lifetime_factory) {
  clang::QualType this_type;
  if (auto method = clang::dyn_cast<clang::CXXMethodDecl>(func);
      method && !method->isStatic()) {
    this_type = method->getThisType();
  }
  clang::TypeLoc type_loc;
  if (func->getTypeSourceInfo()) {
    type_loc = func->getTypeSourceInfo()->getTypeLoc();
  }
  return Create(func, type_loc, this_type, lifetime_factory);
}

llvm::Expected<FunctionLifetimes> FunctionLifetimes::Create(
    const clang::FunctionDecl* func, clang::TypeLoc type_loc,
    const clang::QualType this_type,
    const FunctionLifetimeFactory& lifetime_factory) {
  const clang::FunctionProtoType* type =
      func->getType()->getAs<clang::FunctionProtoType>();
  FunctionLifetimes ret(func->getID());
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
      if (!param->getType()->isPointerType() &&
          !param->getType()->isReferenceType())
        continue;
      if (param && param->getTypeSourceInfo()) {
        param_type_loc = param->getTypeSourceInfo()->getTypeLoc();
      }

      ObjectLifetimes tmpObjectLifetimes;

      // TODO check if this covers everything that should have a lifetime
      clang::TypeLoc pointee_type_loc;
      if (param_type_loc) {
        pointee_type_loc = PointeeTypeLoc(type_loc);
        // Note: We can't assert that `pointee_type_loc` is non-null here. If
        // `type_loc` is a `TypedefTypeLoc`, then there will be no `TypeLoc` for
        // the pointee type because the pointee type never got spelled out at
        // the location of the original `TypeLoc`.
      }

      if (llvm::Error err =
              lifetime_factory
                  .CreateParamLifetimes(type->getParamType(i), param_type_loc)
                  .moveInto(tmpObjectLifetimes)) {
        return std::move(err);
      }

      debugLifetimes("Lifetimes of param " + param->getNameAsString() + ":\n" +
                     tmpObjectLifetimes.DebugString() + "\n");
      ret.InsertParamLifetime(param, tmpObjectLifetimes);
    }
  }

  clang::QualType return_type = type->getReturnType();
  clang::QualType return_pointee = PointeeType(return_type);
  // TODO check if this covers everything that should have a lifetime
  if (!return_pointee.isNull()) {
    clang::TypeLoc return_type_loc;
    if (func_type_loc) {
      return_type_loc = func_type_loc.getReturnLoc();
    }

    if (llvm::Error err =
            lifetime_factory.CreateReturnLifetimes(return_type, return_type_loc)
                .moveInto(ret.ReturnLifetime)) {
      return std::move(err);
    }
    // DEBUG
    debugLifetimes("Lifetimes of return value:\n" +
                   ret.ReturnLifetime.DebugString() + "\n");
  }

  return ret;
}

}  // namespace clang
