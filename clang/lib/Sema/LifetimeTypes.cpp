#include "LifetimeTypes.h"

#include "clang/AST/Attr.h"
#include "clang/AST/Attrs.inc"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"

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

ValueLifetimes::ValueLifetimes(clang::QualType type) : type_(type) {}
ValueLifetimes::ValueLifetimes(const ValueLifetimes& other) { *this = other; }
ValueLifetimes::~ValueLifetimes() = default;

void /* llvm::Expected<ValueLifetimes> */ ValueLifetimes::Create(
    clang::QualType type, clang::TypeLoc type_loc,
    LifetimeFactory lifetime_factory) {
  assert(!type.isNull());
  if (type_loc) {
    assert(SameType(type_loc.getType(), type));
  }

  type = type.IgnoreParens();

  type = StripAttributes(type);
  llvm::SmallVector<const clang::Attr*> attrs;
  if (!type_loc.isNull()) {
    type_loc = StripAttributes(type_loc, attrs);
  }

  llvm::SmallVector<const clang::Expr*> lifetime_names;
  if (llvm::Error err = GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
    // TODO uncomment
    // return std::move(err);
  }

  ValueLifetimes ret(type);

  llvm::SmallVector<std::string> lifetime_params = GetLifetimeParameters(type);
  if (!lifetime_params.empty() && !lifetime_names.empty() &&
      lifetime_names.size() != lifetime_params.size()) {
    // TODO abseil
    // TODO uncomment
    /* return llvm::createStringError(
        llvm::inconvertibleErrorCode(),
        absl::StrCat("Type has ", lifetime_params.size(),
                     " lifetime parameters but ", lifetime_names.size(),
                     " lifetime arguments were given")); */
  }
}

// ValueLifetimes& ValueLifetimes::operator=(const ValueLifetimes& other) {
//   type_ = other.type_;
//   template_argument_lifetimes_ = other.template_argument_lifetimes_;
//   lifetime_parameters_by_name_ = other.lifetime_parameters_by_name_;
//   auto pointee_lifetimes =
//       other.pointee_lifetimes_
//           ? std::make_unique<ObjectLifetimes>(*other.pointee_lifetimes_)
//           : nullptr;
//   auto function_lifetimes =
//       other.function_lifetimes_
//           ? std::make_unique<FunctionLifetimes>(*other.function_lifetimes_)
//           : nullptr;
//   // Note: because ValueLifetimes is a recursive type (pointee_lifetimes_
//   // contains a ValueLifetimes), the following line can destroy `other`.
//   // (Thus the temporary local variables before we perform the assignment.)
//   pointee_lifetimes_ = std::move(pointee_lifetimes);
//   function_lifetimes_ = std::move(function_lifetimes);
//   return *this;
// }

}  // namespace clang