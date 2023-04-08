#include "FunctionLifetimes.h"

#include <iostream>

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

// TODO return
// std::optional<Lifetime> FunctionLifetimeFactory::GetSingleInputLifetime(
//     const llvm::SmallVector<ValueLifetimes>& param_lifetimes,
//     const std::optional<ValueLifetimes>& this_lifetimes) const {
//   // If we have an implicit `this` parameter, its lifetime is assigned to
//   // all lifetimes in the return type.
//   // TODO this
//   // if (this_lifetimes.has_value()) {
//   //   return this_lifetimes->GetPointeeLifetimes().GetLifetime();
//   // }

//   llvm::DenseSet<Lifetime> all_input_lifetimes;
//   for (const ValueLifetimes& v : param_lifetimes) {
//     v.Traverse([&all_input_lifetimes](Lifetime l, Variance) {
//       all_input_lifetimes.insert(l);
//     });
//   }

//   if (all_input_lifetimes.size() == 1) {
//     // If we have a single input lifetime, its lifetime is assigned to all
//     // output lifetimes.
//     return *all_input_lifetimes.begin();
//   } else {
//     // Otherwise, we don't know how to elide the output lifetime.
//     return std::nullopt;
//   }
// }

llvm::Expected<ValueLifetimes> FunctionLifetimeFactory::CreateParamLifetimes(
    clang::QualType param_type, clang::TypeLoc param_type_loc) const {
      debugLifetimes("Inside create param lifetimes");
  return ValueLifetimes::Create(param_type, param_type_loc,
                                ParamLifetimeFactory());
}

// TODO return
// llvm::Expected<ValueLifetimes>
// FunctionLifetimeFactory::CreateReturnLifetimes(
//         clang::QualType return_type, clang::TypeLoc return_type_loc,
//         const llvm::SmallVector<ValueLifetimes> &param_lifetimes,
//         const std::optional<ValueLifetimes> &this_lifetimes) const {

//       // std::optional<Lifetime> input_lifetime =
//       //     GetSingleInputLifetime(param_lifetimes, this_lifetimes);

//       return ValueLifetimes::Create(
//           return_type, return_type_loc,
//           [&input_lifetime,
//            this](const clang::Expr *name) -> llvm::Expected<Lifetime> {
//             if (name) {
//               Lifetime lifetime;
//               if (llvm::Error err =
//               LifetimeFromName(name).moveInto(lifetime)) {
//                 return std::move(err);
//               }
//               return lifetime;
//             }

//             if (!elision_enabled) {
//               return llvm::createStringError(
//                   llvm::inconvertibleErrorCode(),
//                   // TODO abseil
//                   /* absl::StrCat("Lifetime elision not enabled for '",
//                                func->getNameAsString(), "'") */
//                   "Lifetime elision not enabled for function");
//             }

//             // If we have a single input lifetime, its lifetime is assigned
//             to
//             // all output lifetimes.
//             if (input_lifetime.has_value()) {
//               return *input_lifetime;
//             } else {
//               // Otherwise, we don't know how to elide the output lifetime.
//               return llvm::createStringError(
//                   llvm::inconvertibleErrorCode(),
//                   // TODO abseil
//                   /* absl::StrCat("Cannot elide output lifetimes for '",
//                                func->getNameAsString(),
//                                "' because it is a non-member function that "
//                                "does not have "
//                                "exactly one input lifetime") */
//                   "Cannot elide output lifetimes for function...");
//             }
//           });
//     }

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

llvm::Expected<FunctionLifetimes> FunctionLifetimes::CreateForDecl(
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
  return Create(func->getType()->getAs<clang::FunctionProtoType>(),
                       type_loc, this_type, lifetime_factory);
}

llvm::Expected<FunctionLifetimes> FunctionLifetimes::Create(
    const clang::FunctionProtoType* type, clang::TypeLoc type_loc,
    const clang::QualType this_type,
    const FunctionLifetimeFactory& lifetime_factory) {
  FunctionLifetimes ret;

  // TODO this
  // llvm::SmallVector<const clang::Attr*> attrs;
  // if (type_loc) {
  //   debugLifetimes("type_loc is defined as: ", type_loc.getType().getAsString());
  //   debugLifetimes("and the attributes are:");
  //   StripAttributes(type_loc, attrs);
  //   for (const auto& attr : attrs) {  // DEBUG
  //     std::cout << attr << '\n';
  //   }
  // }

  // TODO this
  // llvm::SmallVector<const clang::Expr*> lifetime_names;
  // if (llvm::Error err = GetAttributeLifetimes(attrs).moveInto(lifetime_names)) {
  //   return std::move(err);
  // }

  // TODO this
  // debugLifetimes("lifetime names");
  // for (const auto& name : lifetime_names) {  // DEBUG
  //   name->dump();
  // }
  
  // TODO this
  /* if (this_type.isNull() && !lifetime_names.empty()) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          absl::StrCat("Encountered a `this` lifetime on a function with no "
                       "`this` parameter"));
    } */

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

  ret.param_lifetimes_.reserve(type->getNumParams());
  debugLifetimes("Num of parameters", std::to_string(type->getNumParams()));

  for (size_t i = 0; i < type->getNumParams(); i++) {
    debugLifetimes("Inside loop");
    clang::TypeLoc param_type_loc;
    if (type_loc) {
      const clang::ParmVarDecl* param = func_type_loc.getParam(i);
      debugLifetimes("Get param");
      param->dump();
      if (param && param->getTypeSourceInfo()) {
        debugLifetimes("Param has type source info");
        param_type_loc = param->getTypeSourceInfo()->getTypeLoc();
      }
    }
    ValueLifetimes tmp;
    debugLifetimes("Before create param lifetimes");
    if (llvm::Error err =
            lifetime_factory
                .CreateParamLifetimes(type->getParamType(i), param_type_loc)
                .moveInto(tmp)) {
      return std::move(err);
    }
    ret.param_lifetimes_.push_back(std::move(tmp));
  }

  debugLifetimes("After loop");
  debugLifetimes("FunctionLifetimes ret should have the parameters lifetimes stored now");
  // TODO check above

  // TODO return
  // clang::TypeLoc return_type_loc;
  // if (func_type_loc) {
  //   return_type_loc = func_type_loc.getReturnLoc();
  // }

  // TODO return
  // if (llvm::Error err =
  //         lifetime_factory
  //             .CreateReturnLifetimes(type->getReturnType(), return_type_loc,
  //                                    ret.param_lifetimes_, ret.this_lifetimes_)
  //             .moveInto(ret.return_lifetimes_)) {
  //   // return std::move(err);
  // }

    return ret;
}

}  // namespace clang