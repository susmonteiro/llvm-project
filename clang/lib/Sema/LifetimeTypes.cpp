#include "LifetimeTypes.h"

namespace clang {

ValueLifetimes::ValueLifetimes(clang::QualType type) : type_(type) {}
ValueLifetimes::ValueLifetimes(const ValueLifetimes& other) { *this = other; }
ValueLifetimes::~ValueLifetimes() = default;

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

}