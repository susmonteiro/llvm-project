#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include <iostream>

#include "clang/Sema/Lifetime.h"
#include "clang/Sema/PointeeType.h"
#include "llvm/ADT/SmallVector.h"
// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

using LifetimeFactory =
    std::function<llvm::Expected<Lifetime>(const clang::Expr *)>;

using ParamsLifetimesMap = llvm::DenseMap<const clang::ParmVarDecl *, Lifetime>;

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func)
      : /* elision_enabled(elision_enabled), */ Func(func) {}

  llvm::Expected<Lifetime> CreateParamLifetimes(
      clang::QualType param_type, clang::TypeLoc param_type_loc) const;

  llvm::Expected<Lifetime> CreateReturnLifetimes(
      clang::QualType return_type, clang::TypeLoc return_type_loc) const;

  llvm::Expected<Lifetime> CreateVarLifetimes(
      clang::QualType var_type, clang::TypeLoc var_type_loc) const;

  static llvm::Expected<Lifetime> CreateLifetime(
      clang::QualType type, clang::TypeLoc type_loc,
      LifetimeFactory lifetime_factory);

  llvm::Expected<Lifetime> LifetimeFromName(const clang::Expr *name) const;

  LifetimeFactory ParamLifetimeFactory() const;
  LifetimeFactory ReturnLifetimeFactory() const;
  LifetimeFactory VarLifetimeFactory() const;

 private:
  // TODO elision
  // bool elision_enabled;
  const clang::FunctionDecl *Func;
};

// Holds information about each function
class FunctionLifetimes {
 public:
  FunctionLifetimes();
  FunctionLifetimes(int func_id) : FuncId(func_id) {}

  int Id() { return FuncId; }

  // Returns the number of function parameters (excluding the implicit `this).
  size_t GetNumParams() const { return Params.size(); }

  std::vector<const clang::ParmVarDecl*> GetParamsInOrder() const { return Params; }
  const clang::ParmVarDecl *GetParam(unsigned int i) const { return Params[i]; }

  ParamsLifetimesMap GetParamsLifetimes() const { return ParamsLifetimes; }

  std::optional<Lifetime> GetParamLifetime(const clang::ParmVarDecl *param) const { 
    auto it = ParamsLifetimes.find(param);
    return it != ParamsLifetimes.end() ? std::optional<Lifetime>(it->second) : std::nullopt;
  }

  Lifetime GetReturnLifetime() const { return ReturnLifetime; }
  void SetReturnLifetime(Lifetime l) { ReturnLifetime = l; }

  void InsertParamLifetime(const clang::ParmVarDecl *param, Lifetime l) {
    Params.emplace_back(param);
    ParamsLifetimes[param] = l;
  }

  std::string DebugParams() const;
  std::string DebugReturn() const;
  std::string DebugString() const;

  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  // ? is it better to have optionals or a Lifetime that basically means no
  // lifetime

  // stores param lifetimes in order
  std::vector<const clang::ParmVarDecl*> Params;
  ParamsLifetimesMap ParamsLifetimes;
  Lifetime ReturnLifetime;
  int FuncId;

  // TODO this
  // std::optional<ValueLifetimes> this_lifetimes_;

  static llvm::Expected<FunctionLifetimes> Create(
      const clang::FunctionDecl *func, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
