#ifndef LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
#define LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_

#include <iostream>

#include "clang/Sema/Lifetime.h"
#include "clang/Sema/ObjectLifetimes.h"
#include "clang/Sema/PointeeType.h"
// DEBUG
#include "clang/Sema/DebugLifetimes.h"

namespace clang {

struct ParamInfo {
  clang::QualType type;
  const clang::ParmVarDecl *param;
  unsigned int ptr_idx;  // includes only pointers/references
  unsigned int glb_idx;  // includes all args
  unsigned int original_num_indirections;
};

using LifetimeFactory = std::function<llvm::Expected<Lifetime>(
    const clang::Expr *, clang::QualType type)>;

using ParamsLifetimesMap =
    llvm::DenseMap<const clang::ParmVarDecl *, ObjectLifetimes>;

// for each level of indirections, store the params with that level of
// indirection idx = 0, int*; idx = 1, int**; etc
using ParamsInfoVector = llvm::SmallVector<llvm::SmallVector<ParamInfo>>;

class FunctionLifetimeFactory {
 public:
  FunctionLifetimeFactory() {}
  FunctionLifetimeFactory(
      /* bool elision_enabled, */ const clang::FunctionDecl *func)
      : /* elision_enabled(elision_enabled), */ Func(func) {}

  llvm::Expected<ObjectLifetimes> CreateParamLifetimes(
      clang::QualType param_type, clang::TypeLoc param_type_loc) const;

  llvm::Expected<ObjectLifetimes> CreateReturnLifetimes(
      clang::QualType return_type, clang::TypeLoc return_type_loc) const;

  llvm::Expected<ObjectLifetimes> CreateVarLifetimes(
      clang::QualType var_type, clang::TypeLoc var_type_loc) const;

  static llvm::Expected<ObjectLifetimes> CreateLifetime(
      clang::QualType type, clang::TypeLoc type_loc,
      LifetimeFactory lifetime_factory);

  llvm::Expected<Lifetime> LifetimeFromName(const clang::Expr *name,
                                            clang::QualType type) const;

  LifetimeFactory ParamLifetimeFactory() const;
  LifetimeFactory ReturnLifetimeFactory() const;
  LifetimeFactory VarLifetimeFactory() const;

 private:
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

  std::vector<const clang::ParmVarDecl *> GetParamsInOrder() const {
    return Params;
  }

  ParamsInfoVector GetParamsInfo() const { return ParamsInfo; }

  const clang::ParmVarDecl *GetParam(unsigned int i) const { return Params[i]; }

  ParamsLifetimesMap &GetParamsLifetimes() { return ParamsLifetimes; }

  ObjectLifetimes &GetParamLifetime(const clang::ParmVarDecl *param) {
    auto it = ParamsLifetimes.find(param);
    assert(it != ParamsLifetimes.end());
    return it->second;
  }

  ObjectLifetimes &GetParamLifetime(unsigned int i) {
    const auto *param = Params[i];
    auto it = ParamsLifetimes.find(param);
    assert(it != ParamsLifetimes.end());
    return it->second;
  }

  Lifetime &GetParamLifetime(const clang::ParmVarDecl *param,
                             clang::QualType type) {
    type = type.getCanonicalType();
    auto it = ParamsLifetimes.find(param);
    assert(it != ParamsLifetimes.end());

    ObjectLifetimes object_lifetimes = it->second;
    return object_lifetimes.GetLifetimeOrLocal(type);
  }

  Lifetime &GetParamLifetime(const clang::ParmVarDecl *param,
                             unsigned int num_indirections) {
    auto it = ParamsLifetimes.find(param);
    assert(it != ParamsLifetimes.end());

    ObjectLifetimes object_lifetimes = it->second;
    return object_lifetimes.GetLifetimeOrLocal(num_indirections);
  }

  Lifetime &GetParamLifetime(unsigned int i, unsigned int num_indirections) {
    const auto *param = Params[i];
    auto it = ParamsLifetimes.find(param);
    assert(it != ParamsLifetimes.end());
    return it->second.GetLifetimeOrLocal(num_indirections);
  }

  ObjectLifetimes &GetReturnLifetime() { return ReturnLifetime; }
  Lifetime &GetReturnLifetime(clang::QualType &type) {
    return ReturnLifetime.GetLifetime(type);
  }

  bool IsReturnLifetimeLocal() {
    ObjectLifetimes return_ol = GetReturnLifetime();
    return return_ol.HasLifetimeLocal();
  }

  void InsertParamLifetime(const clang::ParmVarDecl *param) {
    AllParams.emplace_back(param);
  }

  void InsertParamLifetime(const clang::ParmVarDecl *param,
                           ObjectLifetimes &objectsLifetimes) {
    AllParams.emplace_back(param);
    Params.emplace_back(param);
    ParamsLifetimes[param] = objectsLifetimes;
  }

  void ProcessParams();

  std::string DebugParams();
  std::string DebugParamsByType();
  std::string DebugReturn();
  std::string DebugString();

  static llvm::Expected<FunctionLifetimes> CreateForDecl(
      const clang::FunctionDecl *function,
      const FunctionLifetimeFactory &lifetime_factory);

 private:
  // stores param lifetimes in order
  std::vector<const clang::ParmVarDecl *> AllParams;
  std::vector<const clang::ParmVarDecl *> Params;
  ParamsInfoVector ParamsInfo;
  ParamsLifetimesMap ParamsLifetimes;
  ObjectLifetimes ReturnLifetime;
  int FuncId;

  static llvm::Expected<FunctionLifetimes> Create(
      const clang::FunctionDecl *func, clang::TypeLoc type_loc,
      const clang::QualType this_type,
      const FunctionLifetimeFactory &lifetime_factory);
};

}  // namespace clang

#endif  // LIFETIME_ANNOTATIONS_FUNCTION_LIFETIMES_H_
