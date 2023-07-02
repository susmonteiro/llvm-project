#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis() {}
LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis(
    FunctionLifetimes &function_info) {
  const auto &params_lifetimes = function_info.GetParamsLifetimes();
  for (auto &pair : params_lifetimes) {
    VariableLifetimes.insert({pair.first, pair.second});
  }
  ReturnLifetime = function_info.GetReturnLifetime();
}

VariableLifetimesVector &LifetimeAnnotationsAnalysis::GetVariableLifetimes() {
  return VariableLifetimes;
}

ObjectLifetimes &LifetimeAnnotationsAnalysis::GetObjectLifetimes(
    const clang::VarDecl *var_decl) {
  return VariableLifetimes[var_decl];
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetime(
    const clang::VarDecl *var_decl, clang::QualType type) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    // TODO error
    CreateVariable(var_decl, Lifetime(type));
  }
  return VariableLifetimes[var_decl].GetLifetime(type);
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetimeOrLocal(
    const clang::VarDecl *var_decl, clang::QualType type) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    CreateVariable(var_decl, Lifetime(LOCAL, type));
  }
  return VariableLifetimes[var_decl].GetLifetimeOrLocal(type);
}

Lifetime &LifetimeAnnotationsAnalysis::GetReturnLifetime(
    clang::QualType &type) {
  return ReturnLifetime.GetLifetime(type);
}

bool LifetimeAnnotationsAnalysis::IsLifetimeNotset(
    const clang::VarDecl *var_decl, clang::QualType &type) const {
  auto it = VariableLifetimes.find(var_decl);
  if (it != VariableLifetimes.end()) {
    ObjectLifetimes ol = it->second;
    return ol.GetLifetime(type).IsNotSet();
  } else {
    // TODO error?
    return false;
  }
}

void LifetimeAnnotationsAnalysis::CreateDependency(const clang::VarDecl *from,
                                                   clang::QualType from_type,
                                                   const clang::VarDecl *to,
                                                   clang::QualType to_type,
                                                   const clang::Stmt *loc) {
  if (IsLifetimeNotset(from, from_type)) {
    CreateLifetimeDependency(from, from_type, loc, to_type);
    CreateStmtDependency(loc, to);
  }

  if ((!from_type->isPointerType() && !from_type->isReferenceType()) ||
      (!to_type->isPointerType() && !to_type->isReferenceType())) {
    return;
  }

  from_type = from_type->getPointeeType();
  to_type = to_type->getPointeeType();

  while ((from_type->isPointerType() || from_type->isReferenceType()) &&
         (to_type->isPointerType() || to_type->isReferenceType())) {
    if (IsLifetimeNotset(from, from_type)) {
      CreateLifetimeDependency(from, from_type, loc, to_type);
      CreateStmtDependency(loc, to);
    }
    if (IsLifetimeNotset(to, from_type)) {
      // TODO check if it should be the other way around
      CreateLifetimeDependency(to, to_type, loc, from_type);
      CreateStmtDependency(loc, from);
    }
    from_type = from_type->getPointeeType();
    to_type = to_type->getPointeeType();
  }
}

VarStmtDependenciesMap &LifetimeAnnotationsAnalysis::GetLifetimeDependencies() {
  return LifetimeDependencies;
}

StmtVarDependenciesMap &LifetimeAnnotationsAnalysis::GetStmtDependencies() {
  return StmtDependencies;
}

void LifetimeAnnotationsAnalysis::CreateLifetimeDependency(
    const clang::VarDecl *from, clang::QualType from_type,
    const clang::Stmt *to, clang::QualType to_type) {
  LifetimeDependencies[LHSTypeStruct{from, from_type}].insert(
      RHSTypeStruct{to, to_type});
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::VarDecl *to) {
  StmtDependencies[from].insert(to);
}

llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<LHSTypeStruct>>
LifetimeAnnotationsAnalysis::TransposeDependencies() {
  llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<LHSTypeStruct>> result;
  for (const auto &info_lhs : LifetimeDependencies) {
    clang::QualType lhs_type = info_lhs.first.type;
    for (const auto &info_rhs : info_lhs.second) {
      clang::QualType rhs_type = info_rhs.type;
      for (const auto &child : StmtDependencies[info_rhs.stmt]) {
        if (IsLifetimeNotset(child, lhs_type) &&
            info_lhs.first.var_decl != child)
          result[LHSTypeStruct{child, rhs_type}].insert(info_lhs.first);
      }
    }
  }
  return result;
}

std::vector<LHSTypeStruct> LifetimeAnnotationsAnalysis::InitializeWorklist()
    const {
  std::vector<LHSTypeStruct> worklist;
  for (const auto &pair : LifetimeDependencies) {
    worklist.emplace_back(pair.first);
  }
  return worklist;
}

void LifetimeAnnotationsAnalysis::ProcessPossibleLifetimes() {
  // iterate over variables with no fixed lifetime
  for (auto &pair : VariableLifetimes) {
    auto &lifetimes = pair.second.GetLifetimes();
    for (auto &objectLifetime : lifetimes) {
      Lifetime &lifetime = objectLifetime;
      if (lifetime.IsNotSet()) {
        lifetime.ProcessPossibleLifetimes();
      }
    }
  }
}

std::string LifetimeAnnotationsAnalysis::DebugString() {
  std::string str = "[LifetimeAnnotationsAnalysis] - STATE\n\n";
  str += ">> VariableLifetimes\n\n";
  for (const auto &pair : VariableLifetimes) {
    if (pair.first == nullptr) continue;
    str +=
        pair.first->getNameAsString() + ": " + pair.second.DebugString() + '\n';
  }
  str += "\n>> Dependencies\n\n";
  for (const auto &info_lhs : LifetimeDependencies) {
    str += info_lhs.first.var_decl->getNameAsString() + ": ";
    str += "[type] " + info_lhs.first.type.getAsString() + "\t\t[vars] ";
    for (const auto &info_rhs : info_lhs.second) {
      for (const auto &var : StmtDependencies[info_rhs.stmt]) {
        if (info_lhs.first.var_decl == var) continue;
        str += info_rhs.type.getAsString() + ' ' +
               var->getNameAsString() + ' ';
      }
    }
    str += '\n';
  }
  return str;
}

std::string LifetimeAnnotationsAnalysis::WorklistDebugString(
    std::vector<LHSTypeStruct> &worklist) {
  std::string res;
  for (const auto &el : worklist) {
    res += '{' + el.type.getAsString() + ' ' +
           el.var_decl->getNameAsString() + "}, ";
  }
  res += '\n';
  return res;
}

}  // namespace clang
