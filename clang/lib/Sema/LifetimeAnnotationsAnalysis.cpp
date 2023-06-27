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
    // TODO error
    // DEBUG
    // debugWarn("Lifetime not found in VariableLifetimes");
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
                                                   const clang::Stmt *loc) {
  // TODO implement
  // clang::QualType type = to->getType().getCanonicalType();
  // // TODO necessary?
  // if (type->isArrayType()) {
  //   type = type->castAsArrayTypeUnsafe()->getElementType();
  // }

  // if (type->isRecordType()) {
  //   // TODO implement
  //   return;
  // }

  // if (type->isPointerType() || type->isReferenceType() ||
  //     type->isStructureOrClassType()) {
  //   // TODO implement
  // }
  // if (from != to) { }

  // TODO

  if (IsLifetimeNotset(from, from_type)) {
    CreateLifetimeDependency(from, from_type, loc);
    CreateStmtDependency(loc, to);
  }

  from_type = from_type->getPointeeType();

  while (from_type->isPointerType() || from_type->isReferenceType()) {
    if (IsLifetimeNotset(from, from_type)) {
      CreateLifetimeDependency(from, from_type, loc);
      CreateStmtDependency(loc, to);
    }
    if (IsLifetimeNotset(to, from_type)) {
      CreateLifetimeDependency(to, from_type, loc);
      CreateStmtDependency(loc, from);
    }
    from_type = from_type->getPointeeType();
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
    const clang::Stmt *to) {
  LifetimeDependencies[VarTypeStruct{from, from_type}].insert(to);
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::VarDecl *to) {
  StmtDependencies[from].insert(to);
}

llvm::DenseMap<VarTypeStruct, llvm::DenseSet<VarTypeStruct>>
LifetimeAnnotationsAnalysis::TransposeDependencies() {
  llvm::DenseMap<VarTypeStruct, llvm::DenseSet<VarTypeStruct>>
      result;
  for (const auto &info : LifetimeDependencies) {
    clang::QualType lhs_type = info.first.lhs_type;
    for (const auto &stmt : info.second) {
      for (const auto &child : StmtDependencies[stmt]) {
        if (IsLifetimeNotset(child, lhs_type) && info.first.var_decl != child)
          result[VarTypeStruct{child, lhs_type}].insert(info.first);
      }
    }
  }
  return result;
}

std::vector<VarTypeStruct> LifetimeAnnotationsAnalysis::InitializeWorklist()
    const {
  std::vector<VarTypeStruct> worklist;
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
  for (const auto &pair : LifetimeDependencies) {
    str += pair.first.var_decl->getNameAsString() + ": ";
    str += "[type] " + pair.first.lhs_type.getAsString() + "\t\t[vars] ";
    for (const auto &stmt : pair.second) {
      for (const auto &var : StmtDependencies[stmt]) {
        if (pair.first.var_decl == var) continue;
        str += var->getNameAsString() + ' ';
      }
    }
    str += '\n';
  }
  return str;
}

}  // namespace clang
