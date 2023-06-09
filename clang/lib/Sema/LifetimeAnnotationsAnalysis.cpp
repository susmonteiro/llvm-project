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

ObjectsLifetimes &LifetimeAnnotationsAnalysis::GetObjectsLifetimes(
    const clang::VarDecl *var_decl) {
  return VariableLifetimes[var_decl];
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetime(
    const clang::VarDecl *var_decl, clang::QualType type) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    // TODO error
    CreateVariable(var_decl, type);
  }
  return VariableLifetimes[var_decl].GetLifetime(type);
}

Lifetime &LifetimeAnnotationsAnalysis::GetReturnLifetime(
    clang::QualType &type) {
  return ReturnLifetime.GetLifetime(type);
}

bool LifetimeAnnotationsAnalysis::IsLifetimeNotset(
    const clang::VarDecl *var_decl, clang::QualType &type) const {
  auto it = VariableLifetimes.find(var_decl);
  if (it != VariableLifetimes.end()) {
    ObjectsLifetimes ol = std::move(it->second);
    return ol.GetLifetime(type).IsNotSet();
  } else {
    // TODO error?
    return false;
  }
}

bool LifetimeAnnotationsAnalysis::IsLifetimeNotset(
    const clang::VarDecl *var_decl) const {
  auto it = VariableLifetimes.find(var_decl);
  if (it != VariableLifetimes.end()) {
    ObjectsLifetimes ol = std::move(it->second);
    return ol.IsLifetimeNotSet();
  } else {
    // TODO error?
    return false;
  }
}

void LifetimeAnnotationsAnalysis::CreateDependency(const clang::VarDecl *from,
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
  // if (from != to) {
  // TODO
  CreateLifetimeDependency(from, loc);
  CreateStmtDependency(loc, to);
  // }
}

VarStmtDependenciesMap &LifetimeAnnotationsAnalysis::GetLifetimeDependencies() {
  return LifetimeDependencies;
}

StmtVarDependenciesMap &LifetimeAnnotationsAnalysis::GetStmtDependencies() {
  return StmtDependencies;
}

void LifetimeAnnotationsAnalysis::CreateLifetimeDependency(
    const clang::VarDecl *from, const clang::Stmt *to) {
  LifetimeDependencies[from].insert(to);
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::VarDecl *to) {
  StmtDependencies[from].insert(to);
}

llvm::DenseMap<const clang::VarDecl *, llvm::DenseSet<const clang::VarDecl *>>
LifetimeAnnotationsAnalysis::TransposeDependencies() {
  llvm::DenseMap<const clang::VarDecl *, llvm::DenseSet<const clang::VarDecl *>>
      result;
  for (const auto &pair : LifetimeDependencies) {
    for (const auto &stmt : pair.second) {
      for (const auto &child : StmtDependencies[stmt]) {
        if (IsLifetimeNotset(child)) result[child].insert(pair.first);
      }
    }
  }
  return result;
}

std::vector<const clang::VarDecl *>
LifetimeAnnotationsAnalysis::InitializeWorklist() const {
  std::vector<const clang::VarDecl *> worklist;
  for (const auto &pair : LifetimeDependencies) {
    worklist.emplace_back(pair.first);
  }
  return worklist;
}

void LifetimeAnnotationsAnalysis::ProcessShortestLifetimes() {
  // iterate over variables with no fixed lifetime
  for (const auto &pair : LifetimeDependencies) {
    auto &lifetimes = GetObjectsLifetimes(pair.first).GetLifetimes();
    for (auto &objectLifetime : lifetimes) {
      Lifetime &lifetime = objectLifetime;
      if (lifetime.IsNotSet()) {
        lifetime.ProcessShortestLifetimes();
      }
    }
  }
}

std::string LifetimeAnnotationsAnalysis::DebugString() {
  std::string str = "[LifetimeAnnotationsAnalysis] - STATE\n\n";
  str += ">> VariableLifetimes\n\n";
  for (const auto &pair : VariableLifetimes) {
    str +=
        pair.first->getNameAsString() + ": " + pair.second.DebugString() + '\n';
  }

  str += "\n>> Dependencies\n\n";
  for (const auto &pair : LifetimeDependencies) {
    str += pair.first->getNameAsString() + ": ";
    for (const auto &stmt : pair.second) {
      for (const auto &var : StmtDependencies[stmt])
        str += var->getNameAsString() + ' ';
    }
    str += '\n';
  }
  return str;
}

}  // namespace clang
