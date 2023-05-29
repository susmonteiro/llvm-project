#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis() {}
LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis(
    FunctionLifetimes &function_info) {
  const auto &params_lifetimes = function_info.GetParamsLifetimes();
  for (auto &pair : params_lifetimes) {
    VariableLifetimes.insert({pair.first, pair.second});
  }
  ReturnLifetime = Lifetime(function_info.GetReturnLifetime());
}

VariableLifetimesVector &LifetimeAnnotationsAnalysis::GetVariableLifetimes() {
  return VariableLifetimes;
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetime(
    const clang::NamedDecl *var_decl) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    // TODO error
    CreateVariable(var_decl);
  }
  Lifetime &l = VariableLifetimes[var_decl];
  return l;
}

Lifetime &LifetimeAnnotationsAnalysis::GetReturnLifetime() {
  return ReturnLifetime;
}

void LifetimeAnnotationsAnalysis::CreateDependency(const clang::NamedDecl *from,
                                                   const clang::DeclRefExpr *to,
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
  const clang::NamedDecl *decl = to->getFoundDecl();
  if (from != decl) {
    // TODO remove
    CreateLifetimeDependency(from, loc);
    CreateStmtDependency(loc, decl);
  }
}

VarStmtDependenciesMap &LifetimeAnnotationsAnalysis::GetLifetimeDependencies() {
  return LifetimeDependencies;
}

StmtVarDependenciesMap &LifetimeAnnotationsAnalysis::GetStmtDependencies() {
  return StmtDependencies;
}

void LifetimeAnnotationsAnalysis::CreateLifetimeDependency(
    const clang::NamedDecl *from, const clang::Stmt *to) {
  LifetimeDependencies[from].insert(to);
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::NamedDecl *to) {
  StmtDependencies[from].insert(to);
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::DeclRefExpr *to) {
  const clang::NamedDecl *decl = to->getFoundDecl();
  LifetimeAnnotationsAnalysis::CreateStmtDependency(from, decl);
}

llvm::DenseMap<const clang::NamedDecl *,
               llvm::DenseSet<const clang::NamedDecl *>>
LifetimeAnnotationsAnalysis::TransposeDependencies() {
  llvm::DenseMap<const clang::NamedDecl *,
                 llvm::DenseSet<const clang::NamedDecl *>>
      result;
  for (const auto &pair : LifetimeDependencies) {
    for (const auto &stmt : pair.second) {
      for (const auto &child : StmtDependencies[stmt]) {
        if (IsLifetimeNotset(child))
          result[child].insert(pair.first);
      }
    }
  }
  return result;
}

std::vector<const clang::NamedDecl *>
LifetimeAnnotationsAnalysis::InitializeWorklist() const {
  std::vector<const clang::NamedDecl *> worklist;
  for (const auto &pair : LifetimeDependencies) {
    worklist.emplace_back(pair.first);
  }
  return worklist;
}

void LifetimeAnnotationsAnalysis::ProcessShortestLifetimes() {
  // iterate over variables with no fixed lifetime
  for (const auto &pair : LifetimeDependencies) {
    auto &lifetime = GetLifetime(pair.first);
    if (lifetime.IsNotSet()) {
      lifetime.ProcessShortestLifetimes();
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
