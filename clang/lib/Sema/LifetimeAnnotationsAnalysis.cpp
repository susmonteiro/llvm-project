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

VariableLifetimesMap &LifetimeAnnotationsAnalysis::GetVariableLifetimes() {
  return VariableLifetimes;
}
DependenciesMap &LifetimeAnnotationsAnalysis::GetDependencies() {
  return Dependencies;
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetime(
    const clang::NamedDecl *var_decl) {
  VariableLifetimesMap::iterator it = VariableLifetimes.find(var_decl);
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

void LifetimeAnnotationsAnalysis::CreateDependency(
    const clang::NamedDecl *from, const clang::DeclRefExpr *to) {
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
    Dependencies[from].insert(decl);
  }
}

DependenciesMap LifetimeAnnotationsAnalysis::TransposeDependencies() const {
  DependenciesMap result;
  for (const auto &pair : Dependencies) {
    for (const auto &child : pair.second) {
      // don't insert annotated variables into the parents graph
      if (IsLifetimeNotset(child)) result[child].insert(pair.first);
    }
  }
  return result;
}

std::vector<const clang::NamedDecl *>
LifetimeAnnotationsAnalysis::InitializeWorklist() const {
  std::vector<const clang::NamedDecl *> worklist;
  for (const auto &pair : Dependencies) {
    worklist.emplace_back(pair.first);
  }
  return worklist;
}

void LifetimeAnnotationsAnalysis::ProcessShortestLifetimes() {
  // iterate over variables with no fixed lifetime
  for (const auto &pair : Dependencies) {
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
  for (const auto &pair : Dependencies) {
    str += pair.first->getNameAsString() + ": ";
    for (const auto &var : pair.second) {
      str += var->getNameAsString() + ' ';
    }
    str += '\n';
  }
  return str;
}

}  // namespace clang
