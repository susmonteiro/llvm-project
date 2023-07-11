#include "clang/Sema/LifetimeAnnotationsAnalysis.h"

namespace clang {

LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis() {}
LifetimeAnnotationsAnalysis::LifetimeAnnotationsAnalysis(
    FunctionLifetimes &function_info, const clang::FunctionDecl *func)
    : Factory(func) {
  const auto &params_lifetimes = function_info.GetParamsLifetimes();
  for (auto &pair : params_lifetimes) {
    VariableLifetimes.insert({pair.first, pair.second});
  }
  ReturnLifetime = function_info.GetReturnLifetime();
}

VariableLifetimesVector &LifetimeAnnotationsAnalysis::GetVariableLifetimes() {
  return VariableLifetimes;
}

ObjectLifetimes LifetimeAnnotationsAnalysis::GetVarDeclLifetime(
    const clang::VarDecl *var_decl, FunctionLifetimeFactory &lifetime_factory) {
  clang::QualType type = var_decl->getType().IgnoreParens();
  clang::TypeLoc type_loc;
  if (var_decl->getTypeSourceInfo()) {
    type_loc = var_decl->getTypeSourceInfo()->getTypeLoc();
  }
  ObjectLifetimes objectsLifetimes;
  if (llvm::Error err = lifetime_factory.CreateVarLifetimes(type, type_loc)
                            .moveInto(objectsLifetimes)) {
    // TODO error
    return ObjectLifetimes();
    // return std::move(err);
  }
  return objectsLifetimes;
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

Lifetime &LifetimeAnnotationsAnalysis::GetLifetime(
    const clang::VarDecl *var_decl, unsigned int num_indirections) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    // TODO error
    CreateVariable(var_decl, Lifetime(num_indirections));
  }
  return VariableLifetimes[var_decl].GetLifetime(num_indirections);
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetimeOrLocal(
    const clang::VarDecl *var_decl, clang::QualType type) {
  return GetLifetimeOrLocal(var_decl, Lifetime::GetNumIndirections(type));
}

Lifetime &LifetimeAnnotationsAnalysis::GetLifetimeOrLocal(
    const clang::VarDecl *var_decl, unsigned int num_indirections) {
  VariableLifetimesVector::iterator it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    CreateVariable(var_decl, Lifetime(LOCAL, num_indirections));
  }
  return VariableLifetimes[var_decl].GetLifetimeOrLocal(num_indirections);
}

Lifetime &LifetimeAnnotationsAnalysis::GetReturnLifetime(
    clang::QualType &type) {
  return ReturnLifetime.GetLifetime(type);
}

Lifetime &LifetimeAnnotationsAnalysis::GetReturnLifetime(
    unsigned int num_indirections) {
  return ReturnLifetime.GetLifetime(num_indirections);
}

bool LifetimeAnnotationsAnalysis::IsLifetimeNotset(
    const clang::VarDecl *var_decl, clang::QualType &type) {
  return IsLifetimeNotset(var_decl, Lifetime::GetNumIndirections(type));
}

bool LifetimeAnnotationsAnalysis::IsLifetimeNotset(
    const clang::VarDecl *var_decl, unsigned int num_indirections) {
  auto it = VariableLifetimes.find(var_decl);
  ObjectLifetimes ol;
  if (it != VariableLifetimes.end()) {
    ol = it->second;
  } else {
    ol = GetVarDeclLifetime(var_decl, Factory);
    CreateVariable(var_decl, ol);
  }
  return ol.GetLifetime(num_indirections).IsNotSet();
}

void LifetimeAnnotationsAnalysis::CreateVariableIfNotFound(
    const clang::VarDecl *var_decl) {
  auto it = VariableLifetimes.find(var_decl);
  if (it == VariableLifetimes.end()) {
    ObjectLifetimes ol = GetVarDeclLifetime(var_decl, Factory);
    CreateVariable(var_decl, ol);
  }
}

void LifetimeAnnotationsAnalysis::CreateDependencySimple(
    const clang::VarDecl *from, clang::QualType from_type,
    const clang::VarDecl *to, clang::QualType to_type, const clang::Stmt *loc) {
  CreateDependencySimple(from, Lifetime::GetNumIndirections(from_type), to,
                         Lifetime::GetNumIndirections(to_type), loc);
}

void LifetimeAnnotationsAnalysis::CreateDependencySimple(
    const clang::VarDecl *from, unsigned int from_num_indirections,
    const clang::VarDecl *to, unsigned int to_num_indirections,
    const clang::Stmt *loc) {
  if (IsLifetimeNotset(from, from_num_indirections)) {
    CreateLifetimeDependency(from, from_num_indirections, loc,
                             to_num_indirections);
    CreateStmtDependency(loc, to);
  }
}

void LifetimeAnnotationsAnalysis::CreateDependency(const clang::VarDecl *from,
                                                   clang::QualType from_type,
                                                   const clang::VarDecl *to,
                                                   clang::QualType to_type,
                                                   const clang::Stmt *loc) {
  unsigned int from_num_indirections = Lifetime::GetNumIndirections(from_type);
  unsigned int to_num_indirections = Lifetime::GetNumIndirections(to_type);
  CreateDependency(from, from_num_indirections, to, to_num_indirections, loc);
}

void LifetimeAnnotationsAnalysis::CreateDependency(
    const clang::VarDecl *from, unsigned int from_num_indirections,
    const clang::VarDecl *to, unsigned int to_num_indirections,
    const clang::Stmt *loc) {
  CreateVariableIfNotFound(to);

  if (IsLifetimeNotset(from, from_num_indirections)) {
    CreateLifetimeDependency(from, from_num_indirections, loc,
                             to_num_indirections);
    CreateStmtDependency(loc, to);
  }

  // TODO not necessarily true
  // assert(from_num_indirections == to_num_indirections &&
  //        "Indirections must be equal");

  if (from_num_indirections == 0 || to_num_indirections == 0) {
    return;
  }

  from_num_indirections--;
  to_num_indirections--;

  while (from_num_indirections > 0 && to_num_indirections > 0) {
    if (IsLifetimeNotset(from, from_num_indirections)) {
      CreateLifetimeDependency(from, from_num_indirections, loc,
                               to_num_indirections);
      CreateStmtDependency(loc, to);
    }
    if (IsLifetimeNotset(to, from_num_indirections)) {
      CreateLifetimeDependency(to, to_num_indirections, loc,
                               from_num_indirections);
      CreateStmtDependency(loc, from);
    }
    from_num_indirections--;
    to_num_indirections--;
  }
}

VarStmtDependenciesMap &LifetimeAnnotationsAnalysis::GetLifetimeDependencies() {
  return LifetimeDependencies;
}

StmtVarDependenciesMap &LifetimeAnnotationsAnalysis::GetStmtDependencies() {
  return StmtDependencies;
}

char LifetimeAnnotationsAnalysis::GetStmtLifetime(const clang::Stmt *stmt) {
  return StmtLifetimes[stmt];
}

void LifetimeAnnotationsAnalysis::CreateLifetimeDependency(
    const clang::VarDecl *from, clang::QualType from_type,
    const clang::Stmt *to, clang::QualType to_type) {
  CreateLifetimeDependency(from, Lifetime::GetNumIndirections(from_type), to,
                           Lifetime::GetNumIndirections(to_type));
}

void LifetimeAnnotationsAnalysis::CreateLifetimeDependency(
    const clang::VarDecl *from, unsigned int from_num_indirections,
    const clang::Stmt *to, unsigned int to_num_indirections) {
  LifetimeDependencies[LHSTypeStruct{from, from_num_indirections}].insert(
      RHSTypeStruct{to, to_num_indirections});
}

void LifetimeAnnotationsAnalysis::CreateStmtDependency(
    const clang::Stmt *from, const clang::VarDecl *to) {
  StmtDependencies[from].insert(to);
}

void LifetimeAnnotationsAnalysis::CreateStmtLifetime(const clang::Stmt *from,
                                                     char id) {
  StmtLifetimes[from] = id;
}

llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<LHSTypeStruct>>
LifetimeAnnotationsAnalysis::TransposeDependencies() {
  llvm::DenseMap<LHSTypeStruct, llvm::DenseSet<LHSTypeStruct>> result;
  for (const auto &info_lhs : LifetimeDependencies) {
    unsigned int lhs_num_indirections = info_lhs.first.num_indirections;
    for (const auto &info_rhs : info_lhs.second) {
      unsigned int rhs_num_indirections = info_rhs.num_indirections;
      for (const auto &child : StmtDependencies[info_rhs.stmt]) {
        if (IsLifetimeNotset(child, lhs_num_indirections) &&
            info_lhs.first.var_decl != child)
          result[LHSTypeStruct{child, rhs_num_indirections}].insert(
              info_lhs.first);
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
    str += "[type] " + std::string(info_lhs.first.num_indirections, '*') +
           "\t\t[vars] ";
    for (const auto &info_rhs : info_lhs.second) {
      for (const auto &var : StmtDependencies[info_rhs.stmt]) {
        if (info_lhs.first.var_decl == var) continue;
        str += std::string(info_rhs.num_indirections, '*') + ' ' +
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
    res += '{' + std::string(el.num_indirections, '*') + ' ' +
           el.var_decl->getNameAsString() + "}, ";
  }
  res += '\n';
  return res;
}

}  // namespace clang
