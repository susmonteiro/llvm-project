#include "clang/Sema/LifetimeAnnotationsChecker.h"

#include <iostream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/IdentifierResolver.h"
#include "clang/Sema/Sema.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"

namespace clang {

using namespace ast_matchers;

// === MATCHER ===
// namespace {
// class LifetimeAnnotationsProcessor : public MatchFinder::MatchCallback {
//  public:
//   // TODO change from * to &
//   LifetimeAnnotationsProcessor(LifetimeAnnotationsAnalysis *state,
//                                FunctionLifetimes *func_info)
//       : state_(state), func_info_(func_info) {}

//  private:
//   // x = (p-q == 0) ? p : p;
//   void VisitExpr(const clang::Expr *expr,
//                  const clang::DeclRefExpr **decl_ref_expr);
//   void VisitVarDecl(const clang::VarDecl *var_decl);
//   void VisitCallExpr(const clang::CallExpr *call_expr);
//   void VisitAssignment(const clang::BinaryOperator *bin_op);

//   void run(const ast_matchers::MatchFinder::MatchResult &Result) override;

//   LifetimeAnnotationsAnalysis *state_;
//   FunctionLifetimes *func_info_;
// };

// }  // namespace

// TODO change to not void
void GetExprObjectSet(const clang::Expr *expr,
                      LifetimeAnnotationsAnalysis *state) {
  // We can't handle `ParenExpr`s like other `Expr`s because the CFG doesn't
  // contain `CFGStmt`s for them. Instead, if we encounter a `ParenExpr` here,
  // we simply return the object set for its subexpression.
  if (auto paren = clang::dyn_cast<clang::ParenExpr>(expr)) {
    expr = paren->getSubExpr();
  }

  assert(expr->isGLValue() || expr->getType()->isPointerType() ||
         expr->getType()->isArrayType() || expr->getType()->isFunctionType() ||
         expr->getType()->isBuiltinType());

  // TODO implement this better

  // const auto &variable_lifetimes = state->GetVariableLifetimes();
  // auto iter = variable_lifetimes.find(expr);
  // if (iter == variable_lifetimes.end()) {
  //   // TODO error?
  // }
  // return iter->second();
}

// TODO this is just an experience - delete or change this
// void CheckReturnLifetime(const clang::FunctionDecl *func,
//                          FunctionLifetimes &function_lifetimes, Sema &S) {
//   clang::QualType return_type = func->getReturnType();
//   clang::QualType return_pointee = PointeeType(return_type);
//   if (return_pointee.isNull()) return;

//   Lifetime l = function_lifetimes.GetReturnLifetimes();

//   if (!function_lifetimes.CheckIfLifetimeIsDefined(l))
//     S.Diag(func->getLocation(), diag::warn_return_undefined_lifetime)
//         << func->getSourceRange();
// }

// === MATCHER ===
// namespace {

// void LifetimeAnnotationsProcessor::VisitExpr(
//     const clang::Expr *expr,
//     const clang::DeclRefExpr **decl_ref_expr = nullptr) {
//   debugLifetimes("[VisitExpr]");
//   if (const clang::ImplicitCastExpr *implicit_cast_expr =
//           dyn_cast<clang::ImplicitCastExpr>(expr)) {
//     // debugLifetimes("Is implicit cast");
//     // implicit_cast_expr->dump();
//     const auto &children = implicit_cast_expr->children();
//     for (const auto &child : children) {
//       // debugLifetimes("This is a child of implicit_cast");
//       // child->dump();
//       if (const clang::DeclRefExpr *decl =
//               dyn_cast<clang::DeclRefExpr>(child)) {
//         // debugLifetimes("Is declRefExpr");
//         *decl_ref_expr = decl;
//         return;
//       }
//     }
//   }
// }

// void LifetimeAnnotationsProcessor::VisitVarDecl(
//     const clang::VarDecl *var_decl) {
//   debugLifetimes("[VisitVarDecl]", var_decl->getNameAsString());
//   // TODO check if pointer?
//   state_->CreateVariable(var_decl);
//   // TODO if annotations, store annotation

//   // Don't need to record initializers because initialization has already
//   // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
//   // VisitCallExpr().
//   // If the variable of the LHS has a lifetime annotation, don't process RHS
//   if (var_decl->hasInit() && !var_decl->getType()->isRecordType() &&
//       state_->IsLifetimeNotset(var_decl)) {
//     debugLifetimes("VarDecl has initializer!");
//     const clang::Expr *init_expr = var_decl->getInit();

//     // debugLifetimes("Dump init expr");
//     // init_expr->dump();

//     const clang::DeclRefExpr *decl_ref_expr = nullptr;
//     VisitExpr(init_expr, &decl_ref_expr);

//     if (decl_ref_expr != nullptr) {
//       // debugLifetimes("RHS is a DeclRefExpr");
//       state_->CreateDependency(var_decl, decl_ref_expr);
//       // ! old
//       // const auto &rhs_decl = decl_ref_expr->getDecl();
//       // if (state_->IsLifetimeNotset(rhs_decl)) {
//       //   state_->CreateDependency(var_decl, decl_ref_expr);
//       // } else {
//       //   Lifetime *rhs_lifetime = state_->GetLifetime(rhs_decl);
//       //   state_->InsertShortestLifetimes(var_decl, rhs_lifetime);
//       // }
//     }

//     GetExprObjectSet(init_expr, state_);
//     // TODO implement
//   }
//   // return std::nullopt;
// }

// void LifetimeAnnotationsProcessor::VisitCallExpr(
//     const clang::CallExpr *call_expr) {
//   debugLifetimes("[VisitCallExpr]");
// }

// void LifetimeAnnotationsProcessor::VisitAssignment(
//     const clang::BinaryOperator *bin_op) {
//   debugLifetimes("[VisitAssignment]");
//   bin_op->dump();
// }

// void LifetimeAnnotationsProcessor::run(
//     const ast_matchers::MatchFinder::MatchResult &Result) {
//   // DEBUG
//   // debugInfo("On run...");
//   if (const auto *var_decl =
//           Result.Nodes.getNodeAs<clang::VarDecl>("var_decl")) {
//     VisitVarDecl(var_decl);
//     return;
//   }

//   if (const auto *call_expr =
//           Result.Nodes.getNodeAs<clang::CallExpr>("call_expr")) {
//     VisitCallExpr(call_expr);
//     return;
//   }

//   if (const auto *assignment =
//           Result.Nodes.getNodeAs<clang::BinaryOperator>("assignment")) {
//     VisitAssignment(assignment);
//     return;
//   }
//   debugWarn("Matched something with no visit function");
// }

// }  // namespace

namespace {
class TransferStmtVisitor
    : public clang::StmtVisitor<TransferStmtVisitor,
                                std::optional<std::string>> {
 public:
  TransferStmtVisitor(const clang::FunctionDecl *func) : func_(func) {}

  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitBinaryOperator(
      const clang::BinaryOperator *op);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

 private:
  const clang::FunctionDecl *func_;
};

}  // namespace

namespace {

std::optional<std::string> TransferStmtVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr] - VISITOR");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitBinaryOperator(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinaryOperator] - VISITOR");
  // TODO
  return std::nullopt;
}

std::optional<std::string> TransferStmtVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt] - VISITOR");
  // TODO
  return std::nullopt;
}

}  // namespace

// Process functions' headers
void LifetimeAnnotationsChecker::GetLifetimes(const FunctionDecl *func,
                                              Sema &S) {
  debugLifetimes("GetLifetimes of function", func->getNameAsString());

  // BuildBaseToOverrides
  // AnalyzeTranslationUnitAndCollectTemplates -> templates

  func = func->getCanonicalDecl();

  // TODO AnalyzeFunctionRecursive -> templates, virtual, etc.
  // auto *cxxmethod = clang::dyn_cast<clang::CXXMethodDecl>(func);
  // bool is_virtual = cxxmethod != nullptr && cxxmethod->isVirtual();
  // bool is_pure_virtual = is_virtual && cxxmethod->isPure();

  // TODO uncomment when we have defined the data structures
  // if (!func->isDefined() && !is_pure_virtual && !is_analyzed) {
  //     FunctionLifetimes annotations;
  //     if (llvm::Error err = GetLifetimeAnnotations(func, lifetime_context)
  //                             .moveInto(annotations)) {
  //     analyzed[func] = FunctionAnalysisError(err);
  //     } else {
  //     analyzed[func] = annotations;
  //     }
  //     return;
  // }

  if (!func->isDefined()) {
    // DEBUG
    // debugLifetimes("Function is not defined");
    // func->dump();
  }

  // TODO ellision

  // Following Case 2. Not part of a cycle.
  // AnalyzeSingleFunction()
  FunctionLifetimeFactory function_lifetime_factory(
      /* elision_enabled, */ func);

  // if (function_info_[func]) {
  //   debugLifetimes("Before setting of map");

  //   func_lifetimes.DumpParameters();
  //   func_lifetimes.DumpReturn();

  //   function_info_[func] = std::move(func_lifetimes);

  //   // TODO this should be validated in the check step
  //   // CheckReturnLifetime(func, func_lifetimes, S);

  //   debugLifetimes("After setting of map");

  //   // TODO maybe keep track of analyzed functions

  // } else {
  //   // TODO error
  //   /* return llvm::createStringError(
  //         llvm::inconvertibleErrorCode(),
  //         llvm::toString(func_lifetimes.takeError())
  //         // TODO abseil
  //         // absl::StrCat("Lifetime elision not enabled for '",
  //         //              func->getNameAsString(), "'")
  //     ); */
  //   return;
  // }

  llvm::Expected<FunctionLifetimes> expected_func_lifetimes =
      FunctionLifetimes::CreateForDecl(func, function_lifetime_factory);

  if (expected_func_lifetimes) {
    FunctionLifetimes func_lifetimes = *expected_func_lifetimes;

    // DEBUG
    // func_lifetimes.DumpParameters();
    // func_lifetimes.DumpReturn();

    // TODO insert on map
    // TODO std::move?
    function_info_[func] = std::move(func_lifetimes);

    // TODO this should be validated in the check step
    // CheckReturnLifetime(func, func_lifetimes, S);

    // TODO maybe keep track of analyzed functions

  } else {
    // TODO error
    /* return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          llvm::toString(func_lifetimes.takeError())
          // TODO abseil
          // absl::StrCat("Lifetime elision not enabled for '",
          //              func->getNameAsString(), "'")
      ); */
    return;
  }
}

// Process functions' bodies
void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  // DEBUG
  // DumpFunctionInfo();
  auto function_info = function_info_[func];
  clang::ASTContext &Context = func->getASTContext();
  state_ = LifetimeAnnotationsAnalysis(function_info.GetParamsLifetimes());

  // step 1
  debugInfo("\n====== START STEP 1 ======\n");

  GetLifetimeDependencies(func, Context, function_info);

  debugInfo("\n====== FINISH STEP 1 ======\n");
  debugLifetimes(state_.DebugString());

  // clang::SourceManager &source_manager = ast_context.getSourceManager();
  // clang::SourceRange func_source_range = func->getSourceRange();

  // step 2
  debugInfo("\n====== START STEP 2 ======\n");
  LifetimeAnnotationsChecker::PropagateLifetimes();
  debugInfo("\n====== FINISH STEP 2 ======\n");
  debugLifetimes(state_.DebugString());

  // step 3
  debugInfo("\n====== START STEP 3 ======\n");
  LifetimeAnnotationsChecker::CheckLifetimes();
  debugInfo("\n====== FINISH STEP 3 ======\n");
}

void LifetimeAnnotationsChecker::GetLifetimeDependencies(
    const clang::FunctionDecl *func, clang::ASTContext &Context,
    FunctionLifetimes &func_info) {
  debugLifetimes("[GetLifetimeDependencies]");

  // === VISITOR ===

  TransferStmtVisitor visitor(func);

  // debugLifetimes(">> Dumping function body before visit...");
  // func->getBody()->dump();
  
  std::optional<std::string> err =
      visitor.Visit(const_cast<clang::Stmt *>(func->getBody()));

  // === MATCHER ===

  // auto expr_matcher = findAll(expr());
  // auto var_decl_matcher = findAll(
  //     varDecl(
  //         unless(
  //             parmVarDecl()) /* , hasInitializer(expr().bind("initializer"))
  //             */)
  //         .bind("var_decl"));

  // auto call_expr_matcher = findAll(callExpr().bind("call_expr"));

  // FIXME
  // auto assign_matcher = findAll(
  //     binaryOperator(hasOperatorName("="),
  //                    hasLHS(declRefExpr(to(varDecl().bind("rhs_vardecl")))))
  //         .bind("assignment"));
  // TODO try on a matcher which receives func->getCompoundStmt()
  // auto assign_matcher =
  // compoundStmt(eachOf(binaryOperator().bind("assignment")));
  // auto assign_matcher = binaryOperator().bind("assignment");

  // MatchFinder Finder;
  // LifetimeAnnotationsProcessor Callback(&state_, &func_info);
  // Finder.addMatcher(var_decl_matcher, &Callback);
  // Finder.addMatcher(call_expr_matcher, &Callback);
  // Finder.addMatcher(assign_matcher, &Callback);
  // Finder.addMatcher(expr_matcher, &Callback);

  // func->dump();
  // decls need func
  // Finder.match(*func, Context);
  // exprs need functionBody
  // Finder.match(*functionBody, Context);
  // FIXME operators?

  // DEBUG
  // functionBody->dump();
  // func->dump();
}

// After capturing lifetimes from the function, apply the fixed point
// algorithm
void LifetimeAnnotationsChecker::PropagateLifetimes() {
  auto children = state_.GetDependencies();
  auto parents = std::move(state_.TransposeDependencies());

  debugLifetimes("=== dependencies_ ===");
  debugLifetimes(children);

  debugLifetimes("=== parents (transposed) ===");
  debugLifetimes(parents);

  auto worklist = state_.InitializeWorklist();

  while (!worklist.empty()) {
    debugLifetimes("=== worklist ===");
    debugLifetimes(worklist);

    auto &el = worklist.back();
    worklist.pop_back();

    llvm::DenseSet<const clang::NamedDecl *> result = {el};
    llvm::DenseSet<char> shortest_lifetimes;
    for (const auto &child : children[el]) {
      if (child == el) continue;
      result.insert(children[child].begin(), children[child].end());
      auto tmp_lifetimes = state_.GetShortestLifetimes(child);
      if (state_.IsLifetimeNotset(child)) {
        shortest_lifetimes.insert(tmp_lifetimes.begin(), tmp_lifetimes.end());
      } else {
        shortest_lifetimes.insert(state_.GetLifetime(child)->Id());
      }
    }
    if (children[el] != result ||
        state_.GetShortestLifetimes(el) != shortest_lifetimes) {
      children[el].insert(result.begin(), result.end());
      state_.PropagateShortestLifetimes(el, shortest_lifetimes);

      for (const auto &parent : parents[el]) {
        worklist.emplace_back(parent);
      }
    }
    debugLifetimes("\nPropagation of", el->getNameAsString());
    debugLifetimes("=== children ===");
    debugLifetimes(children);
  }
  // return children;

  state_.SetDependencies(children);

  // TODO
  debugLifetimes("=== state_.dependencies_ ===");
  debugLifetimes(state_.GetDependencies());

  // ! if a Lifetime is unset and has no shortest_lifetimes, do nothing
  // ! if a lifetime is local, then set el to local
  // ! if a lifetime is static, then include it in el
  // ! if a lifetime has id_, then skip shortest_lifetimes
  // TODO at the end of the cycle
  // - check if id is local and if so skip next steps
  // - check if a variable has only one "shortest_lifetimes" and set it to
  // the main lifetime
  // - static? Probably nothing to do

  // finally, process the lifetimes dependencies to attribute the correct set of
  // lifetimes to each variable
  // state_.ProcessVarLifetimes();
}

// With all the lifetime information acquired, check that the return
// statements and the attributions are correct
void LifetimeAnnotationsChecker::CheckLifetimes() {
  // TODO
}

}  // namespace clang
