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
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/Sema.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Error.h"

namespace clang {

using namespace ast_matchers;

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

void TransferRHS(const clang::NamedDecl *lhs, const clang::Expr *rhs,
                 PointsToMap &points_to_map,
                 LifetimeAnnotationsAnalysis &state) {
  debugLifetimes("\t[TransferRHS]");
  const auto &points_to = points_to_map.GetExprPoints(rhs);

  // debugLifetimes("This is the points to in the TransferRHS");
  for (const auto &expr : points_to) {
    if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
      // DEBUG
      // expr->dump();
      const auto *rhs_ref_decl = clang::dyn_cast<clang::DeclRefExpr>(expr);
      // debugLifetimes("Yes it is a rhs_decl!");
      state.CreateDependency(lhs, rhs_ref_decl);
    }
  }
}

namespace {
class LifetimesStmtVisitor
    : public clang::StmtVisitor<LifetimesStmtVisitor,
                                std::optional<std::string>> {
 public:
  // TODO func: pointer or reference?
  LifetimesStmtVisitor(const clang::FunctionDecl *func,
                       LifetimeAnnotationsAnalysis &state)
      : func_(func), state_(state) {}

  std::optional<std::string> VisitBinaryOperator(
      const clang::BinaryOperator *op);
  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitCastExpr(const clang::CastExpr *cast);
  std::optional<std::string> VisitCompoundStmt(const clang::CompoundStmt *stmt);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

 private:
  const clang::FunctionDecl *func_;
  LifetimeAnnotationsAnalysis &state_;
  PointsToMap points_to_map;
};

}  // namespace

namespace {

std::optional<std::string> LifetimesStmtVisitor::VisitBinaryOperator(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinaryOperator]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitBinAssign(
    const clang::BinaryOperator *op) {
  debugLifetimes("[VisitBinAssign]");

  assert(op->getLHS()->isGLValue());

  const auto &lhs = op->getLHS();
  Visit(lhs);
  const auto &lhs_points_to = points_to_map.GetExprPoints(lhs);
  points_to_map.InsertExprLifetimes(op, lhs);

  // Because of how we handle reference-like structs, a member access to a
  // non-reference-like field in a struct might still produce lifetimes. We
  // don't want to change points-to sets in those cases.
  if (!lhs->getType()->isPointerType()) {
    debugWarn("LHS of bin_op is not pointer type");
    return std::nullopt;
  }

  const auto &rhs = op->getRHS();

  Visit(rhs);

  const auto &rhs_points_to = points_to_map.GetExprPoints(rhs);
  points_to_map.InsertExprLifetimes(op, rhs);

  if (const auto *lhs_decl_ref_expr = dyn_cast<clang::DeclRefExpr>(lhs)) {
    TransferRHS(lhs_decl_ref_expr->getDecl(), rhs, points_to_map, state_);
  } else {
    // TODO
  }

  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitCastExpr(
    const clang::CastExpr *cast) {
  debugLifetimes("[VisitCastExpr]");
  switch (cast->getCastKind()) {
    case clang::CK_LValueToRValue: {
      // TODO
      debugLight("> Case LValueToRValue");
      if (cast->getType()->isPointerType()) {
        // Converting from a glvalue to a prvalue means that we need to perform
        // a dereferencing operation because the objects associated with
        // glvalues and prvalues have different meanings:
        // - A glvalue is associated with the object identified by the glvalue.
        // - A prvalue is only associated with an object if the prvalue is of
        //   pointer type; the object it is associated with is the object the
        //   pointer points to.
        // See also documentation for PointsToMap.

        // ObjectSet points_to = points_to_map_.GetPointerPointsToSet(
        //     points_to_map_.GetExprObjectSet(cast->getSubExpr()));
        // points_to_map_.SetExprObjectSet(cast, points_to);
      }

      for (const auto *child : cast->children()) {
        Visit(const_cast<clang::Stmt *>(child));

        if (auto *child_expr = dyn_cast<clang::Expr>(child)) {
          points_to_map.InsertExprLifetimes(cast, child_expr);
        }
      }
      break;
    }
    case clang::CK_NullToPointer: {
      // TODO
      debugLight("> Case NullToPointer");

      // points_to_map_.SetExprObjectSet(cast, {});
      break;
    }
    // These casts are just no-ops from a Object point of view.
    case clang::CK_FunctionToPointerDecay:
    case clang::CK_BuiltinFnToFnPtr:
    case clang::CK_ArrayToPointerDecay:
    case clang::CK_UserDefinedConversion:
      // Note on CK_UserDefinedConversion: The actual conversion happens in a
      // CXXMemberCallExpr that is a subexpression of this CastExpr. The
      // CK_UserDefinedConversion is just used to mark the fact that this is a
      // user-defined conversion; it's therefore a no-op for our purposes.
    case clang::CK_NoOp: {
      // TODO
      debugLight("> Case No-ops");

      // clang::QualType type = cast->getType().getCanonicalType();
      // if (type->isPointerType() || cast->isGLValue()) {
      //   points_to_map_.SetExprObjectSet(
      //       cast, points_to_map_.GetExprObjectSet(cast->getSubExpr()));
      // }
      break;
    }
    case clang::CK_DerivedToBase:
    case clang::CK_UncheckedDerivedToBase:
    case clang::CK_BaseToDerived:
    case clang::CK_Dynamic: {
      // TODO
      debugLight("> Case SubExpressions");

      // These need to be mapped to what the subexpr points to.
      // (Simple cases just work okay with this; may need to be revisited when
      // we add more inheritance support.)

      // ObjectSet points_to =
      // points_to_map_.GetExprObjectSet(cast->getSubExpr());
      // points_to_map_.SetExprObjectSet(cast, points_to);
      break;
    }
    case clang::CK_BitCast:
    case clang::CK_LValueBitCast:
    case clang::CK_IntegralToPointer: {
      // We don't support analyzing functions that perform a reinterpret_cast.

      // TODO
      debugLight("> Case Reinterpret cast");

      // diag_reporter_(
      //     func_->getBeginLoc(),
      //     "cannot infer lifetimes because function uses a type-unsafe cast",
      //     clang::DiagnosticIDs::Warning);
      // diag_reporter_(cast->getBeginLoc(), "type-unsafe cast occurs here",
      //                clang::DiagnosticIDs::Note);
      // return "type-unsafe cast prevents analysis";
      break;
    }
    default: {
      if (cast->isGLValue() ||
          cast->getType().getCanonicalType()->isPointerType()) {
        llvm::errs() << "Unknown cast type:\n";
        cast->dump();
        // No-noop casts of pointer types are not handled yet.
        llvm::report_fatal_error("unknown cast type encountered");
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitCompoundStmt(
    const clang::CompoundStmt *stmt) {
  debugLifetimes("[VisitCompoundStmt]");
  for (const auto &child : stmt->children()) {
    Visit(const_cast<clang::Stmt *>(child));
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitDeclRefExpr(
    const clang::DeclRefExpr *decl_ref) {
  debugLifetimes("[VisitDeclRefExpr]");

  auto *decl = decl_ref->getDecl();
  if (!clang::isa<clang::VarDecl>(decl) &&
      !clang::isa<clang::FunctionDecl>(decl)) {
    return std::nullopt;
  }

  assert(decl_ref->isGLValue() || decl_ref->getType()->isBuiltinType());

  // clang::QualType type = decl->getType().getCanonicalType();

  // if (type->isReferenceType()) {
  //   debugLifetimes("It's reference type!");

  // } else {
  //   debugLifetimes("It's not reference type");
  // }

  points_to_map.InsertExprLifetimes(decl_ref, nullptr);

  // TODO
  // if (type->isReferenceType()) {
  //   points_to_map_.SetExprObjectSet(
  //       decl_ref, points_to_map_.GetPointerPointsToSet(object));
  // } else {
  //   points_to_map_.SetExprObjectSet(decl_ref, {object});
  // }

  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitDeclStmt(
    const clang::DeclStmt *decl_stmt) {
  debugLifetimes("[VisitDeclStmt]");

  for (const clang::Decl *decl : decl_stmt->decls()) {
    if (const auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      // TODO check if pointer?
      // TODO if annotations, store annotation

      state_.CreateVariable(var_decl);
      // const Object *var_object = object_repository_.GetDeclObject(var_decl);

      // Don't need to record initializers because initialization has already
      // happened in VisitCXXConstructExpr(), VisitInitListExpr(), or
      // VisitCallExpr().
      if (var_decl->hasInit() && !var_decl->getType()->isRecordType() &&
          state_.IsLifetimeNotset(var_decl)) {
        // debugLifetimes("VarDecl has initializer!");
        const clang::Expr *init = var_decl->getInit();
        Visit(const_cast<clang::Expr *>(init));
        TransferRHS(var_decl, init, points_to_map, state_);
        // const auto &points_to = points_to_map.GetExprPoints(init);

        // debugLifetimes("This is the points to in the DeclStmt");
        // for (const auto &expr : points_to) {
        //   if (expr != nullptr && clang::isa<clang::DeclRefExpr>(expr)) {
        //     // DEBUG
        //     // expr->dump();
        //     const auto *rhs_ref_decl =
        //         clang::dyn_cast<clang::DeclRefExpr>(expr);
        //     // debugLifetimes("Yes it is a rhs_decl!");
        //     state_.CreateDependency(var_decl, rhs_ref_decl);
        //   }
        // }
      }
    }
  }
  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitExpr(
    const clang::Expr *expr) {
  debugLifetimes("[VisitExpr]");
  // TODO
  return std::nullopt;
}

std::optional<std::string> LifetimesStmtVisitor::VisitStmt(
    const clang::Stmt *stmt) {
  debugLifetimes("[VisitStmt]");
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

  LifetimesStmtVisitor visitor(func, state_);

  // debugLifetimes(">> Dumping function body before visit...");
  // func->getBody()->dump();

  std::optional<std::string> err = visitor.Visit(func->getBody());
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

  // DEBUG
  int i = 1;

  while (!worklist.empty()) {
    debugInfo("---> Iteration", i++);
    debugLifetimes("=== worklist ===");
    debugLifetimes(worklist);

    auto &el = worklist.back();
    worklist.pop_back();

    debugLifetimes("\nPropagation of", el->getNameAsString());

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
