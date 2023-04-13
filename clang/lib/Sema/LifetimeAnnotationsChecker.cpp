#include "clang/Sema/LifetimeAnnotationsChecker.h"

#include <iostream>

#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
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

// namespace {
// class TransferStmtVisitor
//     : public clang::StmtVisitor<TransferStmtVisitor,
//                                 std::optional<std::string>> {
//  public:
//   TransferStmtVisitor(const clang::FunctionDecl *func) : func_(func) {}

//   std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);

//  private:
//   const clang::FunctionDecl *func_;
// };
// }  // namespace

void LifetimeAnnotationsChecker::AnalyzeFunctionBody(const FunctionDecl *func,
                                                     Sema &S) {
  // TODO implement
  if (func->getBody()) {
    debugLifetimes("Function has body");
  } else {
    debugLifetimes("Function does not have body");
  }

  clang::ASTContext &ast_context = func->getASTContext();
  clang::SourceManager &source_manager = ast_context.getSourceManager();
  clang::SourceRange func_source_range = func->getSourceRange();

  // step 2
  LifetimeAnnotationsChecker::PropagateLifetimes();

  // step 3
  LifetimeAnnotationsChecker::CheckLifetimes();
}

void LifetimeAnnotationsChecker::PropagateLifetimes() {
  // TODO
  // After capturing lifetimes from the function, apply the fixed point
  // algorithm
}

void LifetimeAnnotationsChecker::CheckLifetimes() {
  // TODO
  // With all the lifetime information acquired, check that the return
  // statements and the attributions are correct
}

// TODO list
//   TODO add this function header to .h
//   TODO define the class TransferStmtVisitor in this file
//   TODO define LifetimeLattice class
//   TODO make this function be called from the GetLifetimes
//   TODO implement some functions of the StmtVisitor

// void LifetimeAnnotationsChecker::transfer(
//     const clang::CFGElement &elt, LifetimeLattice &state,
//     clang::dataflow::Environment & /*environment*/) {
//   if (state.IsError())
//     return;

//   auto cfg_stmt = elt.getAs<clang::CFGStmt>();
//   if (!cfg_stmt)
//     return;
//   auto stmt = cfg_stmt->getStmt();

//   TransferStmtVisitor visitor(object_repository_, state.PointsTo(),
//                               state.Constraints(),
//                               state.SingleValuedObjects(), func_,
//                               callee_lifetimes_, diag_reporter_);

//   // * visitor pattern -> visit the specific function and handle different
//   // * elements in each specific way
//   if (std::optional<std::string> err =
//           visitor.Visit(const_cast<clang::Stmt *>(stmt))) {
//     state = LifetimeLattice(*err);
//   }
// }

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

    func_lifetimes.DumpParameters();
    func_lifetimes.DumpReturn();

    // TODO insert on map

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

// namespace {
// // Below is the implementation of all visit functions
// std::optional<std::string> TransferStmtVisitor::VisitDeclStmt(
//     const clang::DeclStmt *decl_stmt) {
//   // TODO implement
// }
// }  // namespace

}  // namespace clang
