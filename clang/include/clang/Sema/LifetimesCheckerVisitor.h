#ifndef LIFETIMES_CHECKER_VISITOR_H_
#define LIFETIMES_CHECKER_VISITOR_H_

#include <iostream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Sema/PointsToMap.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/Error.h"

namespace clang {
using PrintNotesFactory = std::function<void(
    const clang::VarDecl *, const clang::Decl *, const clang::BinaryOperator *,
    const clang::Expr *, const clang::Stmt *, Lifetime &, Lifetime &)>;

constexpr int MAX_NUM_NOTES = 9;

class LifetimesCheckerVisitorFactory {
 public:
  LifetimesCheckerVisitorFactory(Sema &sema) : S(sema) {}

  int PrintNotes(Lifetime &lifetime, const clang::Decl *var_decl, int msg,
                 int notes) const;
  int PrintNotes(Lifetime &lifetime, clang::SourceLocation loc,
                 clang::SourceRange range, int msg, int notes) const;
  int PrintNotes(Lifetime &lifetime, const clang::Decl *var_decl, int msg,
                 char id, int notes) const;
  int PrintNotes(Lifetime &lifetime, clang::SourceLocation Loc,
                 clang::SourceRange range, int msg, char id, int notes) const;

  PrintNotesFactory BinAssignFactory() const;
  PrintNotesFactory DeclStmtFactory() const;
  PrintNotesFactory ReturnStmtFactory() const;

 private:
  Sema &S;
};

class LifetimesCheckerVisitor
    : public clang::StmtVisitor<LifetimesCheckerVisitor,
                                std::optional<std::string>> {
 public:
  LifetimesCheckerVisitor(
      const clang::FunctionDecl *func, LifetimeAnnotationsAnalysis &state,
      Sema &sema,
      llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &func_info)
      : Func(func),
        State(state),
        FuncInfo(func_info),
        PointsTo(state.GetPointsTo()),
        S(sema),
        Factory(sema) {}

  std::optional<std::string> VisitBinAssign(const clang::BinaryOperator *op);
  std::optional<std::string> VisitCallExpr(const clang::CallExpr *call);
  std::optional<std::string> VisitDeclRefExpr(
      const clang::DeclRefExpr *decl_ref);
  std::optional<std::string> VisitDeclStmt(const clang::DeclStmt *decl_stmt);
  std::optional<std::string> VisitExpr(const clang::Expr *expr);
  std::optional<std::string> VisitReturnStmt(
      const clang::ReturnStmt *return_stmt);
  std::optional<std::string> VisitStmt(const clang::Stmt *stmt);

  const clang::VarDecl *GetDeclFromArg(const clang::Expr *arg) const;

  void VerifyBinAssign(
      clang::Expr *lhs, const clang::Expr *rhs, const clang::Expr *expr,
      const clang::BinaryOperator *op,
      const llvm::SmallSet<const clang::Expr *, 2U> &lhs_points_to) const;

  void VerifyMaxLifetimes(
      const clang::UnaryOperator *deref_op, const clang::Expr *rhs,
      const clang::BinaryOperator *op,
      const llvm::SmallSet<const clang::Expr *, 2U> &lhs_points_to) const;

  void ParamsCallExprChecker(
      const clang::CallExpr *call, const clang::FunctionDecl *direct_callee,
      const clang::VarDecl *first_arg, const clang::VarDecl *second_arg,
      Lifetime &first_lifetime, Lifetime &second_lifetime,
      int first_num_indirections, int second_num_indirections,
      unsigned int num_indirections, int msg) const;

  void CallExprChecker(const clang::VarDecl *lhs_var_decl,
                       unsigned int lhs_num_indirections,
                       const clang::Expr *expr,
                       unsigned int rhs_num_indirections,
                       const clang::Stmt *stmt, const clang::BinaryOperator *op,
                       bool is_return, clang::TypeToSet &call_info,
                       PrintNotesFactory factory) const;

  void DeclChecker(const clang::VarDecl *lhs_var_decl,
                   unsigned int lhs_num_indirections, const clang::Expr *expr,
                   const clang::VarDecl *rhs_var_decl,
                   unsigned int rhs_num_indirections, const clang::Stmt *stmt,
                   const clang::BinaryOperator *op, bool is_return,
                   PrintNotesFactory factory) const;

  void CompareAndCheck(const clang::VarDecl *lhs_var_decl,
                       unsigned int lhs_num_indirections,
                       const clang::Expr *expr, const clang::Expr *rhs,
                       unsigned int rhs_num_indirections,
                       const clang::Stmt *stmt, const clang::BinaryOperator *op,
                       bool return_lifetime, PrintNotesFactory factory) const;

  int GenerateNotes(const clang::VarDecl *var_decl, Lifetime &lifetime,
                 unsigned int num_indirections, int notes) const;
  int GenerateNotes(const clang::VarDecl *var_decl, Lifetime &lifetime, char id,
                 unsigned int num_indirections, int notes) const;
  void GenerateNotes(const clang::VarDecl *var_decl, char id,
                  unsigned int num_indirections) const;

 private:
  const clang::FunctionDecl *Func;
  LifetimeAnnotationsAnalysis &State;
  llvm::DenseMap<const clang::FunctionDecl *, FunctionLifetimes> &FuncInfo;
  PointsToMap &PointsTo;
  Sema &S;
  LifetimesCheckerVisitorFactory Factory;
  bool debugEnabled = false;
};

}  // namespace clang

#endif  // LIFETIMES_CHECKER_VISITOR_H_
