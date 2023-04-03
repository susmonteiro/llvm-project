//===--- LifetimeAnnotationsChecker.cpp - Static Analysis for Lifetime
// Annotations ----------------===//

#include <iostream>
#include <unordered_set>

#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

using namespace std;
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

static constexpr llvm::StringLiteral FuncID("fun");

// debug
void debug(string text) { cout << text << endl; }
void debug(string text1, string text2) {
  cout << text1 << ": " << text2 << endl;
}

// StatementMatcher LoopMatcher =
//         forStmt(hasLoopInit(declStmt(
//                     hasSingleDecl(varDecl(hasInitializer(integerLiteral(equals(0))))
//                                       .bind("initVarName")))),
//                 hasIncrement(unaryOperator(
//                     hasOperatorName("++"),
//                     hasUnaryOperand(declRefExpr(
//                         to(varDecl(hasType(isInteger())).bind("incVarName")))))),
//                 hasCondition(binaryOperator(
//                     hasOperatorName("<"),
//                     hasLHS(ignoringParenImpCasts(declRefExpr(
//                         to(varDecl(hasType(isInteger())).bind("condVarName"))))),
//                     hasRHS(expr(hasType(isInteger())))))).bind("forLoop");

// === Lifetime Annotations ===
// TODO lifetime elision -> check crubit
// TODO if we have multiple declarations of a function, make sure they are all
// annotated with the same lifetimes.

// TODO
// llvm::Expected<FunctionLifetimes>
void ParseLifetimeAnnotations(
    const clang::FunctionDecl *func,
    llvm::DenseMap<std::string, Lifetime> &symbol_table,
    const std::string &lifetimes_str) {
  clang::LangOptions lang_opts;
  clang::Lexer lexer(clang::SourceLocation(), lang_opts, lifetimes_str.data(),
                     lifetimes_str.data(),
                     lifetimes_str.data() + lifetimes_str.size());

  const char *end = lifetimes_str.data() + lifetimes_str.size();

  auto tok = [&lexer, &lifetimes_str, end]() -> llvm::StringRef {
    clang::Token token;
    if (lexer.getBufferLocation() != end) {
      lexer.LexFromRawLexer(token);
      return llvm::StringRef(
          lifetimes_str.data() + token.getLocation().getRawEncoding(),
          token.getLength());
    }
    return "";
  };

  // TODO(veluca): this is too permissive.
  auto next_lifetime = [&]() {
    llvm::StringRef next = tok();
    while (next == "(" || next == ")" || next == "," || next == "->" ||
           next == ":" || next == "[" || next == "]" || next == ">" ||
           next == "<") {
      next = tok();
    }
    return next;
  };

  FunctionLifetimeFactorySingleCallback factory(
      [&symbol_table,
       &next_lifetime](const clang::Expr *) -> llvm::Expected<Lifetime> {
        llvm::StringRef next = next_lifetime();
        if (next.empty()) {
          return llvm::createStringError(
              llvm::inconvertibleErrorCode(),
              "Invalid lifetime annotation: too few lifetimes");
        }
        return symbol_table.LookupNameAndMaybeDeclare(next);
      });

  auto ret = FunctionLifetimes::CreateForDecl(func, factory);

  if (!next_lifetime().empty()) {
    return llvm::createStringError(
        llvm::inconvertibleErrorCode(),
        "Invalid lifetime annotation: too many lifetimes");
  }
  return ret;
}

// TODO
// llvm::Expected<FunctionLifetimes>
void ParseLifetimeAnnotations(
    const clang::FunctionDecl *func, const std::string &lifetimes_str,
    llvm::DenseMap<std::string, Lifetime> *symbol_table) {
  return ParseLifetimeAnnotations(func, *symbol_table, lifetimes_str);
}

// TODO
// llvm::Expected<FunctionLifetimes>
void GetLifetimeAnnotationsInternal(
    const clang::FunctionDecl *func, LifetimeSymbolTable &symbol_table,
    llvm::DenseMap<std::string, Lifetime> *symbol_table) {
  const clang::AnnotateAttr *lifetime_annotation = nullptr;
  // * clang::annotate annotations
  for (const clang::Attr *attr : func->attrs()) {
    debug("Func has attrs");
    if (auto annotate = clang::dyn_cast<clang::AnnotateAttr>(attr)) {
      if (annotate->getAnnotation() == "lifetimes") {
        if (lifetime_annotation != nullptr) {
          return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                         "Can't extract lifetime...");
          // TODO abseil
          // absl::StrCat("Can't extract lifetimes as '",
          //              func->getNameAsString(),
          //              "' has multiple lifetime annotations"));
        }
        lifetime_annotation = annotate;
      }
    }
  }

  debug("No more attrs");

  // * lifetime_annotation will be empty if they are clang::annotate
  if (lifetime_annotation) {
    debug("ParseLifetimeAnnotations");
    return ParseLifetimeAnnotations(func, symbol_table, lifetime_annotation);
  }

  class Factory : public FunctionLifetimeFactory {
   public:
    Factory(bool elision_enabled, const clang::FunctionDecl *func,
            LifetimeSymbolTable &symbol_table)
        : elision_enabled(elision_enabled),
          func(func),
          symbol_table(symbol_table) {}

   private:
    llvm::Expected<Lifetime> LifetimeFromName(const clang::Expr *name) const {
      llvm::StringRef name_str;
      if (llvm::Error err = EvaluateAsStringLiteral(name, func->getASTContext())
                                .moveInto(name_str)) {
        return std::move(err);
      }
      return symbol_table.LookupNameAndMaybeDeclare(name_str);
    }

    LifetimeFactory ParamLifetimeFactory() const {
      return [this](const clang::Expr *name) -> llvm::Expected<Lifetime> {
        if (name) {
          Lifetime lifetime;
          if (llvm::Error err = LifetimeFromName(name).moveInto(lifetime)) {
            return std::move(err);
          }
          return lifetime;
        }

        // As a special-case, lifetime is always inferred for the `this`
        // parameter for destructors. The obvious lifetime is definitionally
        // correct in this case: the object must be valid for the duration
        // of the call, or else the behavior is undefined. So we can infer
        // safely even if elision is disabled.
        if (!elision_enabled && func->getDeclName().getNameKind() !=
                                    clang::DeclarationName::CXXDestructorName) {
          return llvm::createStringError(
              llvm::inconvertibleErrorCode(),
              absl::StrCat("Lifetime elision not enabled for '",
                           func->getNameAsString(), "'"));
        }

        Lifetime lifetime = Lifetime::CreateVariable();
        symbol_table.LookupLifetimeAndMaybeDeclare(lifetime);
        return lifetime;
      };
    }

    llvm::Expected<ValueLifetimes> CreateThisLifetimes(
        clang::QualType type, const clang::Expr *lifetime_name) const override {
      LifetimeFactory lifetime_factory = ParamLifetimeFactory();

      clang::QualType pointee_type = PointeeType(type);
      assert(!pointee_type.isNull());

      ValueLifetimes value_lifetimes;
      if (llvm::Error err =
              ValueLifetimes::Create(pointee_type, clang::TypeLoc(),
                                     lifetime_factory)
                  .moveInto(value_lifetimes)) {
        return std::move(err);
      }

      Lifetime object_lifetime;
      if (llvm::Error err =
              lifetime_factory(lifetime_name).moveInto(object_lifetime)) {
        return std::move(err);
      }

      return ValueLifetimes::ForPointerLikeType(
          type, ObjectLifetimes(object_lifetime, value_lifetimes));
    }

    llvm::Expected<ValueLifetimes> CreateParamLifetimes(
        clang::QualType param_type,
        clang::TypeLoc param_type_loc) const override {
      return ValueLifetimes::Create(param_type, param_type_loc,
                                    ParamLifetimeFactory());
    }

    static std::optional<Lifetime> GetSingleInputLifetime(
        const llvm::SmallVector<ValueLifetimes> &param_lifetimes,
        const std::optional<ValueLifetimes> &this_lifetimes) {
      // If we have an implicit `this` parameter, its lifetime is assigned to
      // all lifetimes in the return type.
      if (this_lifetimes.has_value()) {
        return this_lifetimes->GetPointeeLifetimes().GetLifetime();
      }

      llvm::DenseSet<Lifetime> all_input_lifetimes;
      for (const ValueLifetimes &v : param_lifetimes) {
        v.Traverse([&all_input_lifetimes](Lifetime l, Variance) {
          all_input_lifetimes.insert(l);
        });
      }

      if (all_input_lifetimes.size() == 1) {
        // If we have a single input lifetime, its lifetime is assigned to all
        // output lifetimes.
        return *all_input_lifetimes.begin();
      } else {
        // Otherwise, we don't know how to elide the output lifetime.
        return std::nullopt;
      }
    }

    llvm::Expected<ValueLifetimes> CreateReturnLifetimes(
        clang::QualType return_type, clang::TypeLoc return_type_loc,
        const llvm::SmallVector<ValueLifetimes> &param_lifetimes,
        const std::optional<ValueLifetimes> &this_lifetimes) const override {
      // TODO(veluca): adapt to lifetime elision for function pointers.

      std::optional<Lifetime> input_lifetime =
          GetSingleInputLifetime(param_lifetimes, this_lifetimes);

      return ValueLifetimes::Create(
          return_type, return_type_loc,
          [&input_lifetime,
           this](const clang::Expr *name) -> llvm::Expected<Lifetime> {
            if (name) {
              Lifetime lifetime;
              if (llvm::Error err = LifetimeFromName(name).moveInto(lifetime)) {
                return std::move(err);
              }
              return lifetime;
            }

            if (!elision_enabled) {
              return llvm::createStringError(
                  llvm::inconvertibleErrorCode(),
                  absl::StrCat("Lifetime elision not enabled for '",
                               func->getNameAsString(), "'"));
            }

            // If we have a single input lifetime, its lifetime is assigned to
            // all output lifetimes.
            if (input_lifetime.has_value()) {
              return *input_lifetime;
            } else {
              // Otherwise, we don't know how to elide the output lifetime.
              return llvm::createStringError(
                  llvm::inconvertibleErrorCode(),
                  absl::StrCat("Cannot elide output lifetimes for '",
                               func->getNameAsString(),
                               "' because it is a non-member function that "
                               "does not have "
                               "exactly one input lifetime"));
            }
          });
    }

    bool elision_enabled;
    const clang::FunctionDecl *func;
    LifetimeSymbolTable &symbol_table;
  };

  Factory factory(elision_enabled, func, symbol_table);
  return FunctionLifetimes::CreateForDecl(func, factory);
}

// llvm::Expected<FunctionLifetimes>
void GetLifetimeAnnotations(
    const clang::FunctionDecl *func,
    llvm::DenseMap<std::string, Lifetime> *symbol_table) {
  clang::SourceManager &source_manager =
      func->getASTContext().getSourceManager();
  clang::FileID file_id =
      source_manager.getFileID(func->getSourceRange().getBegin());

  return GetLifetimeAnnotationsInternal(func, *symbol_table);
}

class Lifetime {
 public:
 private:
  char lifetime;                            // annotated variables
  unordered_set<std::string> dependencies;  // non-annotated variables
  bool isStatic;
};

class LifetimeAnnotationsChecker : public MatchFinder::MatchCallback {
 public:
  void run(const MatchFinder::MatchResult &Result) override;

  void registerMatchers(MatchFinder *Finder) {
    using namespace ast_matchers;

    Finder->addMatcher(functionDecl().bind(FuncID), this);
  }

 private:
  // TODO create object for this
  llvm::DenseMap<std::string, Lifetime> variable_to_lifetime;
};

void LifetimeAnnotationsChecker::run(const MatchFinder::MatchResult &Result) {
  if (Result.SourceManager->getDiagnostics().hasUncompilableErrorOccurred())
    return;

  const auto *FuncDecl = Result.Nodes.getNodeAs<FunctionDecl>(FuncID);
  // TODO take care of templates
  if (FuncDecl->isTemplated()) return;

  debug("Found function", FuncDecl->getNameAsString());

  ASTContext *Context = Result.Context;
}

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  LifetimeAnnotationsChecker Checker;
  MatchFinder Finder;
  // TODO get all source code, then apply crubit's
  // auto &SourceManager = Finder.getASTContext().getSourceManager();

  Checker.registerMatchers(&Finder);

  return Tool.run(newFrontendActionFactory(&Finder).get());
}