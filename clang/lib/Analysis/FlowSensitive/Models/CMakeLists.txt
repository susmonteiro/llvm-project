add_clang_library(clangAnalysisFlowSensitiveModels
  ChromiumCheckModel.cpp
  UncheckedOptionalAccessModel.cpp
  TaintDataflowModel.cpp

  LINK_LIBS
  clangAnalysis
  clangAnalysisFlowSensitive
  clangAST
  clangASTMatchers
  clangBasic
  )
