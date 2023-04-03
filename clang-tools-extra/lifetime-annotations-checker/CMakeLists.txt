set(LLVM_LINK_COMPONENTS support)

add_clang_executable(lifetime-annotations-checker
  LifetimeAnnotationsChecker.cpp
  )
target_link_libraries(lifetime-annotations-checker
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )
