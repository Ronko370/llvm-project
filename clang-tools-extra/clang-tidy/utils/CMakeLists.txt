set(LLVM_LINK_COMPONENTS
  FrontendOpenMP
  Support
  )

add_clang_library(clangTidyUtils
  Aliasing.cpp
  ASTUtils.cpp
  DeclRefExprUtils.cpp
  ExceptionAnalyzer.cpp
  ExprSequence.cpp
  FileExtensionsUtils.cpp
  FixItHintUtils.cpp
  HeaderGuard.cpp
  IncludeInserter.cpp
  IncludeSorter.cpp
  JsonUtils.cpp
  LexerUtils.cpp
  NamespaceAliaser.cpp
  OptionsUtils.cpp
  RenamerClangTidyCheck.cpp
  TransformerClangTidyCheck.cpp
  TypeTraits.cpp
  UsingInserter.cpp

  LINK_LIBS
  clangTidy

  DEPENDS
  omp_gen
  )

clang_target_link_libraries(clangTidyUtils
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangSema
  clangTransformer
  )
