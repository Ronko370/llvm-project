//===--- MoveConstArgCheck.cpp - clang-tidy -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include "MoveConstArgCheck.h"

#include "clang/Lex/Lexer.h"
#include "../utils/JsonUtils.h"

using namespace clang::ast_matchers;
using namespace llvm::json;
using namespace clang::tidy::utils;

namespace clang::tidy::performance {

auto MoveCallMatcher =
      callExpr(callee(functionDecl(hasName("::std::move"))), argumentCountIs(1))
          .bind("call-move");

auto MoveCallMatcherNonStd =
      callExpr(allOf(
          callee(functionDecl(hasName("::std::move"))),
          unless(hasAncestor(isInStdNamespace())),
          argumentCountIs(1)))
          .bind("call-move");

auto ConstTypeParmMatcher =
    qualType(references(isConstQualified())).bind("invocation-parm-type");
auto RValueTypeParmMatcher =
    qualType(rValueReferenceType()).bind("invocation-parm-type");
// Matches respective ParmVarDecl for a CallExpr or CXXConstructExpr.

auto ArgumentWithParamMatcher = forEachArgumentWithParam(
    MoveCallMatcher, parmVarDecl(anyOf(hasType(ConstTypeParmMatcher),
                                        hasType(RValueTypeParmMatcher)))
                          .bind("invocation-parm"));

auto ArgumentWithParamMatcherNonStd = forEachArgumentWithParam(
    MoveCallMatcherNonStd, parmVarDecl(anyOf(hasType(ConstTypeParmMatcher),
                                        hasType(RValueTypeParmMatcher)))
                          .bind("invocation-parm"));

// Matches respective types of arguments for a CallExpr or CXXConstructExpr
// and it works on calls through function pointers as well.
auto ArgumentWithParamTypeMatcher = forEachArgumentWithParamType(
    MoveCallMatcher, anyOf(ConstTypeParmMatcher, RValueTypeParmMatcher));

auto ArgumentWithParamTypeMatcherNonStd = forEachArgumentWithParamType(
    MoveCallMatcherNonStd, anyOf(ConstTypeParmMatcher, RValueTypeParmMatcher));

auto InvocationMatcher = invocation(anyOf(ArgumentWithParamMatcher, 
  ArgumentWithParamTypeMatcher))
          .bind("receiving-expr");

auto InvocationMatcherNonStd = invocation(anyOf(ArgumentWithParamMatcherNonStd, 
  ArgumentWithParamTypeMatcherNonStd))
          .bind("receiving-expr");


AST_MATCHER_P(Decl, CallsStdMoveIndirectly, int, N) {
  if(N > 100) {
    return false;
  }
/*  
  // Check if the current function is 'std::move()'.
  auto MoveMatcher =
    (functionDecl(hasName("::std::move")));

  if (MoveMatcher.matches(Node, Finder, Builder)) {
    return true;
  }
*/


  // Recursively search for function calls in the current function's body.
  auto IndirectCallMatcher =
    decl(anyOf(
      hasDescendant(InvocationMatcher),
      hasDescendant(callExpr(callee(decl((
        CallsStdMoveIndirectly(N+1))))))));

  return IndirectCallMatcher.matches(Node, Finder, Builder);
}

static void replaceCallWithArg(const CallExpr *Call, DiagnosticBuilder &Diag,
                               const SourceManager &SM,
                               const LangOptions &LangOpts) {
  const Expr *Arg = Call->getArg(0);

  CharSourceRange BeforeArgumentsRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(Call->getBeginLoc(), Arg->getBeginLoc()),
      SM, LangOpts);
  CharSourceRange AfterArgumentsRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(Call->getEndLoc(),
                                    Call->getEndLoc().getLocWithOffset(1)),
      SM, LangOpts);

  if (BeforeArgumentsRange.isValid() && AfterArgumentsRange.isValid()) {
    Diag << FixItHint::CreateRemoval(BeforeArgumentsRange)
         << FixItHint::CreateRemoval(AfterArgumentsRange);
  }
}

void MoveConstArgCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "CheckTriviallyCopyableMove", CheckTriviallyCopyableMove);
  Options.store(Opts, "CheckMoveToConstRef", CheckMoveToConstRef);
}

void MoveConstArgCheck::registerMatchers(MatchFinder *Finder) {
  auto MoveIndirectCallMatcher =
      callExpr(allOf(
        callee(functionDecl(hasName("::std::move"))),
        unless(hasAncestor(decl(isInStdNamespace()))),
        callee(decl(CallsStdMoveIndirectly(0)))
    )).bind("indirect-caller");

  Finder->addMatcher(MoveIndirectCallMatcher, this);
  Finder->addMatcher(MoveCallMatcherNonStd, this);
  Finder->addMatcher(InvocationMatcherNonStd, this);
  
    /* 
    auto IndirectMoveMatcher = callExpr(allOf(
      unless(hasAncestor(decl(isInStdNamespace()))),
      hasDescendant(callExpr(callee(functionDecl(hasName("::std::move"))), argumentCountIs(1)).bind("call-move-indirect")))).bind("indirect-caller");
    
    */
  
   // Finder->addMatcher(callExpr(callee(decl(CallsStdMoveIndirectly(0)))).bind("indirect-caller"), this);
}

bool IsRValueReferenceParam(const Expr *Invocation,
                            const QualType *InvocationParmType,
                            const Expr *Arg) {
  if (Invocation && (*InvocationParmType)->isRValueReferenceType() &&
      Arg->isLValue()) {
    if (!Invocation->getType()->isRecordType())
      return true;
    else {
      if (const auto *ConstructCallExpr =
              dyn_cast<CXXConstructExpr>(Invocation)) {
        if (const auto *ConstructorDecl = ConstructCallExpr->getConstructor()) {
          if (!ConstructorDecl->isCopyOrMoveConstructor() &&
              !ConstructorDecl->isDefaultConstructor())
            return true;
        }
      }
    }
  }
  return false;
}

void MoveConstArgCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *CallMove = Result.Nodes.getNodeAs<CallExpr>("call-move");
  const auto *ReceivingExpr = Result.Nodes.getNodeAs<Expr>("receiving-expr");
  const auto *InvocationParm =
      Result.Nodes.getNodeAs<ParmVarDecl>("invocation-parm");
  const auto *InvocationParmType =
      Result.Nodes.getNodeAs<QualType>("invocation-parm-type");
  //const auto* CallMoveIndirect = Result.Nodes.getNodeAs<CallExpr>("call-move-indirect");
  const auto* IndirectCaller = Result.Nodes.getNodeAs<CallExpr>("indirect-caller");

/*
  if(IndirectCaller) {
    std::cerr << "Indirect caller" << std::endl;
    diag(IndirectCaller->getExprLoc(), "Some warning");
  }

  if(CallMove) {
    std::cerr << "CallMoveIndirect" << std::endl;
    diag(CallMove->getExprLoc(), "Some other warning");
  }

  printf("%p, %p, %p, %p, %p, %p\n", CallMove, ReceivingExpr, InvocationParm, InvocationParmType, IndirectCaller);
*/

//return;
  // Skipping matchers which have been matched.
  if (!IndirectCaller
      && !ReceivingExpr
      && AlreadyCheckedMoves.contains(CallMove))
    return; // TODO

  if (ReceivingExpr)
    AlreadyCheckedMoves.insert(CallMove);

  const Expr *Arg = CallMove->getArg(0);
  SourceManager &SM = Result.Context->getSourceManager();

  auto CallerForRange = IndirectCaller ? IndirectCaller : CallMove;
  CharSourceRange MoveRange =
      CharSourceRange::getCharRange(CallerForRange->getSourceRange());
  CharSourceRange FileMoveRange =
      Lexer::makeFileCharRange(MoveRange, SM, getLangOpts());
  if (!FileMoveRange.isValid())
    return;

  bool IsConstArg = Arg->getType().isConstQualified();
  bool IsTriviallyCopyable =
      Arg->getType().isTriviallyCopyableType(*Result.Context);
  std::string Diagnostic = "";

  const auto FileID = SM.getMainFileID();
  const std::string FileName = "move_stats/"
    + SM.getFileEntryForID(FileID)->getName().str() + ".json";
  llvm::Regex regex(".*/");
  llvm::SmallVector<llvm::StringRef, 1> matches;
  regex.match(FileName, &matches);
  llvm::sys::fs::create_directories(matches[0]);
  FileManager& Manager = SM.getFileManager();
  Object* Obj = readJSONFromFile(Manager, FileName);

  bool IsVariable = isa<DeclRefExpr>(Arg);
  // std::move shouldn't be removed when an lvalue wrapped by std::move is
  // passed to the function with an rvalue reference parameter.
  bool IsRVRefParam =
      IsRValueReferenceParam(ReceivingExpr, InvocationParmType, Arg);
  const auto *Var =
      IsVariable ? dyn_cast<DeclRefExpr>(Arg)->getDecl() : nullptr;

  if (IsConstArg || IsTriviallyCopyable) {
    if (const CXXRecordDecl *R = Arg->getType()->getAsCXXRecordDecl()) {
      // According to [expr.prim.lambda]p3, "whether the closure type is
      // trivially copyable" property can be changed by the implementation of
      // the language, so we shouldn't rely on it when issuing diagnostics.
      if (R->isLambda())
        return;
      // Don't warn when the type is not copyable.
      for (const auto *Ctor : R->ctors()) {
        if (Ctor->isCopyConstructor() && Ctor->isDeleted())
          return;
      }
    }

    if (!IsConstArg && IsTriviallyCopyable && !CheckTriviallyCopyableMove)
      return;

    {
      // ugly workaround
      Diagnostic = "std::move of the ";
      if(IsConstArg) {
        Diagnostic += "const ";
      }
      if(IsVariable) {
        Diagnostic += "variable " + Var->getName().str() + " ";
      } else {
        Diagnostic += "expression ";
      }
      if(IsTriviallyCopyable) {
        Diagnostic += "of the trivially-copyable type (" + std::string(Arg->getType().getAsString()) + ") ";
      }
      Diagnostic += "has no effect";
      if(!IsRVRefParam) {
        Diagnostic += "; remove std::move()";
      }
      if(IsConstArg && IsVariable && !IsTriviallyCopyable) {
        Diagnostic += " or make the variable non-const";
      }

      auto Diag = diag(FileMoveRange.getBegin(),
                       "std::move of the %select{|const }0"
                       "%select{expression|variable %5}1 "
                       "%select{|of the trivially-copyable type %6 }2"
                       "has no effect%select{; remove std::move()|}3"
                       "%select{| or make the variable non-const}4")
                  << IsConstArg << IsVariable << IsTriviallyCopyable
                  << IsRVRefParam
                  << (IsConstArg && IsVariable && !IsTriviallyCopyable) << Var
                  << Arg->getType();

      if (!IsRVRefParam)
        replaceCallWithArg(CallMove, Diag, SM, getLangOpts());
    }
    if (IsRVRefParam) {
      // Generate notes for an invocation with an rvalue reference parameter.
      const auto *ReceivingCallExpr = dyn_cast<CallExpr>(ReceivingExpr);
      const auto *ReceivingConstructExpr =
          dyn_cast<CXXConstructExpr>(ReceivingExpr);
     /*
      // Skipping the invocation which is a template instantiation.
      if ((!ReceivingCallExpr || !ReceivingCallExpr->getDirectCallee() ||
           ReceivingCallExpr->getDirectCallee()->isTemplateInstantiation()) &&
          (!ReceivingConstructExpr ||
           !ReceivingConstructExpr->getConstructor() ||
           ReceivingConstructExpr->getConstructor()->isTemplateInstantiation()))
        return;*/

      const NamedDecl *FunctionName = nullptr;
      FunctionName =
          ReceivingCallExpr
              ? ReceivingCallExpr->getDirectCallee()->getUnderlyingDecl()
              : ReceivingConstructExpr->getConstructor()->getUnderlyingDecl();

      QualType NoRefType = (*InvocationParmType)->getPointeeType();
      PrintingPolicy PolicyWithSuppressedTag(getLangOpts());
      PolicyWithSuppressedTag.SuppressTagKeyword = true;
      PolicyWithSuppressedTag.SuppressUnwrittenScope = true;
      std::string ExpectParmTypeName =
          NoRefType.getAsString(PolicyWithSuppressedTag);
      if (!NoRefType->isPointerType()) {
        NoRefType.addConst();
        ExpectParmTypeName =
            NoRefType.getAsString(PolicyWithSuppressedTag) + " &";
      }

      diag(InvocationParm->getLocation(),
           "consider changing the %ordinal0 parameter of %1 from %2 to '%3'",
           DiagnosticIDs::Note)
          << (InvocationParm->getFunctionScopeIndex() + 1) << FunctionName
          << *InvocationParmType << ExpectParmTypeName;
    }
  } else if (ReceivingExpr && CheckMoveToConstRef) {
    if ((*InvocationParmType)->isRValueReferenceType())
      return;

    auto Diag = diag(FileMoveRange.getBegin(),
                     "passing result of std::move() as a const reference "
                     "argument; no move will actually happen");

    Diagnostic = "passing result of std::move() as a const reference "
                 "argument; no move will actually happen";
    replaceCallWithArg(CallMove, Diag, SM, getLangOpts());
  }

  if(!Diagnostic.empty()) {
    if(IndirectCaller) {
      CallMove = IndirectCaller;
    }
    const auto MoveLocationPath = SM.getFileEntryForID(FullSourceLoc(CallMove->getSourceRange().getBegin(), SM).getFileID())->getName().str();

    const std::string VarID =
      (Var ? Var->getName() + "|"
          + std::to_string(SM.getSpellingLineNumber(Var->getSourceRange().getBegin()))
          + "_"
          + std::to_string(SM.getPresumedColumnNumber(Var->getSourceRange().getBegin()))
          + "-"
          + std::to_string(SM.getPresumedColumnNumber(Var->getSourceRange().getEnd()))
        : "|" 
          + std::to_string(SM.getSpellingLineNumber(CallMove->getSourceRange().getBegin()))
          + "_"
          + std::to_string(SM.getPresumedColumnNumber(CallMove->getSourceRange().getBegin())) + "-"
          + std::to_string(SM.getPresumedColumnNumber(CallMove->getSourceRange().getEnd()))).str();

    (*(*Obj->try_emplace(VarID, Object({})).first)
      .getSecond().getAsObject()
      ->try_emplace("moveNoEffect", Array({})).first)
      .getSecond().getAsArray()->push_back(Array({
        Object{Object::KV{"move_location",
          std::to_string(SM.getSpellingLineNumber(CallMove->getSourceRange().getBegin()))
          + "_"
          + std::to_string(SM.getPresumedColumnNumber(CallMove->getSourceRange().getBegin()))
          + "-"
          + std::to_string(SM.getPresumedColumnNumber(CallMove->getSourceRange().getEnd()))}},
        Object{Object::KV{"path", MoveLocationPath}},
        Object{Object::KV{"reasoning", Diagnostic}}}));

    writeJSONToFile(Manager, Obj, FileName);
  }
}

} // namespace clang::tidy::performance
