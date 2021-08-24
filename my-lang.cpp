//
// Created by Gang-Ryung Uh on 8/18/21.
//
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "KaleidoscopeJIT.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

static int gettok() {
    static int LastChar = ' ';

    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;
        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.') {   // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == '#') {
        do
            LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    if (LastChar == EOF)
        return tok_eof;

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

// global
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, Value *> NamedValues;
static ExitOnError ExitOnErr;

// class and function declaration
class ExprAST;
class PrototypeAST;
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<ExprAST>  LogError(const char *Str);
static std::unique_ptr<PrototypeAST>  LogErrorP(const char *Str);

// class and function definition
class ExprAST {
public:
    virtual ~ExprAST() {}
    virtual Value *codegen() = 0; // not implemented. subclass must
    // implement
};


class NumberExprAST: public ExprAST {
    double Val;
public:
    NumberExprAST(double Val) : Val(Val) {}
    Value *codegen() {
       //return ConstantFP::get(TheContext, APFloat(Val));
       //return ConstantFP::get(Type::getDoubleTy(TheContext), APFloat(Val));
       return ConstantFP::get(Type::getDoubleTy(TheContext), APFloat(Val));
    }
};

class VariableExprAST: public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value *codegen() {
       Value *V = NamedValues[Name];
       if (!V) {
          LogError("Unknown variable name");
       }
       return V;
    }
};

class BinaryExprAST: public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op,
                  std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS):
                  Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
   Value *codegen() {
       Value *L = LHS->codegen();
       Value *R = RHS->codegen();
       if (!L || !R)
          return nullptr;

       switch (Op) {
          case '+':
             return Builder.CreateFAdd(L,R, "addtmp");
          case '-':
             return Builder.CreateFSub(L,R, "subtmp");
          case '*':
             return Builder.CreateFMul(L,R, "multmp");
          case '<':
             L = Builder.CreateFCmpULT(L,R, "cmptmp");
             // convert bool 0/1 to double 0.0 or 1.0
             return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext),
                                         "booltmp");
          default:
             LogError("invalid binary operator");
             return nullptr;
       }
    }
};

class CallExprAST: public ExprAST {
   std::string Callee;
   std::vector<std::unique_ptr<ExprAST>> Args;
public:
   CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
                : Callee(Callee), Args(std::move(Args)) {}
   Value *codegen() {
      Function *CalleeF = TheModule->getFunction(Callee);
      if (!CalleeF) {
         LogError("Unknown function referenced");
         return nullptr;
      }

      if (CalleeF->arg_size() != Args.size()) {
         LogError("Incorrect # of arguments");
         return nullptr;
      }
      std::vector<Value *> ArgsV;
      for (auto it = Args.begin(); it != Args.end(); ++it) {
         ArgsV.push_back((*it)->codegen());
         if (!ArgsV.back())
            return nullptr;
      }

      return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
   }
};

class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args)
    : Name(name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
    Function *codegen() {
       std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
       FunctionType *FT = FunctionType::get(Type::getDoubleTy(TheContext),
                                            Doubles, false);
       Function *F = Function::Create(FT, Function::ExternalLinkage, Name,
                                      TheModule.get());
       unsigned Idx = 0;
       for (auto &Arg : F->args()) {
          Arg.setName(Args[Idx++]);
       }
       return F;
    }
};

// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
                : Proto(std::move(Proto)), Body(std::move(Body)) {}
   Function *codegen() {
      Function *TheFunction = TheModule->getFunction(Proto->getName());

      if (!TheFunction) {
         TheFunction = Proto->codegen();

      }

      if (!TheFunction) {
         return nullptr;
      }

      if (!TheFunction->empty()) {
         LogError("Function cannot be redefined");
         return nullptr;
      }

      BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
      Builder.SetInsertPoint(BB);

      NamedValues.clear();
      for (auto &Arg : TheFunction->args()) {
         NamedValues[std::string(Arg.getName())] = &Arg;
      }

      Value *RetVal = Body->codegen();
      if (RetVal) {
         // Finish off the function
         Builder.CreateRet(RetVal);

         // Validate the generated code, checking for consistency
         verifyFunction(*TheFunction);

         // Optimize the function
         TheFPM->run(*TheFunction);

         return TheFunction;
      }
      else {
         TheFunction->eraseFromParent();
         return nullptr;
      }
   }
};

static int CurTok;
static int getNextToken() {
   return CurTok = gettok();
}

static std::unique_ptr<ExprAST>  LogError(const char *Str) {
   fprintf(stderr, "LogError: %s\n", Str);
   return nullptr;
}

static std::unique_ptr<PrototypeAST>  LogErrorP(const char *Str) {
   LogError(Str);
   return nullptr;
}

static std::unique_ptr<ExprAST>  ParseNumberExpr() {
   auto Result = std::make_unique<NumberExprAST>(NumVal);
   getNextToken();
   return std::move(Result);
}

static std::unique_ptr<ExprAST>  ParseParenExpr() {
   getNextToken();
   auto V = ParseExpression();
   if (!V)
      return nullptr;

   if (CurTok == ')') {
      getNextToken();
   }
   else {
      return LogError("Expecting ')'");
   }
   return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierOrCallExpr() {
   std::string IdName = IdentifierStr;

   getNextToken();
   if (CurTok == '(') {
      // function call
      getNextToken();
      std::vector<std::unique_ptr<ExprAST>> Args;
      // construct argument list
      while (true) {
         auto Arg = ParseExpression();
         if (Arg) {
            Args.push_back(std::move(Arg));
         }
         else {
            return LogError("Argument is null");
         }

         if (CurTok == ')') {
            getNextToken();
            break;
         }
         else if (CurTok == ',') {
            getNextToken();
            continue;
         }
         else {
            return LogError("Expected ')' or ',' in argument list");
         }
      }
      return std::make_unique<CallExprAST>(IdName, std::move(Args));
   }
   else {
      return std::make_unique<VariableExprAST>(IdName);
   }
}

static std::unique_ptr<ExprAST> ParsePrimary() {
   switch(CurTok) {
      case tok_identifier:
         return ParseIdentifierOrCallExpr();
      case tok_number:
         return ParseNumberExpr();
      case '(':
         return ParseParenExpr();
      default:
         return LogError("Unknown token when expecting an expression");
   }
}

static int GetTokPrecedence() {

   switch(CurTok) {
      case '<':
      case '>':
         return 10;
      case '+':
      case '-':
         return 20;
      case '*':
      case '/':
         return 40;
      default:
         return -1;
   }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS)
{
   while (true) {
      int TokPrec = GetTokPrecedence();

      if (TokPrec < ExprPrec) {
         return LHS;
      }
      else {
         int BinOp = CurTok;
         getNextToken();

         auto RHS = ParsePrimary();
         if (RHS) {
            int NextPrec = GetTokPrecedence(); // NextPrec is a lookahead
            if (TokPrec < NextPrec) {
               RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS));
               if (!RHS) {
                  return nullptr;
               }
            }
            LHS = std::make_unique<BinaryExprAST>(BinOp,
                                                  std::move(LHS),
                                                  std::move(RHS));
         }
         else
            return nullptr;
      }
   }
}

static std::unique_ptr<ExprAST> ParseExpression() {
   auto LHS = ParsePrimary();
   if (LHS) {
      return ParseBinOpRHS(0, std::move(LHS));
   }
   return nullptr;

}

static std::unique_ptr<PrototypeAST> ParsePrototype() {
   if (CurTok != tok_identifier)
      return LogErrorP("Expected function name in prototyp");

   std::string FnName = IdentifierStr;
   getNextToken();

   if (CurTok != '(')
      return LogErrorP("Expected '(' in prototype");

   // Read the list of argument names.
   std::vector<std::string> ArgNames;
   while (getNextToken() == tok_identifier)
      ArgNames.push_back(IdentifierStr);
   if (CurTok != ')')
      return LogErrorP("Expected ')' in prototype");

   getNextToken(); // eat ')'
   return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition() {

   getNextToken(); // eat def.
   auto Proto = ParsePrototype();
   if (!Proto)
      return nullptr;

   auto E = ParseExpression();
   if (E) {
      return std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
   }
   else {
      return nullptr;
   }
}

static std::unique_ptr<PrototypeAST> ParseExtern() {
   getNextToken(); // eat extern.
   return ParsePrototype();
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
   auto E = ParseExpression();
   if (E) {
      auto Proto = std::make_unique<PrototypeAST>("",
                                                  std::vector<std::string>());
      return std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
   }
   else
      return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and codegen
//===----------------------------------------------------------------------===//

static void InitializeModulePasses() {
   // Open a new context and module.
   //TheContext = std::make_unique<LLVMContext>();
   TheModule = std::make_unique<Module>("my cool jit", TheContext);
   TheModule->setDataLayout(TheJIT->getDataLayout());

   // Create a new pass manager attached to it.
   TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

   // Do simple "peephole" optimizations and bit-twiddling optzns.
   TheFPM->add(createInstructionCombiningPass());
   // Reassociate expressions.
   TheFPM->add(createReassociatePass());
   // Eliminate Common SubExpressions.
   TheFPM->add(createGVNPass());
   // Simplify the control flow graph (deleting unreachable blocks, etc).
   TheFPM->add(createCFGSimplificationPass());

   TheFPM->doInitialization();
}

static void HandleDefinition() {
   if (auto FnAST = ParseDefinition()) {
      if (auto *FnIR = FnAST->codegen()) {
         fprintf(stderr, "Read function definition:\n");
         FnIR->print(errs());
         fprintf(stderr, "\n");
      }
   } else {
      // Skip token for error recovery.
      getNextToken();
   }
}

static void HandleExtern() {
   if (auto ProtoAST = ParseExtern()) {
      if (auto *FnIR = ProtoAST->codegen()) {
         fprintf(stderr, "Read extern: \n");
         FnIR->print(errs());
         fprintf(stderr, "\n");
      }
   } else {
      // Skip token for error recovery.
      getNextToken();
   }
}

static void HandleTopLevelExpression() {
   // Evaluate a top-level expression into an anonymous function.
   if (auto FnAST = ParseTopLevelExpr()) {
      if (auto *FnIR = FnAST->codegen()) {
         fprintf(stderr, "Read top-level expression: \n");
         FnIR->print(errs());
         fprintf(stderr, "\n");

         auto H = TheJIT->addModule(std::move(TheModule));

         // Remove the anonymous expression.
         FnIR->eraseFromParent();
      }
   } else {
      // Skip token for error recovery.
      getNextToken();
   }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
   while (true) {
      fprintf(stderr, "ready> ");
      switch (CurTok) {
         case tok_eof:
            return;
         case ';': // ignore top-level semicolons.
            getNextToken();
         break;
         case tok_def:
            HandleDefinition();
            break;
         case tok_extern:
            HandleExtern();
            break;
         default:
            HandleTopLevelExpression();
            break;
      }
   }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {

   InitializeNativeTarget();
   InitializeNativeTargetAsmPrinter();
   InitializeNativeTargetAsmParser();

   // Prime the first token.
   fprintf(stderr, "ready> ");
   getNextToken();

   TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
   InitializeModulePasses();

   // Run the main "interpreter loop" now.
   MainLoop();
   TheModule->print(errs(),nullptr);
   return 0;
}








