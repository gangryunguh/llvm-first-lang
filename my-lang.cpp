//
// Created by Gang-Ryung Uh on 8/18/21.
//
#include <string>
#include <iostream>
#include <vector>

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

class ExprAST {
public:
    virtual ~ExprAST() {}
};

class NumberExprAST: public ExprAST {
    double Val;
public:
    NumberExprAST(double Val) : Val(Val) {}
};

class VariableExprAST: public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
};

class BinaryExprAST: public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op,
                  std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS):
                  Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

class CallExprAST: public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
                : Callee(Callee), Args(std::move(Args)) {}
};

class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args)
    : Name(name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
                : Proto(std::move(Proto)), Body(std::move(Body)) {}
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
            Args.push_back(Arg);
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
#if 0
int main() {
    while (true) {
        int tok = gettok();
        std::cout << "got token: " << tok << std::endl;
    }
}
#endif // 0








