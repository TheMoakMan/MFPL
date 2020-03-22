/*
	moake.y

	Grammar Rules:
      - N_START -> e | N_START N_EXPR
      - N_EXPR -> N_CONST | T_IDENT | T_LPAREN N_PARENTHESIZED_EXPR T_RPAREN
      - N_CONST -> T_INTCONST | T_STRCONST | T_T | T_NIL
      - N_PARENTHESIZED_EXPR -> N_ARITHLOGIC_EXPR | N_IF_EXPR | N_LET_EXPR |
                                N_LAMBDA_EXPR | N_PRINT_EXPR | N_INPUT_EXPR |
                                N_PROGN_EXPR_OR_USERFUNCTCALL | T_EXIT
      - N_PROGN_OR_USERFUNCTCALL -> N_FUNCT_NAME N_EXPR_LIST
      - N_FUNCT_NAME -> T_PROGN | T_IDENT
      - N_ARITHLOGIC_EXPR -> N_UN_OP N_EXPR | N_BIN_OP N_EXPR N_EXPR
      - N_IF_EXPR -> T_IF N_EXPR N_EXPR N_EXPR
      - N_LET_EXPR -> T_LETSTAR T_LPAREN N_ID_EXPR_LIST T_RPAREN N_EXPR
      - N_ID_EXPR_LIST -> e | N_ID_EXPR_LIST T_LPAREN T_IDENT N_EXPR T_RPAREN
      - N_LAMBDA_EXPR -> T_LAMBDA T_LPAREN N_ID_LIST T_RPAREN N_EXPR
      - N_ID_LIST -> e | N_ID_LIST T_IDENT
      - N_PRINT_EXPR -> T_PRINT N_EXPR
      - N_INPUT_EXPR -> T_INPUT
      - N_EXPR_LIST -> N_EXPR N_EXPR_LIST | e
      - N_BIN_OP -> N_ARITH_OP | N_LOG_OP | N_REL_OP
      - N_ARITH_OP -> T_MULT | T_SUB | T_DIV | T_ADD
      - N_LOG_OP -> T_AND | T_OR
      - N_REL_OP -> T_LT | T_GT | T_LE | T_GE | T_EQ | T_NE 
      - N_UN_OP -> T_NOT 

      To create the syntax analyzer:
        flex parser.l
        bison parser.y
        g++ parser.tab.c -o parser
        parser < inputFileName
 */

%{
#include <stdio.h>
#include <stack>
#include "SymbolTable.h"

int numLines = 1;
stack<SYMBOL_TABLE> scopeStack;

void printRule(const char *, const char *);
int yyerror(const char *s);

void beginScope();
void endScope();

void multDefError();
void undefError();

bool findEntryInAnyScope(const string theName);
bool findEntryInTopScope(const string theName);

extern "C"
{
    int yyparse(void);
    int yylex(void);
    int yywrap() {return 1;}
}

%}

%union
{
  char* text;
};

/* Token Declarations */
%token  T_INTCONST T_STRCONST T_LETSTAR T_IF T_LAMBDA T_PRINT T_INPUT 
%token  T_AND T_OR T_NOT T_T T_NIL T_PROGN T_EXIT T_IDENT
%token  T_ADD T_SUB T_MULT T_DIV T_LT T_GT T_LE T_GE T_EQ T_NE T_LPAREN T_RPAREN

%type <text> T_IDENT

/* Starting Point */
%start		N_START

/* Translation rules */
%%
N_START		: // e
            { printRule("START", "epsilon"); }
            | N_START N_EXPR
            { printRule("START", "START EXPR"); printf("\n---- Completed parsing ----\n\n"); }
            ;
N_EXPR      : N_CONST
            { printRule("EXPR", "CONST"); }
            | T_IDENT
            { printRule("EXPR", "IDENT");

              if(!findEntryInAnyScope(string($1)))
                undefError();
            }
            | T_LPAREN N_PARENTHESIZED_EXPR T_RPAREN
            { printRule("EXPR", "( PARENTHESIZED_EXPR )"); }
            ;
N_CONST     : T_INTCONST
            { printRule("CONST", "INTCONST"); }
            | T_STRCONST
            { printRule("CONST", "STRCONST"); }
            | T_T
            { printRule("CONST", "t"); }
            | T_NIL
            { printRule("CONST", "nil"); }
            ;
N_PARENTHESIZED_EXPR    : N_ARITHLOGIC_EXPR
                        { printRule("PARENTHESIZED_EXPR", "ARITHLOGIC_EXPR"); }
                        | N_IF_EXPR
                        { printRule("PARENTHESIZED_EXPR", "IF_EXPR"); }
                        | N_LET_EXPR
                        { printRule("PARENTHESIZED_EXPR", "LET_EXPR"); }
                        | N_LAMBDA_EXPR
                        { printRule("PARENTHESIZED_EXPR", "LAMBDA_EXPR"); }
                        | N_PRINT_EXPR
                        { printRule("PARENTHESIZED_EXPR", "PRINT_EXPR"); }
                        | N_INPUT_EXPR
                        { printRule("PARENTHESIZED_EXPR", "INPUT_EXPR"); }
                        | N_PROGN_EXPR_OR_USERFUNCTCALL 
                        { printRule("PARENTHESIZED_EXPR", "PROGN_OR_USERFUNCTCALL"); }
                        | T_EXIT
                        { printRule("PARENTHESIZED_EXPR", "EXIT"); printf("\nBye!\n"); exit(1);}
                        ;
N_PROGN_EXPR_OR_USERFUNCTCALL   : N_FUNCT_NAME N_EXPR_LIST
                                { printRule("PROGN_OR_USERFUNCTCALL", "FUNCT_NAME EXPR_LIST"); } //EXTRA SPACE AFTER CALL IN SAMPLE
                                ;
N_FUNCT_NAME    : T_PROGN
                { printRule("FUNCT_NAME", "PROGN"); }
                | T_IDENT
                { printRule("FUNCT_NAME", "IDENT");
                  if(!findEntryInAnyScope(string($1)))
                    undefError();
                }
                ;
N_ARITHLOGIC_EXPR   : N_UN_OP N_EXPR
                    { printRule("ARITHLOGIC_EXPR", "UN_OP EXPR"); }
                    | N_BIN_OP N_EXPR N_EXPR
                    { printRule("ARITHLOGIC_EXPR", "BIN_OP EXPR EXPR"); }
                    ;
N_IF_EXPR       : T_IF N_EXPR N_EXPR N_EXPR
                { printRule("IF_EXPR", "if EXPR EXPR EXPR"); }
                ;
N_LET_EXPR      : T_LETSTAR T_LPAREN N_ID_EXPR_LIST T_RPAREN N_EXPR
                { printRule("LET_EXPR", "let* ( ID_EXPR_LIST ) EXPR"); 
                  endScope();
                }
                ;
N_ID_EXPR_LIST  : // e
                { printRule("ID_EXPR_LIST", "epsilon"); }
                | N_ID_EXPR_LIST T_LPAREN T_IDENT N_EXPR T_RPAREN
                { printRule("ID_EXPR_LIST", "ID_EXPR_LIST ( IDENT EXPR )"); 
                  
                  printf("___Adding ");
                  printf($3);
                  printf(" to symbol table\n");

                  //Add X to symbol table if not already in it
                  SYMBOL_TABLE_ENTRY indent(string($3), -1);
                  if(!scopeStack.top().addEntry(indent))
                  {
                    multDefError();
                  }

                }
                ;
N_LAMBDA_EXPR   : T_LAMBDA T_LPAREN N_ID_LIST T_RPAREN N_EXPR
                { printRule("LAMBDA_EXPR", "lambda ( ID_LIST ) EXPR"); 
                  endScope();
                }
                ;
N_ID_LIST       : // e
                { printRule("ID_LIST", "epsilon"); }
                | N_ID_LIST T_IDENT
                { printRule("ID_LIST", "ID_LIST IDENT"); 
                  
                  printf("___Adding ");
                  printf($2);
                  printf(" to symbol table\n");

                  //Add X to symbol table if not already in it
                  SYMBOL_TABLE_ENTRY indent(string($2), -1);
                  if(!scopeStack.top().addEntry(indent))
                  {
                    multDefError();
                  }
                }
                ;
N_PRINT_EXPR    : T_PRINT N_EXPR
                { printRule("PRINT_EXPR", "print EXPR"); }
                ;
N_INPUT_EXPR    : T_INPUT
                { printRule("INPUT_EXPR", "input"); }
                ;
N_EXPR_LIST     : N_EXPR N_EXPR_LIST
                { printRule("EXPR_LIST", "EXPR EXPR_LIST"); }
                | // e
                { printRule("EXPR_LIST", "epsilon"); }
                ;
N_BIN_OP        : N_ARITH_OP
                { printRule("BIN_OP", "ARITH_OP"); }
                | N_LOG_OP
                { printRule("BIN_OP", "LOG_OP"); }
                | N_REL_OP
                { printRule("BIN_OP", "REL_OP"); }
                ;
N_ARITH_OP      : T_MULT
                { printRule("ARITH_OP", "*"); }
                | T_ADD
                { printRule("ARITH_OP", "+"); }
                | T_SUB
                { printRule("ARITH_OP", "-"); }
                | T_DIV
                { printRule("ARITH_OP", "/"); }
                ;
N_LOG_OP        : T_AND
                { printRule("LOG_OP", "and"); }
                | T_OR
                { printRule("LOG_OP", "or"); }
                ;
N_REL_OP        : T_LT
                { printRule("REL_OP", "<"); }
                | T_GT
                { printRule("REL_OP", ">"); }
                | T_LE
                { printRule("REL_OP", "<="); }
                | T_GE
                { printRule("REL_OP", ">="); }
                | T_EQ
                { printRule("REL_OP", "="); }
                | T_NE
                { printRule("REL_OP", "/="); }
                ;
N_UN_OP         : T_NOT
                { printRule("UN_OP", "not"); }
                ;
%%

#include "lex.yy.c"
extern FILE *yyin;

void printRule(const char *lhs, const char *rhs)
{
  printf("%s -> %s\n", lhs, rhs);
  return;
}

int yyerror(const char *s) 
{
  printf("Line %d: %s\n", numLines, s);
  printf("\nBye!\n");
  exit(1);
}

void multDefError()
{
  printf("Line %d: Multiply defined identifier\n", numLines);
  printf("\nBye!\n");
  exit(1); 
}

void undefError()
{
  printf("Line %d: Undefined identifier\n", numLines);
  printf("\nBye!\n");
  exit(1); 
}

void beginScope()
{
  scopeStack.push(SYMBOL_TABLE());
  printf("\n___Entering new scope...\n\n");
}

void endScope()
{
  scopeStack.pop();
  printf("\n___Exiting scope...\n\n");
}

bool findEntryInAnyScope(const string theName)
{
  if(scopeStack.empty())
    return false;
  
  bool found = scopeStack.top().findEntry(theName);

  if(found)
    return true;
  else {
    // Check in the next higher scope for symbol
    SYMBOL_TABLE symbolTable = scopeStack.top();
    scopeStack.pop();
    
    found = findEntryInAnyScope(theName);
    scopeStack.push(symbolTable); // Restore the stack
    return found;
  }
}

bool findEntryInTopScope(const string theName)
{
  if(scopeStack.empty())
    return false;
  
  bool found = scopeStack.top().findEntry(theName);

  if(found)
    return true;
  
  return false;
}


int main()
{
  
  do
  {
    yyparse(); 
  } while (!feof(yyin));

  printf("\nBye!\n");
  return(0);
}