%{
#include <iostream>
using namespace std;

int lines = 0;
int tokenNo = 0;
%}
		              /* ---- DEFINITIONS ---- */
%option noyywrap
DIG  		[0-9]
NZ_DIGIT    [1-9]
ALPHA       [a-zA-Z]
ALPHANUM    [a-zA-Z0-9]
ALPHANUM_W_SPACE [a-zA-Z0-9 ]

INT         [+-]?{DIG}+
STR         \"[^\n]*\"
LET         "let*"
IF          "if"
LBDA        "lambda"
PRNT        "print"
INPT        "input"
AND         "and"
OR          "or"
NOT         "not"
T           "t"
NIL         "nil"
PROGN       "progn"
EXIT        "exit"
IDN         ({ALPHA}|"_")({ALPHANUM}|"_")*
ADD         "+"
SUB         "-"
MULT        "*"
DIV         "/"
LT          "<"
GT          ">"
LE          "<="
GE          ">="
EQ          "="
NE          "/="
COM         ";"[^\n]*
LPAR        "("
RPAR        ")"





%%  		              /* ---- REGULAR EXPRESSIONS ---- */

[ \t]   ;
\n      { numLines++; }
{INT}   { cout << "TOKEN: INTCONST  LEXEME: " << yytext << endl; return T_INTCONST;}
{STR}   { cout << "TOKEN: STRCONST  LEXEME: " << yytext << endl; return T_STRCONST;}
{LET}   { cout << "TOKEN: LETSTAR   LEXEME: " << yytext << endl; 
		  beginScope();
		  return T_LETSTAR;}
{IF}    { cout << "TOKEN: IF        LEXEME: " << yytext << endl; return T_IF;}
{LBDA}  { cout << "TOKEN: LAMBDA    LEXEME: " << yytext << endl; 
		  beginScope();
		  return T_LAMBDA;
		}
{PRNT}  { cout << "TOKEN: PRINT     LEXEME: " << yytext << endl; return T_PRINT;}
{INPT}  { cout << "TOKEN: INPUT     LEXEME: " << yytext << endl; return T_INPUT;}
{AND}   { cout << "TOKEN: AND       LEXEME: " << yytext << endl; return T_AND;}
{OR}    { cout << "TOKEN: OR        LEXEME: " << yytext << endl; return T_OR;}
{NOT}   { cout << "TOKEN: NOT       LEXEME: " << yytext << endl; return T_NOT;}
{T}     { cout << "TOKEN: T         LEXEME: " << yytext << endl; return T_T;}
{NIL}   { cout << "TOKEN: NIL       LEXEME: " << yytext << endl; return T_NIL;}
{PROGN} { cout << "TOKEN: PROGN     LEXEME: " << yytext << endl; return T_PROGN;}
{EXIT}  { cout << "TOKEN: EXIT      LEXEME: " << yytext << endl; return T_EXIT;}
{IDN}   { 
	        cout << "TOKEN: IDENT     LEXEME: " << yytext << endl;
		    yylval.text = strdup(yytext);
		    return T_IDENT; 
		}
{ADD}   { cout << "TOKEN: ADD       LEXEME: " << yytext << endl; return T_ADD;}
{SUB}   { cout << "TOKEN: SUB       LEXEME: " << yytext << endl; return T_SUB;}
{MULT}  { cout << "TOKEN: MULT      LEXEME: " << yytext << endl; return T_MULT;}
{DIV}   { cout << "TOKEN: DIV       LEXEME: " << yytext << endl; return T_DIV;}
{LT}    { cout << "TOKEN: LT        LEXEME: " << yytext << endl; return T_LT;}
{GT}    { cout << "TOKEN: GT        LEXEME: " << yytext << endl; return T_GT;}
{LE}    { cout << "TOKEN: LE        LEXEME: " << yytext << endl; return T_LE;}
{GE}    { cout << "TOKEN: GE        LEXEME: " << yytext << endl; return T_GE;}
{EQ}    { cout << "TOKEN: EQ        LEXEME: " << yytext << endl; return T_EQ;}
{NE}    { cout << "TOKEN: NE        LEXEME: " << yytext << endl; return T_NE;}
{LPAR}  { cout << "TOKEN: LPAREN    LEXEME: " << yytext << endl; return T_LPAREN;}
{RPAR}  { cout << "TOKEN: RPAREN    LEXEME: " << yytext << endl; return T_RPAREN;}
{COM}   ;
.       ; //{ cout << "TOKEN: UNKNOWN   LEXEME: " << yytext << endl; }

%%  		              /* ---- EPILOGUE ---- */

