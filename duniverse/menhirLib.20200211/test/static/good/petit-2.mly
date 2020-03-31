(* This file defines the set of tokens produced by the lexer and
   exploited by the parser. *)

%token <int32> INTCONST
%token <bool> BOOLCONST
%token <string Location.t> ID
%token PLUS MINUS TIMES SLASH AND OR NOT LT LE GT GE EQ NE
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLONEQ SEMICOLON COLON DOT
%token PROGRAM BEGIN END IF THEN ELSE WHILE DO PROCEDURE FUNCTION VAR
%token NEW READLN WRITE WRITELN
%token INTEGER BOOLEAN ARRAY OF

%%

