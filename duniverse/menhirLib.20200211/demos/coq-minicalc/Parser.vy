(* Grammar for little arithmetic expressions (+-*/) *)

%{

Require Import String.

Module Ast.
Inductive expr :=
 | Var : string -> expr
 | Num : nat -> expr
 | Add : expr -> expr -> expr
 | Sub : expr -> expr -> expr
 | Mul : expr -> expr -> expr
 | Div : expr -> expr -> expr.
End Ast.

%}

%token ADD SUB MUL DIV LPAREN RPAREN EOF
%token<nat> NUM
%token<string> ID

%start<Ast.expr> parse_expr
%type<Ast.expr> p_expr
%type<Ast.expr> p_factor
%type<Ast.expr> p_atom

%%

parse_expr : p_expr EOF       { $1 }

p_atom :
  ID                          { Ast.Var $1 }
| NUM                         { Ast.Num $1 }
| LPAREN p_expr RPAREN        { $2 } 

p_expr :
| p_factor                    { $1 }
| p_expr ADD p_factor         { Ast.Add $1 $3 }
| p_expr SUB p_factor         { Ast.Sub $1 $3 }

p_factor :
| p_atom                      { $1 }
| p_factor MUL p_atom         { Ast.Mul $1 $3 }
| p_factor DIV p_atom         { Ast.Div $1 $3 }
