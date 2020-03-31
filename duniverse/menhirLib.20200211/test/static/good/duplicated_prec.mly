(* Message from Valentin Gatien-Baron, Sat, 09 Jan 2010. *)

(* Version of Menhir earlier than 2015/10/06 would warn that the
   declaration [%prec type_] is never useful. In fact, it is.
   This definition is duplicated by the inlining of ioption,
   and one of its copies is useful. *)

%token Eoi Rparen Lparen Colon Ident Typevar

%left type_
%left Lparen
%start <unit> expr_eoi
%%

expr_eoi: expr Eoi {}

type_expr:
| Ident ioption(delimited(Lparen, type_expr, Rparen)) {} %prec type_
| Typevar {}

expr:
| Ident {}
| expr Colon type_expr {}
| expr Lparen expr Rparen {}

