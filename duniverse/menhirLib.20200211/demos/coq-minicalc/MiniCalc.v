
Require Import Parser Lexer List String PeanoNat.
Import MenhirLibParser.Inter.
Import ListNotations.
Open Scope string_scope.

(** Lexer + Parser for little arithmetic expressions *)

Definition string2expr s :=
  match option_map (Parser.parse_expr 50) (Lexer.lex_string s) with
  | Some (Parsed_pr f _) => Some f
  | _ => None
  end.

(** Let's print back our little expressions *)

Module Print.

Definition pr_digit n :=
  match n with
  | 0 => "0"
  | 1 => "1"
  | 2 => "2"
  | 3 => "3"
  | 4 => "4"
  | 5 => "5"
  | 6 => "6"
  | 7 => "7"
  | 8 => "8"
  | _ => "9"
  end.

Fixpoint prnat_loop n k :=
  match k with
  | 0 => ""
  | S k =>
    match n/10 with
    | 0 => pr_digit n
    | q => prnat_loop q k ++ pr_digit (n mod 10)
    end
  end.

Definition prnat n := prnat_loop n (S n).

Fixpoint pr_expr e :=
 match e with
 | Ast.Num n => prnat n
 | Ast.Var x => x
 | Ast.Add e e' => "(" ++ pr_expr e ++ "+" ++ pr_expr e' ++ ")"
 | Ast.Sub e e' => "(" ++ pr_expr e ++ "-" ++ pr_expr e' ++ ")"
 | Ast.Mul e e' => "(" ++ pr_expr e ++ "*" ++ pr_expr e' ++ ")"
 | Ast.Div e e' => "(" ++ pr_expr e ++ "/" ++ pr_expr e' ++ ")"
 end.

End Print.

Definition example := "12 + 34*x / (48+y)".
Definition expected_reprint := "(12+((34*x)/(48+y)))".

Compute option_map Print.pr_expr (string2expr example).

Definition check :=
  match option_map Print.pr_expr (string2expr example) with
  | Some s => if string_dec s expected_reprint then "OK" else "KO"
  | None => "KO"
  end.

(** This should display "OK" *)

Compute check.
