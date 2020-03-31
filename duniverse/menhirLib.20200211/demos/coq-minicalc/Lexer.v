
(** Hand-written lexer for natural numbers, idents, parens and + - * / *)

Require Import BinNat Ascii String.
Require Import Parser.
Import MenhirLibParser.Inter.
Open Scope char_scope.
Open Scope bool_scope.

(** No such thing as an empty buffer, instead we use
    an infinite stream of EOF *)

CoFixpoint TheEnd : buffer := Buf_cons (EOF tt) TheEnd.

Fixpoint ntail n s :=
  match n, s with
  | 0, _ => s
  | _, EmptyString => s
  | S n, String _ s => ntail n s
  end.

(** Comparison on characters *)

Definition ascii_eqb c c' := (N_of_ascii c =? N_of_ascii c')%N.
Definition ascii_leb c c' := (N_of_ascii c <=? N_of_ascii c')%N.

Infix "=?" := ascii_eqb : char_scope.
Infix "<=?" := ascii_leb : char_scope.

Definition is_digit c := (("0" <=? c) && (c <=? "9"))%char.

Definition is_alpha c :=
  ((("a" <=? c) && (c <=? "z")) ||
   (("A" <=? c) && (c <=? "Z")) ||
   (c =? "_"))%char.

Fixpoint identsize s :=
  match s with
  | EmptyString => 0
  | String c s =>
    if is_alpha c || is_digit c then S (identsize s)
    else 0
  end.

Fixpoint readnum acc s :=
  match s with
  | String "0" s => readnum (acc*10) s
  | String "1" s => readnum (acc*10+1) s
  | String "2" s => readnum (acc*10+2) s
  | String "3" s => readnum (acc*10+3) s
  | String "4" s => readnum (acc*10+4) s
  | String "5" s => readnum (acc*10+5) s
  | String "6" s => readnum (acc*10+6) s
  | String "7" s => readnum (acc*10+7) s
  | String "8" s => readnum (acc*10+8) s
  | String "9" s => readnum (acc*10+9) s
  | _ => (acc,s)
  end.

Fixpoint lex_string_cpt n s :=
  match n with
  | 0 => None
  | S n =>
    match s with
    | EmptyString => Some TheEnd
    | String c s' =>
      match c with
      | " " => lex_string_cpt n s'
      | "009" => lex_string_cpt n s' (* \t *)
      | "010" => lex_string_cpt n s' (* \n *)
      | "013" => lex_string_cpt n s' (* \r *)
      | "(" => option_map (Buf_cons (LPAREN tt)) (lex_string_cpt n s')
      | ")" => option_map (Buf_cons (RPAREN tt)) (lex_string_cpt n s')
      | "+" => option_map (Buf_cons (ADD tt)) (lex_string_cpt n s')
      | "-" => option_map (Buf_cons (SUB tt)) (lex_string_cpt n s')
      | "*" => option_map (Buf_cons (MUL tt)) (lex_string_cpt n s')
      | "/" => option_map (Buf_cons (DIV tt)) (lex_string_cpt n s')
      | _ =>
        if is_digit c then
          let (m,s) := readnum 0 s in
          option_map (Buf_cons (NUM m)) (lex_string_cpt n s)
        else if is_alpha c then
          let k := identsize s in
          let id := substring 0 k s in
          let s := ntail k s in
          option_map (Buf_cons (ID id)) (lex_string_cpt n s)
        else None
      end
    end
  end.

Definition lex_string s := lex_string_cpt (length s) s.
