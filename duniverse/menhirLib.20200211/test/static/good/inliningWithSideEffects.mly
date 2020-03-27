(* This grammar tests whether %inline affects the order in which
   semantic actions are executed. *)

(* If neither [a] nor [b] are marked %inline, then this program
   should print "Reducing A" followed with "Reducing B". *)

(* If [a] is marked %inline and [b] is not, then this program
   should print "Reducing B" followed with "Reducing A". This is
   unavoidable -- it follows from the meaning of %inline. *)

(* Similarly, if [b] is marked %inline and [a] is not,
   then there is no choice. *)

(* If both [a] and [b] are marked %inline, however,
   then the behavior of this program is unspecified.
   As of 2018/09/18, the order is indeed reversed:
   this program prints "Reducing B" first. *)

%start<unit> main
%token EOF

%%

main:
  a b EOF {}

%inline a:
  { Printf.printf "Reducing A\n%!" }

%inline b:
  { Printf.printf "Reducing B\n%!" }

%%

let () =
  let dummy = Lexing.from_string "" in
  main (fun _lexbuf -> EOF) dummy

(*
menhir inliningWithSideEffects.mly
/usr/bin/env ocaml inliningWithSideEffects.ml
*)
