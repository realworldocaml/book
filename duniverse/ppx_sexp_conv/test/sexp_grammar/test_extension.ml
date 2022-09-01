open! Base

(* Not sure how much people will want to use this, considering that the input is more
   complicated and specific than the output, but they have it. *)
module type S = sig
  val t_sexp_grammar : [%sexp_grammar: int Map.M(String).t]
end

module _ (M : S) : sig
  val t_sexp_grammar : int Map.M(String).t Sexplib0.Sexp_grammar.t [@@warning "-32"]
end =
  M

(* The grammar is illegible, so just make sure it builds. *)

let (_ : _ Sexplib0.Sexp_grammar.t) = [%sexp_grammar: int Map.M(String).t]

(* This used to give a compilation error. *)
let (_ : _ Sexplib0.Sexp_grammar.t) = [%sexp_grammar: _ list]
