open! Base

open struct
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = int_sexp_grammar
  let _ = t_sexp_grammar

  [@@@end]
end

type nonrec t = t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()
let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = t_sexp_grammar
let _ = t_sexp_grammar

[@@@end]
