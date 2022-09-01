open! Base

type t = (int[@sexp.opaque]) list [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
  { untyped =
      Lazy (lazy (list_sexp_grammar Sexplib0.Sexp_conv.opaque_sexp_grammar).untyped)
  }
;;

let _ = t_sexp_grammar

[@@@end]
