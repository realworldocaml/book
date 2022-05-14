open! Base

type ('a, _, 'b) t = 'a * 'b

and u = (string, int, float) t [@@deriving_inline sexp_grammar]

let _ = fun (_ : ('a, _, 'b) t) -> ()
let _ = fun (_ : u) -> ()

let (t_sexp_grammar :
       'a Sexplib0.Sexp_grammar.t
     -> 'b__001_ Sexplib0.Sexp_grammar.t
     -> 'b Sexplib0.Sexp_grammar.t
     -> ('a, 'b__001_, 'b) t Sexplib0.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar _'b__001__sexp_grammar _'b_sexp_grammar ->
  { untyped =
      List (Cons (_'a_sexp_grammar.untyped, Cons (_'b_sexp_grammar.untyped, Empty)))
  }
;;

let _ = t_sexp_grammar

let (u_sexp_grammar : u Sexplib0.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (t_sexp_grammar string_sexp_grammar int_sexp_grammar float_sexp_grammar).untyped)
  }
;;

let _ = u_sexp_grammar

[@@@end]
