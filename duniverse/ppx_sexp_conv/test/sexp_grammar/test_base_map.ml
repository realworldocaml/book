open! Base

module type S = sig
  type t [@@deriving sexp_grammar]
end

module Key = struct
  type t = int [@@deriving sexp_grammar]
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp_grammar]

  module M (A : T) = struct
    type 'b t = A.t * 'b
  end

  let m__t_sexp_grammar (type a) (module Key : S with type t = a) v_sexp_grammar =
    t_sexp_grammar Key.t_sexp_grammar v_sexp_grammar
  ;;
end

type t = string Pair.M(Key).t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
  { untyped =
      Lazy (lazy (Pair.m__t_sexp_grammar (module Key) string_sexp_grammar).untyped)
  }
;;

let _ = t_sexp_grammar

[@@@end]
