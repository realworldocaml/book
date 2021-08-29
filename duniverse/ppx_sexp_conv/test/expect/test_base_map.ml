open! Base

module Key = struct
  type t = int [@@deriving sexp_grammar]
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp_grammar]

  module M (A : T) = struct
    type 'b t = A.t * 'b
  end

  let m__t_sexp_grammar = [%sexp_grammar: < for_all : 'a 'b. ('a, 'b) t > ]
end

type t = string Pair.M(Key).t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { implicit_vars = [ "string"; "Pair.m__t"; "Key.t" ]
    ; ggid          = "^DF\173\243\197\131\141\253\181\029-\19450\231"
    ; types         = [ "t", Apply (Implicit_var 1, [ Implicit_var 2; Implicit_var 0 ]) ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; apply_implicit = [ string_sexp_grammar; Pair.m__t_sexp_grammar; Key.t_sexp_grammar ]
    ; generic_group  = _the_generic_group
    ; origin         = "test_base_map.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

let _ = t_sexp_grammar

[@@@end]
