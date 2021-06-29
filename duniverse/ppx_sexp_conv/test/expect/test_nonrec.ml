open! Base

open  struct
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types         = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_nonrec.ml"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

type nonrec t = t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { implicit_vars = [ "t" ]
    ; ggid          = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
    ; types         = [ "t", Implicit_var 0 ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; apply_implicit = [ t_sexp_grammar ]
    ; generic_group  = _the_generic_group
    ; origin         = "test_nonrec.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

let _ = t_sexp_grammar

[@@@end]
