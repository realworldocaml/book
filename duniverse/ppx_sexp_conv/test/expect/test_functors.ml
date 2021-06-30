open! Base

module Maybe = struct
  type 'a t = 'a option [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "option" ]
      ; ggid          = "j\132);\135qH\158\135\222H\001\007\004\158\218"
      ; types         =
          [ "t", Explicit_bind ([ "a" ], Apply (Implicit_var 0, [ Explicit_var 0 ])) ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ option_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_functors.ml.Maybe"
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

module Make (T : sig
    type 'a t [@@deriving sexp_grammar]
  end) =
struct
  [@@@warning "-37"]

  type 'a t = T of 'a T.t u

  and 'a u = U of 'a T.t t Maybe.t [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)
      , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) )
    =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "T.t"; "Maybe.t" ]
      ; ggid          = "\245\184\243\180\181_5t\027u6u\233p#\158"
      ; types         =
          [ ( "t"
            , Explicit_bind
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts                  =
                        [ ( "T"
                          , [ One
                                (Apply
                                   ( Recursive "u"
                                   , [ Apply (Implicit_var 0, [ Explicit_var 0 ]) ] ))
                            ] )
                        ]
                    } ) )
          ; ( "u"
            , Explicit_bind
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts                  =
                        [ ( "U"
                          , [ One
                                (Apply
                                   ( Implicit_var 1
                                   , [ Apply
                                         ( Recursive "t"
                                         , [ Apply (Implicit_var 0, [ Explicit_var 0 ]) ]
                                         )
                                     ] ))
                            ] )
                        ]
                    } ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ T.t_sexp_grammar; Maybe.t_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_functors.ml.Make"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    and (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("u", _the_group)
    in
    t_sexp_grammar, u_sexp_grammar
  ;;

  let _ = t_sexp_grammar
  and _ = u_sexp_grammar

  [@@@end]

  type 'a v = V of 'a t [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a v) -> ()

  let (v_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "t" ]
      ; ggid          = "W\019\225!\031\181\213k\190\002\145\212\228\251\207#"
      ; types         =
          [ ( "v"
            , Explicit_bind
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts = [ "V", [ One (Apply (Implicit_var 0, [ Explicit_var 0 ])) ] ]
                    } ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ t_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_functors.ml.Make"
      }
    in
    let (v_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("v", _the_group)
    in
    v_sexp_grammar
  ;;

  let _ = v_sexp_grammar

  [@@@end]
end

module T1 = Make (Maybe)
module T2 = Make (T1)

type t = int T2.t * int T1.t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { implicit_vars = [ "int"; "T2.t"; "T1.t" ]
    ; ggid          = "\023\203\177!5(\\B1\167\214\007S\000\134B"
    ; types         =
        [ ( "t"
          , List
              [ One (Apply (Implicit_var 1, [ Implicit_var 0 ]))
              ; One (Apply (Implicit_var 2, [ Implicit_var 0 ]))
              ] )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; apply_implicit = [ int_sexp_grammar; T2.t_sexp_grammar; T1.t_sexp_grammar ]
    ; generic_group  = _the_generic_group
    ; origin         = "test_functors.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

let _ = t_sexp_grammar

[@@@end]
