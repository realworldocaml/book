open! Base

[@@@warning "-37"]

module Records_we_can_handle = struct
  type ('a, 'b) t =
    { not_first_class_tick_a : 'a
    ; b                      : 'a. 'b
    ; int                    : 'a. int
    ; either                 : 'a. 'a option
    ; polymorphic_variant    : 'a. [ `A of 'a | `B of 'b | `Int of int ]
    }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int"; "option" ]
      ; ggid          = "\018M5`u:\186\223\136?/\182\187\135R\029"
      ; types         =
          [ ( "t"
            , Explicit_bind
                ( [ "a"; "b" ]
                , Record
                    { allow_extra_fields = false
                    ; fields             =
                        [ ( "not_first_class_tick_a"
                          , { optional = false; args = [ One (Explicit_var 0) ] } )
                        ; "b"  , { optional = false; args = [ One (Explicit_var 1) ] }
                        ; "int", { optional = false; args = [ One (Implicit_var 0) ] }
                        ; ( "either"
                          , { optional = false
                            ; args     = [ One (Apply (Implicit_var 1, [ Union [] ])) ]
                            } )
                        ; ( "polymorphic_variant"
                          , { optional = false
                            ; args     =
                                [ One
                                    (Variant
                                       { ignore_capitalization = false
                                       ; alts                  =
                                           [ "A"  , [ One (Union []) ]
                                           ; "B"  , [ One (Explicit_var 1) ]
                                           ; "Int", [ One (Implicit_var 0) ]
                                           ]
                                       })
                                ]
                            } )
                        ]
                    } ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar; option_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_record_fields.ml.Records_we_can_handle"
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

module Impossible_record = struct
  type t = { a : 'a. 'a } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = []
      ; ggid          = "&\224\178\151\b>\2179\022\203\130~i\190G\245"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields = [ "a", { optional = false; args = [ One (Union []) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = []
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_record_fields.ml.Impossible_record"
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

module Inline_record = struct
  type 'a t =
    | Non_poly of { a : 'a }
    | Poly     of { a : 'a. 'a }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = []
      ; ggid          = "\217\0037(\136\214}@\029 \130x\242\146\137\179"
      ; types         =
          [ ( "t"
            , Explicit_bind
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts                  =
                        [ ( "Non_poly"
                          , [ Fields
                                { allow_extra_fields = false
                                ; fields             =
                                    [ ( "a"
                                      , { optional = false
                                        ; args     = [ One (Explicit_var 0) ]
                                        } )
                                    ]
                                }
                            ] )
                        ; ( "Poly"
                          , [ Fields
                                { allow_extra_fields = false
                                ; fields             =
                                    [ "a", { optional = false; args = [ One (Union []) ] }
                                    ]
                                }
                            ] )
                        ]
                    } ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = []
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_record_fields.ml.Inline_record"
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
