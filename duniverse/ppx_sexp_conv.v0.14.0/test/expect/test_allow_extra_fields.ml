open! Base

module Allow_extra_fields = struct
  type t = { a : int } [@@sexp.allow_extra_fields] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\024\018\219\231,\176\148\206Y\195\132\0042H\\U"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = true
                ; fields = [ "a", { optional = false; args = [ One (Implicit_var 0) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_allow_extra_fields.ml.Allow_extra_fields"
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

module Forbid_extra_fields = struct
  type t = { a : int } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\142\248\194uE\0077q\014\151\186\131R\n\213$"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields = [ "a", { optional = false; args = [ One (Implicit_var 0) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_allow_extra_fields.ml.Forbid_extra_fields"
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

module Variant_type = struct
  type t =
    | Allow_extra_fields  of { foo : int } [@sexp.allow_extra_fields]
    | Forbid_extra_fields of { bar : int }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "d\241\139\1990f06\b\n\217\001J:\030B"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = true
                ; alts                  =
                    [ ( "Allow_extra_fields"
                      , [ Fields
                            { allow_extra_fields = true
                            ; fields             =
                                [ ( "foo"
                                  , { optional = false; args = [ One (Implicit_var 0) ] }
                                  )
                                ]
                            }
                        ] )
                    ; ( "Forbid_extra_fields"
                      , [ Fields
                            { allow_extra_fields = false
                            ; fields             =
                                [ ( "bar"
                                  , { optional = false; args = [ One (Implicit_var 0) ] }
                                  )
                                ]
                            }
                        ] )
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_allow_extra_fields.ml.Variant_type"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let _ = Allow_extra_fields  { foo = 1 }
  let _ = Forbid_extra_fields { bar = 1 }
end
