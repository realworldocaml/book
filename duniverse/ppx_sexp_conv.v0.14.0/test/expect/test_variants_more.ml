open Base

[@@@warning "-37"]

module Nested_inside_variant = struct
  type t = A of [ `A of int ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "Z\241\230\155\202\128I6<#U\238\187\226\131,"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = true
                ; alts                  =
                    [ ( "A"
                      , [ One
                            (Variant
                               { ignore_capitalization = false
                               ; alts                  = [ "A", [ One (Implicit_var 0) ] ]
                               })
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
      ; origin         = "test_variants_more.ml.Nested_inside_variant"
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

module Nested_inside_record = struct
  type t = { a : [ `A of int ] } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\001\215\152\002\244\149\139\179d\bwc\181\223W\187"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields             =
                    [ ( "a"
                      , { optional = false
                        ; args     =
                            [ One
                                (Variant
                                   { ignore_capitalization = false
                                   ; alts = [ "A", [ One (Implicit_var 0) ] ]
                                   })
                            ]
                        } )
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_variants_more.ml.Nested_inside_record"
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
