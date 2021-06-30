open Base

module Variable_never_used = struct
  type t = { foo : 'a. int } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\024\152\133\012~\187\175\166\b\177\175\029\184\128m\128"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields             =
                    [ "foo", { optional = false; args = [ One (Implicit_var 0) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_first_class_polymorphism.ml.Variable_never_used"
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

module Variable_used = struct
  type t = { foo : 'a. 'a option } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "option" ]
      ; ggid          = "\140\253\217\243\196\191\188CD'Q\192\007\000yu"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields             =
                    [ ( "foo"
                      , { optional = false
                        ; args     = [ One (Apply (Implicit_var 0, [ Union [] ])) ]
                        } )
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ option_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_first_class_polymorphism.ml.Variable_used"
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
