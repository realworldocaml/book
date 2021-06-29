open Base

[@@@warning "-37"]

module One_type = struct
  type t = T of int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\243A~\012\241*Zj\026)S&\127Q\231x"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = true; alts = [ "T", [ One (Implicit_var 0) ] ] }
            )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_recursive_groups.ml.One_type"
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

module Two_types = struct
  type t =
    | T_int of int
    | T_u   of u

  and u =
    | U_int of int
    | U_t   of t
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)
      , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) )
    =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\241o\231&\242\021\147\249\029+\000\245\187\240\158H"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = true
                ; alts                  =
                    [ "T_int", [ One (Implicit_var 0) ]; "T_u", [ One (Recursive "u") ] ]
                } )
          ; ( "u"
            , Variant
                { ignore_capitalization = true
                ; alts                  =
                    [ "U_int", [ One (Implicit_var 0) ]; "U_t", [ One (Recursive "t") ] ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_recursive_groups.ml.Two_types"
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
end
