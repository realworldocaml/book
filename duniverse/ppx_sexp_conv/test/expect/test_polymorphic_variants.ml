open Base

[@@@warning "-37"]

module Nullary = struct
  type 'a t =
    [ `A
    | `B
    ]
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = []
      ; ggid          = "\133a\241\019; \198\184U\181\220#\191\190\200\b"
      ; types         =
          [ ( "t"
            , Explicit_bind
                ( [ "a" ]
                , Variant { ignore_capitalization = false; alts = [ "A", []; "B", [] ] }
                ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = []
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_variants.ml.Nullary"
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

module With_arguments = struct
  module With_sexp = struct
    type t =
      [ `A of int * int
      | `B of string
      ]
    [@@deriving sexp_of]
  end

  type t =
    [ `A of int * int
    | `B of string
    ]
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int"; "string" ]
      ; ggid          = "\196t P\169\167\173C\251\132\141N\003n \132"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = false
                ; alts                  =
                    [ "A", [ One (List [ One (Implicit_var 0); One (Implicit_var 0) ]) ]
                    ; "B", [ One (Implicit_var 1) ]
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar; string_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_variants.ml.With_arguments"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  open Expect_test_helpers_core

  let%expect_test _ =
    print_s (With_sexp.sexp_of_t (`A (1, 2)));
    print_s (With_sexp.sexp_of_t (`B "foo"));
    [%expect {|
      (A (1 2))
      (B foo) |}]
  ;;
end

module Sexp_list = struct
  module With_sexp = struct
    type t =
      [ `Int           of int
      | `List          of int list
      | `Sexp_dot_list of int list [@sexp.list]
      | `Sexp_list     of int sexp_list [@warning "-3"]
      ]
    [@@deriving sexp]
  end

  type t =
    [ `Int           of int
    | `List          of int list
    | `Sexp_dot_list of int list [@sexp.list]
    | `Sexp_list     of int sexp_list [@warning "-3"]
    ]
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int"; "list" ]
      ; ggid          = "\221\2240I,\229H~\212(;\201\127\159rK"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = false
                ; alts                  =
                    [ "Int"          , [ One (Implicit_var 0) ]
                    ; "List"         , [ One (Apply (Implicit_var 1, [ Implicit_var 0 ])) ]
                    ; "Sexp_dot_list", [ Many (Implicit_var 0) ]
                    ; "Sexp_list"    , [ Many (Implicit_var 0) ]
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar; list_sexp_grammar ]
      ; generic_group  = _the_generic_group
      ; origin         = "test_polymorphic_variants.ml.Sexp_list"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let (T : (With_sexp.t, t) Type_equal.t) = T

  open Expect_test_helpers_core

  let%expect_test _ =
    print_s (With_sexp.sexp_of_t (`Int 1));
    List.iter [ []; [ 1 ]; [ 1; 2 ] ] ~f:(fun l ->
      print_s (With_sexp.sexp_of_t (`List          l ));
      print_s (With_sexp.sexp_of_t (`Sexp_dot_list l ));
      print_s (With_sexp.sexp_of_t (`Sexp_list     l)));
    [%expect
      {|
      (Int 1)
      (List ())
      (Sexp_dot_list)
      (Sexp_list)
      (List (1))
      (Sexp_dot_list 1)
      (Sexp_list 1)
      (List (1 2))
      (Sexp_dot_list 1 2)
      (Sexp_list 1 2) |}]
  ;;
end
