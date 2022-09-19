open Core
open Ppxlib

let loc = Location.none

let print_expr expr =
  Pprintast.string_of_structure [%str let () = [%e expr]] |> print_string
;;

let%expect_test "single pattern map" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect {|
    let () = Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY) |}]
;;

let%expect_test "single pattern map with modul" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () = X.Let_syntax.Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY) |}]
;;

let%expect_test "double pattern map" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__003_ = MY_EXPR_1
      and __let_syntax__004_ = MY_EXPR_2 in
      Let_syntax.map2 __let_syntax__003_ __let_syntax__004_
        ~f:(fun (MY_PAT_1) -> fun (MY_PAT_2) -> MY_BODY) |}]
;;

let%expect_test "single pattern map open" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n_open
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
        ~f:(fun (MY_PAT_1) -> MY_BODY) |}]
;;

let%expect_test "double pattern map open" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n_open
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__008_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
      and __let_syntax__009_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2 in
      Let_syntax.map2 __let_syntax__008_ __let_syntax__009_
        ~f:(fun (MY_PAT_1) -> fun (MY_PAT_2) -> MY_BODY) |}]
;;

let%expect_test "quadruple pattern map" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2
      and SUB_PATTERN_1, SUB_PATTERN_2 = MY_EXPR_3
      and MY_PAT_4 = MY_EXPR_4 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__012_ = MY_EXPR_1
      and __let_syntax__013_ = MY_EXPR_2
      and __let_syntax__014_ = MY_EXPR_3
      and __let_syntax__015_ = MY_EXPR_4 in
      Let_syntax.map4 __let_syntax__012_ __let_syntax__013_ __let_syntax__014_
        __let_syntax__015_
        ~f:(fun (MY_PAT_1) ->
              fun (MY_PAT_2) ->
                fun (SUB_PATTERN_1, SUB_PATTERN_2) -> fun (MY_PAT_4) -> MY_BODY) |}]
;;

let%expect_test "quadruple pattern bind" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2
      and SUB_PATTERN_1, SUB_PATTERN_2 = MY_EXPR_3
      and MY_PAT_4 = MY_EXPR_4 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__020_ = MY_EXPR_1
      and __let_syntax__021_ = MY_EXPR_2
      and __let_syntax__022_ = MY_EXPR_3
      and __let_syntax__023_ = MY_EXPR_4 in
      Let_syntax.bind4 __let_syntax__020_ __let_syntax__021_ __let_syntax__022_
        __let_syntax__023_
        ~f:(fun (MY_PAT_1) ->
              fun (MY_PAT_2) ->
                fun (SUB_PATTERN_1, SUB_PATTERN_2) -> fun (MY_PAT_4) -> MY_BODY) |}]
;;
