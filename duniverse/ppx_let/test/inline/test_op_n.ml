open Core
open Ppxlib

let loc = Location.none

let print_expr expr =
  Pprintast.string_of_structure [%str let () = [%e expr]] |> print_string
;;

let%expect_test "single pattern map" =
  Ppx_let_expander.expand
    ~modul:None
    Mapn
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect {|
    let () = Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY) |}]
;;

let%expect_test "single pattern map with modul" =
  Ppx_let_expander.expand
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    Mapn
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
    ~modul:None
    Mapn
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__001_ = MY_EXPR_1
      and __let_syntax__002_ = MY_EXPR_2 in
      Let_syntax.map2 __let_syntax__001_ __let_syntax__002_
        ~f:(fun (MY_PAT_1) -> fun (MY_PAT_2) -> MY_BODY) |}]
;;

let%expect_test "single pattern map open" =
  Ppx_let_expander.expand
    ~modul:None
    Mapn_open
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
    ~modul:None
    Mapn_open
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      let __let_syntax__003_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
      and __let_syntax__004_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2 in
      Let_syntax.map2 __let_syntax__003_ __let_syntax__004_
        ~f:(fun (MY_PAT_1) -> fun (MY_PAT_2) -> MY_BODY) |}]
;;

let%expect_test "quadruple pattern map" =
  Ppx_let_expander.expand
    ~modul:None
    Mapn
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
      let __let_syntax__005_ = MY_EXPR_1
      and __let_syntax__006_ = MY_EXPR_2
      and __let_syntax__007_ = MY_EXPR_3
      and __let_syntax__008_ = MY_EXPR_4 in
      Let_syntax.map4 __let_syntax__005_ __let_syntax__006_ __let_syntax__007_
        __let_syntax__008_
        ~f:(fun (MY_PAT_1) ->
              fun (MY_PAT_2) ->
                fun (SUB_PATTERN_1, SUB_PATTERN_2) -> fun (MY_PAT_4) -> MY_BODY) |}]
;;

let%expect_test "quadruple pattern bind" =
  Ppx_let_expander.expand
    ~modul:None
    Bindn
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
      let __let_syntax__009_ = MY_EXPR_1
      and __let_syntax__010_ = MY_EXPR_2
      and __let_syntax__011_ = MY_EXPR_3
      and __let_syntax__012_ = MY_EXPR_4 in
      Let_syntax.bind4 __let_syntax__009_ __let_syntax__010_ __let_syntax__011_
        __let_syntax__012_
        ~f:(fun (MY_PAT_1) ->
              fun (MY_PAT_2) ->
                fun (SUB_PATTERN_1, SUB_PATTERN_2) -> fun (MY_PAT_4) -> MY_BODY) |}]
;;
