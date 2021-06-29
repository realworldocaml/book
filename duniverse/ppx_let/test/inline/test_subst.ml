open Core
open Ppxlib

let loc = Location.none

let print_expr expr =
  Pprintast.string_of_structure [%str let () = [%e expr]] |> print_string
;;

let%expect_test "single let%sub " =
  Ppx_let_expander.expand
    ~modul:None
    Sub
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect {|
    let () = Let_syntax.sub MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY) |}]
;;

let%expect_test "single pattern sub with modul" =
  Ppx_let_expander.expand
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    Sub
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () = X.Let_syntax.Let_syntax.sub MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY) |}]
;;

let assert_fails_with_syntax_error ~f =
  try
    ignore (f ());
    assert false
  with
  | ex ->
    Location.Error.of_exn ex
    |> (fun a -> Option.value_exn a)
    |> Location.Error.message
    |> print_endline
;;

let%expect_test "double pattern " =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      ~modul:None
      Sub
      [%expr
        let MY_PAT_1 = MY_EXPR_1
        and MY_PAT_2 = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub cannot be used with 'and' |}]
;;

let%expect_test "single pattern sub open" =
  Ppx_let_expander.expand
    ~modul:None
    Sub_open
    [%expr
      let MY_PAT_1 = MY_EXPR_1 in
      MY_BODY]
  |> print_expr;
  [%expect
    {|
    let () =
      Let_syntax.sub (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
        ~f:(fun (MY_PAT_1) -> MY_BODY) |}]
;;

let%expect_test "double pattern map open" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      ~modul:None
      Sub_open
      [%expr
        let MY_PAT_1 = MY_EXPR_1
        and MY_PAT_2 = MY_EXPR_2 in
        MY_BODY]);
  [%expect {|
    let%sub cannot be used with 'and' |}]
;;

let%expect_test "while%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      ~modul:None
      Sub_open
      [%expr
        while MY_PAT_1 = MY_EXPR_1 do
          MY_BODY
        done]);
  [%expect {|
    while%sub is not supported |}]
;;

let%expect_test "if%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      ~modul:None
      Sub_open
      [%expr if MY_EXPR_1 then BODY_1 else BODY_2]);
  [%expect {|
    if%sub is not supported |}]
;;

let%expect_test "match%sub is banned" =
  assert_fails_with_syntax_error ~f:(fun () ->
    Ppx_let_expander.expand
      ~modul:None
      Sub_open
      [%expr
        match MY_EXPR_1 with
        | PAT_1 -> BODY_1]);
  [%expect {|
    match%sub is not supported |}]
;;
