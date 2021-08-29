open Core
open Ppxlib

let loc = Location.none

let print_expr expr =
  Pprintast.string_of_structure [%str let () = [%e expr]] |> print_string
;;

let%expect_test "while%bind expansion" =
  Ppx_let_expander.expand
    ~modul:None
    Bind
    [%expr
      while MY_CONDITION do
        MY_BODY
      done]
  |> print_expr;
  [%expect
    {|
    let () =
      let rec __let_syntax_loop__001_ () =
        Let_syntax.bind MY_CONDITION
          ~f:(function
              | true -> Let_syntax.bind MY_BODY ~f:__let_syntax_loop__001_
              | false -> Let_syntax.return ()) in
      __let_syntax_loop__001_ () |}]
;;

let%expect_test "while%bind trivial test" =
  let i = ref 0 in
  while%bind.Monad.Ident
    incr i;
    !i <= 5
  do
    printf "%d\n" !i
  done;
  [%expect {|
    1
    2
    3
    4
    5 |}]
;;

let%expect_test "monadic use" =
  let open Or_error.Let_syntax in
  let next i = if i < 5 then Ok (i + 1) else error_s [%message "too big"] in
  let t n =
    let i = ref 0 in
    let result =
      while%bind
        let%map i' = next !i in
        i := i';
        !i <= n
      do
        printf "%d\n" !i;
        Ok ()
      done
    in
    print_s [%sexp (result : unit Or_error.t)]
  in
  t 3;
  [%expect {|
    1
    2
    3
    (Ok ()) |}];
  t 10;
  [%expect {|
    1
    2
    3
    4
    5
    (Error "too big") |}]
;;
