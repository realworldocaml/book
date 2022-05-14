open Sexplib

let%test_module "tests" =
  (module struct
    let same x y = assert (x = y)
    let good s = same (Sexp.of_string s) (Atom "foo")

    let bad s =
      match Sexp.of_string s with
      | exception _exn -> ()
      | _sexp -> failwith "should have failed"
    ;;

    let%test_unit _ = good "foo"
    let%test_unit _ = good "foo\n"
    let%test_unit _ = good "foo;"
    let%test_unit _ = good "foo #;()"
    let%test_unit _ = good "foo #|blah|#"
    let%test_unit _ = good "foo #|blah|#\n"
    let%test_unit _ = good "foo; blah"
    let%test_unit _ = good "foo; blah\n"
    let%test_unit _ = good "foo; blah\n"
    (* multiple sexps *)
    let%test_unit _ = bad "foo bar"
    (* unterminated block comment *)
    let%test_unit _ = bad "foo #| bar"
    (* unterminated sexp *)
    let%test_unit _ = bad "foo ("

    let%expect_test "of_string_many" =
      let test str =
        let sexps = Sexp.of_string_many str in
        List.iter (fun sexp -> print_endline (sexp |> Sexp.to_string)) sexps
      in
      test "(foo) (bar)";
      [%expect {|
        (foo)
        (bar) |}]
    ;;

    let%expect_test "of_string_many_conv_exn" =
      let module Foo = struct
        type t =
          | A
          | B
        [@@deriving sexp]
      end
      in
      let list_of_sexp = Conv.list_of_sexp in
      let sexp_of_list = Conv.sexp_of_list in
      let test str =
        let foos = Sexp.of_string_many_conv_exn str [%of_sexp: Foo.t list] in
        List.iter
          (fun (x : Foo.t list) ->
             print_endline (Sexp.to_string ([%sexp_of: Foo.t list] x)))
          foos
      in
      test "(A) (B)";
      [%expect {|
        (A)
        (B) |}];
      Expect_test_helpers_core.show_raise (fun () -> test "(A) (B) (C)");
      [%expect
        {|
        (raised (
          Of_sexp_error
          "test_sexp_of_string.ml.t_of_sexp: unexpected variant constructor"
          (invalid_sexp C)
          (containing_sexp (C)))) |}]
    ;;
  end)
;;
