open Sexplib

let%test_module "tests" = (
  module struct

    let same x y = assert (x = y)

    let good s = same (Sexp.of_string s) (Atom "foo")

    let bad s = match Sexp.of_string s with
      | exception _exn -> ()
      | _sexp -> failwith "should have failed"

    let%test_unit _ =
      good "foo"
    let%test_unit _ =
      good "foo\n"
    let%test_unit _ =
      good "foo;"
    let%test_unit _ =
      good "foo #;()"
    let%test_unit _ =
      good "foo #|blah|#"
    let%test_unit _ =
      good "foo #|blah|#\n"
    let%test_unit _ =
      good "foo; blah"
    let%test_unit _ =
      good "foo; blah\n"
    let%test_unit _ =
      good "foo; blah\n"

    (* multiple sexps *)
    let%test_unit _ =
      bad "foo bar"

    (* unterminated block comment *)
    let%test_unit _ =
      bad "foo #| bar"

    (* unterminated sexp *)
    let%test_unit _ =
      bad "foo ("

  end)
