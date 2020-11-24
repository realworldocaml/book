open Sexplib
open Conv

type foo = A | B of int * float
[@@deriving sexp]

type 'a t =
  {
    x : foo;
    foo : int;
    bar : (float * string) list option;
  }
[@@deriving sexp]

type u = { t : int t }
[@@deriving sexp]

let%test_unit _ =
  let t =
    { x = B (42, 3.1)
    ; foo = 3
    ; bar = Some [(3.1, "foo")];
    }
  in
  let u = { t } in
  let u_sexp = sexp_of_u u in
  assert (Sexp.to_string u_sexp = "((t((x(B 42 3.1))(foo 3)(bar(((3.1 foo)))))))");

  let path_str = ".[0].[1]" in
  let path = Path.parse path_str in
  let subst, el = Path.subst_path u_sexp path in
  assert (Sexp.to_string el = "((x(B 42 3.1))(foo 3)(bar(((3.1 foo)))))");

  let dumb_sexp = subst (Atom "SUBST1") in
  assert (Sexp.to_string dumb_sexp = "((t SUBST1))");

  let path_str = ".t.x.B[1]" in
  let path = Path.parse path_str in
  let subst, el = Path.subst_path u_sexp path in
  assert (Sexp.to_string el = "3.1");

  let u_sexp = subst (Atom "SUBST2") in
  assert (Sexp.to_string u_sexp = "((t((x(B 42 SUBST2))(foo 3)(bar(((3.1 foo)))))))");
;;
