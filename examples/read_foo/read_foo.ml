(* file: read_foo.ml *)

open Core.Std

type u = Foo | Bar with sexp
type t = { a: u;
           b: int;
           c: float option }
with sexp

let run () =
  let t =
    Sexp.load_sexp "foo.scm"
    |! t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
