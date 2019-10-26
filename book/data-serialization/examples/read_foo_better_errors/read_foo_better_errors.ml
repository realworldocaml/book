open Core

type t = {
  a: string;
  b: int;
  c: float option
} [@@deriving sexp]

let run () =
  let t = Sexp.load_sexp_conv_exn "foo_broken_example.scm" t_of_sexp in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
