open Core.Std

type t = { 
  a: string;
  b: int;
  c: float option 
} with sexp

let run () =
  let t =
    Sexp.load_sexp "foo_broken_example.scm"
    |> t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
