(* In non-tail position. *)
let () =
  (print_endline "foo" :> unit)

(* In tail position. *)
let f () =
  (print_endline "foo" :> unit)
