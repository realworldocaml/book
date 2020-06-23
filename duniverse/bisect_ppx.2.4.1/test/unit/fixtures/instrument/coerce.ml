(* In non-tail position. *)
let () =
  (print_endline "foo" : unit :> unit)

(* In tail position. *)
let f () =
  (print_endline "foo" : unit :> unit)
