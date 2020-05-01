(* Non-tail position. *)
let () =
  let () = print_endline "foo" in
  print_endline "bar"

(* Tail position. *)
let f () =
  let () = print_endline "foo" in
  print_endline "bar"
