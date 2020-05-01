(* Non-tail position. *)
let () =
  let exception E in
  print_endline "bar"

(* Tail position. *)
let f () =
  let exception E in
  print_endline "bar"

