(* Floating. *)
[@@@foo print_endline "bar"; ()]

(* Structure item. *)
let () =
  ()
  [@@foo print_endline "bar"; ()]

(* On expression. *)
let () =
  () [@foo print_endline "bar"; ()]
