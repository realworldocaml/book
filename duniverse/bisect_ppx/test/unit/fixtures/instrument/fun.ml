(* Basic. *)
let f = fun () ->
  print_endline "foo"

(* Structure item. *)
let f () =
  print_endline "foo"

(* Let-expression. *)
let () =
  let f () = print_endline "foo" in
  ()

(* Labeled argument. *)
let f = fun ~foo ->
  print_endline foo

(* Optional argument. *)
let f = fun ?foo () ->
  print_endline "foo"

(* Optional argument with default value. *)
let f = fun ?(foo = "foo") () ->
  print_endline foo

(* Optional argument elimination. *)
let f () ?x () =
  x

let () =
  ignore (List.map (f ()) [])
