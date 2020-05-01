(* If-then-else: non-tail position. *)
let () =
  if true then
    print_endline "foo"
  else
    print_endline "bar"

(* If-then-else: tail position. *)
let f () =
  if true then
    print_endline "foo"
  else
    print_endline "bar"

(* If-then-else: condition subexpression. *)
let () =
  if not true then
    ()
  else
    ()

(* If-then: non-tail position. *)
let () =
  if true then
    print_endline "foo"

(* If-then: tail position. *)
let f () =
  if true then
    print_endline "foo"

(* If-then: condition subexpression. *)
let () =
  if not true then
    ()

