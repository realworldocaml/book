let (let*) x f = f x
let (and*) x y = (x, y)
let return x = x

(* Basic. *)
let () =
  let* () = print_endline "foo" in
  return ()

(* ands. *)
let () =
  let* () = print_endline "foo"
  and* () = print_endline "bar" in
  return ()

(* Sequence. *)
let () =
  let* () = print_endline "foo" in
  let* () = print_endline "bar" in
  return ()
