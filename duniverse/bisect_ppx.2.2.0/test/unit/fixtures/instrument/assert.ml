(* In non-tail position. *)
let () =
  assert true

(* In tail position. *)
let f () =
  assert true

(* Subexpression. *)
let () =
  assert (print_endline "foo"; true)

(* assert false. *)
let () =
  assert false

(* assert false: function. *)
let f = function
  | `A -> assert false

(* assert false: match. *)
let () =
  match `A with
  | `A -> assert false

(* assert false: try. *)
let () =
  try ()
  with Exit -> assert false

(* assert false: if. *)
let () =
  if true then
    assert false
  else
    assert false

(* assert false: while. *)
let () =
  while false do
    assert false
  done

(* assert false: for. *)
let () =
  for i = 1 to 0 do
    assert false
  done

(* assert false: lazy. *)
let _ =
  lazy (assert false)

(* assert false: method. *)
let _ =
  object
    method foo =
      assert false
  end
