(* Basic. *)
let () =
  match `A with
  | `A -> ()
  | `B -> print_endline "foo"

(* In tail position. *)
let f () =
  match `A with
  | `A -> ()
  | `B -> print_endline "foo"

(* Selector subexpression. *)
let () =
  match not true with
  | true -> ()
  | false -> ()

(* Or-pattern. *)
let () =
  match `A with
  | `A | `B -> ()
  | `C -> ()

(* Exception. *)
let () =
  match `A with
  | `A -> ()
  | exception Exit -> print_endline "foo"

(* Exception or-pattern. *)
let () =
  match `A with
  | `A -> ()
  | exception (Exit | Not_found) -> ()

(* Exception in tail position. *)
let f () =
  match `A with
  | `A -> ()
  | exception Exit -> print_endline "foo"