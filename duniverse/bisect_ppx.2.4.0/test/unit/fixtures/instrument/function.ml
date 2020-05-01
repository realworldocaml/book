(* Basic. Most of the testing of cases is done in cases.ml. *)
let f = function
  | `A -> ()
  | `B -> print_endline "foo"

(* Abstracted. *)
let f () = function
  | `A -> ()
  | `B -> ()

(* Or pattern. *)
let f = function
  | `A | `B -> ()
  | `C -> ()
