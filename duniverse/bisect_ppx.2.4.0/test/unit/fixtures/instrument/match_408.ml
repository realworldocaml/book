(* Exception patterns under an or-pattern. *)
let () =
  match `A with
  | `A -> ()
  | exception Exit | exception Not_found -> ()

(* Exception under a constraint. *)
let () =
  match () with
  | () -> ()
  | ((exception Exit) : unit) -> ()

(* Exception or-pattern under a constraint. *)
let () =
  match () with
  | () -> ()
  | ((exception Exit | exception Not_found) : unit) -> ()

(* Exception under open. *)
let () =
  match `A with
  | `A -> ()
  | List.(exception Exit) -> ()

(* Exception or-pattern under open. *)
let () =
  match `A with
  | `A -> ()
  | List.(exception Exit | exception Not_found) -> ()

(* Mixed value/exception case. *)
let () =
  match `A with
  | `A | exception Exit -> ()

(* Mixed case - non-trivial RHS. *)
let () =
  match `A with
  | `A | exception Exit -> if true then ignore `B else ignore `C

(* Ordinary and mixed cases. *)
let () =
  match `A with
  | `A -> ()
  | `B | exception Exit -> ()
  | exception Not_found -> ()

(* Multiple mixed cases. *)
let () =
  match `A with
  | `A | exception Exit -> ()
  | `B | exception Not_found -> ()

(* Bound variables. *)
let () =
  match "foo" with
  | x as y | exception Invalid_argument (x as y) -> ()
