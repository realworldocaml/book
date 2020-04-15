type ('a, 'b) record = {
  left : 'a;
  right : 'b;
}

(* Subexpression. *)
let () =
  match `A with
  | `A -> print_endline "foo"

(* Guard. *)
let () =
  match `A with
  | `A when print_endline "foo"; true -> ()
  | _ -> ()


(* assert false. *)
let () =
  match `A with
  | `A -> assert false

(* Or-pattern. *)
let () =
  match `A with
  | `A | `B -> ()

(* Nested or-pattern: alias. *)
let () =
  match `A with
  | (`A | `B) as x -> ()

(* Nested or-pattern: constructor. *)
let () =
  match None with
  | Some (`A | `B) -> ()
  | _ -> ()

(* Nested or-pattern: polymorphic variant. *)
let () =
  match `A `B with
  | `A (`B | `C) -> ()

(* Nested or-pattern: constraint. *)
let () =
  match `A with
  | ((`A | `B) : _) -> ()

(* Nested or-pattern: lazy. *)
let () =
  match lazy `A with
  | lazy (`A | `B) -> ()

(* Nested or-pattern: tuple. *)
let () =
  match (`A, `C) with
  | ((`A | `B), (`C | `D)) -> ()

(* Nested or-pattern: record. *)
let () =
  match {left = `A; right = `C} with
  | {left = `A | `B; right = `C | `D} -> ()

(* Nested or-pattern: array. *)
let () =
  match [|`A; `C|] with
  | [|`A | `B; `C | `D|] -> ()

(* Nested or-pattern: or. *)
let () =
  match `A with
  | (`A | `B) | `C -> ()

(* Or-pattern: binding. *)
let () =
  match `A () with
  | `A x | `B x -> x

(* Or-pattern: alias. *)
let () =
  match `A with
  | (`A as x) | (`B as x) -> ()

(* Or-pattern with guard. *)
let () =
  match `A with
  | `A | `B when print_endline "foo"; true -> ()
  | _ -> ()

(* Exception pattern. *)
let () =
  match `A with
  | `A -> ()
  | exception Exit -> print_endline "foo"

(* Exception pattern with guard. *)
let () =
  match `A with
  | `A -> ()
  | exception Exit when print_endline "foo"; true -> ()

(* Exception or-pattern. *)
let () =
  match `A with
  | `A -> ()
  | exception (Exit | Not_found) -> ()
