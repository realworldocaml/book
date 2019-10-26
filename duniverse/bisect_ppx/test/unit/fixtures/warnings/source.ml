(* Trigger at least one warning to make sure they are on. *)
let () =
  let f ?maybe () = ignore maybe in
  () |> f

type t = A | B | C
type u = {a : int; b : int; c : int}

(* Triggers (suppressed) warning 4: fragile pattern matching (due to
   wildcard). *)
(* Triggers (suppressed) warning 11: unused match case (wildcard). *)
let f x =
  match x with
  | A | B | C -> 42

let g x =
  match x with
  | A | B -> 42
  | C -> 43

(* Triggers a (suppressed) duplicate set of warning 9 (missing record
   labels). *)
(* Triggers (suppressed) warning 27: unused variable a. *)
let h x =
  match x with
  | {a} | {a} -> ignore a

(* Triggers (suppressed) warning 26: unused variable x. *)
let i x =
  match x with
  | (A as x) | (B as x) -> ignore x; 42
  | C -> 43

(* Triggers (suppressed) duplicate warning 28 (wildcard given to constant
   constructor). *)
let j x =
  match x with
  | A _ | B | C -> ()
