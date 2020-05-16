(** Nat0: natural numbers (including zero) *)

type t = private int

(** [of_int n] converts integer [n] to a natural number.  @raise Failure
    if [n] is negative. *)
val of_int : int -> t

external unsafe_of_int : int -> t = "%identity"
