(* Nat0: natural numbers (including zero) *)

type t = int

let of_int n =
  if n < 0 then failwith "Bin_prot.Nat0.of_int: n < 0";
  n

external unsafe_of_int : int -> t = "%identity"
