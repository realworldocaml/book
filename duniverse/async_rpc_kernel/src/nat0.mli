(** The bin-prot representation of lengths of strings, arrays, etc. *)

type t = Bin_prot.Nat0.t [@@deriving bin_io]

(** fails on negative values *)
val of_int_exn : int -> t
