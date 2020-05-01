(** Provides functors for making a module quickcheckable with {!Quickcheck}. *)

open! Import

module type S = Quickcheck.S
module type S1 = Quickcheck.S1
module type S2 = Quickcheck.S2
module type S_int = Quickcheck.S_int
