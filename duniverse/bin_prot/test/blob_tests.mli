open! Core
open! Import

val run_stability_test : 'a Bin_prot.Type_class.t -> ('a -> 'a -> bool) -> 'a -> unit
