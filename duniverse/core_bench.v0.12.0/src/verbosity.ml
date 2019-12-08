(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core

type t =
  | Quiet
  | Low
  | High
[@@deriving sexp]

let verbosity : t ref = ref Low

let set_verbosity v =
  verbosity := v

let print_high s =
  match !verbosity with
  | High -> printf s
  | Low | Quiet -> Printf.ifprintf stdout s

let print_low s =
  match !verbosity with
  | High | Low -> printf s
  | Quiet -> Printf.ifprintf stdout s
