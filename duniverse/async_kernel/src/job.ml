open! Core
open! Import

type t = Job_pool.slots Pool.Pointer.t [@@deriving sexp_of]
