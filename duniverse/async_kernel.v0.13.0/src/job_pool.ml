open! Core_kernel
open! Import
open Pool
module Pointer = Pointer
module Slot = Slot

let dummy_e = Execution_context.main
let dummy_f : Obj.t -> unit = ignore
let dummy_a : Obj.t = Obj.repr ()

type slots = (Execution_context.t, Obj.t -> unit, (Obj.t[@sexp.opaque])) Slots.t3
[@@deriving sexp_of]

type t = slots Pool.t [@@deriving sexp_of]

let invariant t = Pool.invariant ignore t
let create () = create Slots.t3 ~capacity:1 ~dummy:(dummy_e, dummy_f, dummy_a)
