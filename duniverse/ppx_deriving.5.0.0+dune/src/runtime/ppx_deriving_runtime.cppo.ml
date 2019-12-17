type nonrec int = int
type nonrec char = char
type nonrec string = string
type nonrec float = float
type nonrec bool = bool
type nonrec unit = unit
type nonrec exn = exn
type nonrec 'a array = 'a array
type nonrec 'a list = 'a list
type nonrec 'a option = 'a option
type nonrec nativeint = nativeint
type nonrec int32 = int32
type nonrec int64 = int64
type nonrec 'a lazy_t = 'a lazy_t
type nonrec bytes = bytes

#if OCAML_VERSION >= (4, 08, 0)
(* We require 4.08 while 4.07 already has a Stdlib module.
   In 4.07, the type equalities on Stdlib.Pervasives
   are not strong enough for the 'include Stdlib'
   below to satisfy the signature constraints on
   Ppx_deriving_runtime.Pervasives. *)
module Stdlib = Stdlib

include Stdlib

module Result = struct
  type ('a, 'b) t = ('a, 'b) Result.t =
    | Ok of 'a
    | Error of 'b

  type ('a, 'b) result = ('a, 'b) Result.t =
    | Ok of 'a
    | Error of 'b
end
#else
module Pervasives = Pervasives
module Stdlib = Pervasives

module Char = Char
module String = String
module Printexc = Printexc
module Array = Array
module List = List
module Nativeint = Nativeint
module Int32 = Int32
module Int64 = Int64
module Lazy = Lazy
module Bytes = Bytes

module Hashtbl = Hashtbl
module Queue = Queue
module Stack = Stack
module Set = Set
module Map = Map
module Weak = Weak

module Printf = Printf
module Format = Format
module Buffer = Buffer
module Result = struct
  (* the "result" compatibility module defines Result.result,
     not Result.t as the 4.08 stdlib *)
  type ('a, 'b) t = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b

  (* ... and we also expose Result.result for backward-compatibility *)
  type ('a, 'b) result = ('a, 'b) Result.result =
    | Ok of 'a
    | Error of 'b
end

include Pervasives
#endif
