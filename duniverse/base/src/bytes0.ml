(* [Bytes0] defines string functions that are primitives or can be simply
   defined in terms of [Caml.Bytes]. [Bytes0] is intended to completely express
   the part of [Caml.Bytes] that [Base] uses -- no other file in Base other
   than bytes0.ml should use [Caml.Bytes]. [Bytes0] has few dependencies, and
   so is available early in Base's build order.

   All Base files that need to use strings and come before [Base.Bytes] in
   build order should do:

   {[
     module Bytes  = Bytes0
   ]}

   Defining [module Bytes = Bytes0] is also necessary because it prevents
   ocamldep from mistakenly causing a file to depend on [Base.Bytes]. *)

open! Import0
module Sys = Sys0

module Primitives = struct
  external get : bytes -> int -> char = "%bytes_safe_get"
  external length : bytes -> int = "%bytes_length"
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"

  include Bytes_set_primitives

  (* [unsafe_blit_string] is not exported in the [stdlib] so we export it here *)
  external unsafe_blit_string
    :  src:string
    -> src_pos:int
    -> dst:bytes
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_blit_string"
  [@@noalloc]
end

include Primitives

let max_length = Sys.max_string_length
let blit = Caml.Bytes.blit
let blit_string = Caml.Bytes.blit_string
let compare = Caml.Bytes.compare
let copy = Caml.Bytes.copy
let create = Caml.Bytes.create
let fill = Caml.Bytes.fill
let make = Caml.Bytes.make
let map = Caml.Bytes.map
let mapi = Caml.Bytes.mapi
let sub = Caml.Bytes.sub
let unsafe_blit = Caml.Bytes.unsafe_blit
let to_string = Caml.Bytes.to_string
let of_string = Caml.Bytes.of_string

let unsafe_to_string ~no_mutation_while_string_reachable:s =
  Caml.Bytes.unsafe_to_string s
;;

let unsafe_of_string_promise_no_mutation = Caml.Bytes.unsafe_of_string
