(* [Array0] defines array functions that are primitives or can be simply defined in terms
   of [Caml.Array].  [Array0] is intended to completely express the part of [Caml.Array]
   that [Base] uses -- no other file in Base other than array0.ml should use [Caml.Array].
   [Array0] has few dependencies, and so is available early in Base's build order.  All
   Base files that need to use arrays and come before [Base.Array] in build order should
   do [module Array = Array0].  This includes uses of subscript syntax ([x.(i)], [x.(i) <-
   e]), which the OCaml parser desugars into calls to [Array.get] and [Array.set].
   Defining [module Array = Array0] is also necessary because it prevents ocamldep from
   mistakenly causing a file to depend on [Base.Array]. *)

open! Import0

module Sys = Sys0

let invalid_argf = Printf.invalid_argf

module Array = struct
  external create     :             int -> 'a -> 'a array = "caml_make_vect"
  external get        : 'a array -> int -> 'a             = "%array_safe_get"
  external length     : 'a array -> int                   = "%array_length"
  external set        : 'a array -> int -> 'a -> unit     = "%array_safe_set"
  external unsafe_get : 'a array -> int -> 'a             = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit     = "%array_unsafe_set"
end

include Array

let max_length = Sys.max_array_length

let create ~len x =
  try create len x
  with Invalid_argument _ ->
    invalid_argf "Array.create ~len:%d: invalid length" len ()
;;

let append      = Caml.Array.append
let blit        = Caml.Array.blit
let concat      = Caml.Array.concat
let copy        = Caml.Array.copy
let fill        = Caml.Array.fill
let init        = Caml.Array.init
let make_matrix = Caml.Array.make_matrix
let of_list     = Caml.Array.of_list
let sub         = Caml.Array.sub
let to_list     = Caml.Array.to_list


(* These are eta expanded in order to permute parameter order to follow Base
   conventions. *)
let fold        t ~init ~f = Caml.Array.fold_left   t ~init ~f
let fold_right  t ~f ~init = Caml.Array.fold_right  t ~f ~init
let iter        t ~f       = Caml.Array.iter        t ~f
let iteri       t ~f       = Caml.Array.iteri       t ~f
let map         t ~f       = Caml.Array.map         t ~f
let mapi        t ~f       = Caml.Array.mapi        t ~f
let stable_sort t ~compare = Caml.Array.stable_sort t ~cmp:compare

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;
;;
