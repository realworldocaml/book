open! Core_kernel

module Weak = Caml.Weak

type 'a t = 'a Heap_block.t Weak.t

let create ~len = Weak.create len
let length t = Weak.length t
let set = Weak.set
let get = Weak.get
let is_some t i = Weak.check t i
let is_none t i = not (is_some t i)
let to_array t = Array.init (length t) ~f:(fun i -> get t i)
let sexp_of_t sexp_of_a t = [%sexp_of: a Heap_block.t option array] (to_array t)
