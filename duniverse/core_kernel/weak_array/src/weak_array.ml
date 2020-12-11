open! Core_kernel

module Weak = Caml.Weak

type 'a t = 'a Heap_block.t Weak.t

let create ~len = Weak.create len
let length t = Weak.length t
let set = Weak.set

let set_exn t i x = set t i (Option.map x ~f:Heap_block.create_exn)

let get = Weak.get
let is_some t i = Weak.check t i
let is_none t i = not (is_some t i)
let to_array t = Array.init (length t) ~f:(fun i -> get t i)
let sexp_of_t sexp_of_a t = [%sexp_of: a Heap_block.t option array] (to_array t)

let iter t ~f =
  for i = 0 to length t - 1 do
    match get t i with
    | None -> ()
    | Some v -> f (Heap_block.value v)
  done

let iteri t ~f =
  for i = 0 to length t - 1 do
    match get t i with
    | None -> ()
    | Some v -> f i (Heap_block.value v)
  done

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  Weak.blit src src_pos dst dst_pos len
