open Core_kernel
open! Int.Replace_polymorphic_compare

type 'a t =
  { mutable array : 'a Option_array.t
  ; mutable length : int
  ; mutable capacity : int
  }

let to_array t = Array.init t.length ~f:(fun i -> Option_array.get_some_exn t.array i)

let create ?(capacity = 1) () =
  { array = Option_array.create ~len:capacity; capacity; length = 0 }
;;

let set_capacity t desired_capacity =
  let new_capacity = Int.ceil_pow2 (max 1 (max desired_capacity t.length)) in
  if new_capacity <> t.capacity
  then (
    let dst = Option_array.create ~len:new_capacity in
    Option_array.unsafe_blit ~len:t.length ~src:t.array ~src_pos:0 ~dst ~dst_pos:0;
    t.array <- dst;
    t.capacity <- new_capacity)
;;

let append t x =
  if t.length >= t.capacity then set_capacity t (2 * t.length);
  Option_array.unsafe_set_some t.array t.length x;
  t.length <- t.length + 1
;;

let lax_clear t = t.length <- 0

let nth_exn t n =
  if n < 0 || n >= t.length
  then failwith "Index out of bounds"
  else Option_array.get_some_exn t.array n
;;

let of_list l =
  let t = create ~capacity:(List.length l) () in
  List.iter l ~f:(fun x -> append t x);
  t
;;

let to_list t = List.init t.length ~f:(Option_array.get_some_exn t.array)
let length t = t.length
