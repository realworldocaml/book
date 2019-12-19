(* Care has been taken to ensure that when working with immediate values in the pool, the
   compiler is able to emit code that does not invoke the write barrier.

   This can be very easily broken, and as such we're adding these tests to detect when
   this may have happened. *)

open Core_kernel
module Caml_modify = Replace_caml_modify_for_testing

(* Big enough length so an array goes directly into the major heap. *)
let big = 32 * 1024

let check ~expected ~f =
  Caml_modify.reset ();
  f ();
  Caml_modify.count () = expected
;;

let () =
  let array_imm = Array.init big ~f:Fn.id in
  let array_obj = Array.init big ~f:(fun i -> ref i) in
  let v = ref (Random.int 42) in
  let array_imm_set (a : int array) i (x : int) = a.(i) <- x in
  let array_obj_set a i x = a.(i) <- x in
  assert (check ~expected:0 ~f:(fun () -> array_imm_set array_imm 0 0));
  assert (check ~expected:1 ~f:(fun () -> array_obj_set array_imm 0 0));
  assert (check ~expected:1 ~f:(fun () -> array_obj_set array_obj 0 v))
;;

let () =
  let open Tuple_pool in
  let p = create Slots.t3 ~capacity:3 ~dummy:(Pointer.null (), 0, "") in
  let e = new3 p (Pointer.null ()) 0 "" in
  let v = Int.to_string (Random.int 42) in
  let n = if Obj.is_int (Obj.repr e) then 0 else 1 in
  assert (check ~expected:n ~f:(fun () -> set p e Slot.t0 e));
  assert (check ~expected:0 ~f:(fun () -> set p e Slot.t1 0));
  assert (check ~expected:1 ~f:(fun () -> set p e Slot.t2 v))
;;
