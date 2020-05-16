open! Core_kernel
open! Expect_test_helpers_base
module Hash_heap = Hash_heap.Make (Int)

let ( @? ) s b = require [%here] b ~if_false_then_print_s:(lazy [%message s])
let s = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

let make () =
  let h = Hash_heap.create Int.compare in
  List.iter s ~f:(fun i ->
    match Hash_heap.push h ~key:i ~data:i with
    | `Ok -> ()
    | `Key_already_present -> assert false);
  h
;;

let z = string_of_int

let%expect_test "create, and push" = ignore (make () : int Hash_heap.t)

let%expect_test "find" =
  let t = make () in
  List.iter s ~f:(fun i -> z i @? [%equal: int option] (Hash_heap.find t i) (Some i))
;;

let%expect_test "pop and top" =
  let t = make () in
  List.iter s ~f:(fun i ->
    ("top" ^ z i) @? [%equal: int option] (Hash_heap.top t) (Some i);
    ("pop" ^ z i) @? [%equal: int option] (Hash_heap.pop t) (Some i);
    ("topafter" ^ z i) @? not ([%equal: int option] (Hash_heap.top t) (Some i));
    ("findable" ^ z i) @? [%equal: int option] (Hash_heap.find t i) None)
;;

let%expect_test "mem" =
  let t = make () in
  "all" @? List.for_all s ~f:(Hash_heap.mem t)
;;

let%expect_test "find_pop" =
  let t = make () in
  List.iter s ~f:(fun i ->
    ("lookup" ^ z i) @? [%equal: int option] (Hash_heap.find_pop t i) (Some i);
    ("remove" ^ z i) @? not (Hash_heap.mem t i))
;;

let%expect_test "pop_if" =
  let t = make () in
  "no" @? [%equal: int option] (Hash_heap.pop_if t (fun i -> i < 0)) None;
  "still-there" @? List.for_all s ~f:(Hash_heap.mem t);
  "yes" @? [%equal: int option] (Hash_heap.pop_if t (fun _ -> true)) (Some 1);
  "gone-top" @? not ([%equal: int option] (Hash_heap.top t) (Some 1));
  "gone-mem" @? not (Hash_heap.mem t 1)
;;

let%expect_test "iteri" =
  let t = make () in
  "match"
  @? (Set.compare_direct
        (Set.Poly.of_list s)
        (let s = ref Set.Poly.empty in
         Hash_heap.iteri t ~f:(fun ~key ~data ->
           s := Set.add !s key;
           assert (key = data));
         !s)
      = 0)
;;

let%expect_test "remove" =
  let t = make () in
  Hash_heap.remove t 2;
  Hash_heap.remove t 1;
  "1gone-mem" @? not (Hash_heap.mem t 1);
  "2gone-mem" @? not (Hash_heap.mem t 2);
  "1gone-top" @? not ([%equal: int option] (Hash_heap.top t) (Some 1));
  "2gone-top" @? not ([%equal: int option] (Hash_heap.top t) (Some 2))
;;

let%expect_test "replace" =
  let t = make () in
  Hash_heap.replace t ~key:1 ~data:12;
  "present" @? [%equal: int option] (Hash_heap.find t 1) (Some 12);
  "top" @? [%equal: int option] (Hash_heap.top t) (Some 2)
;;
