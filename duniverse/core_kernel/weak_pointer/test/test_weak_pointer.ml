open! Core_kernel
open! Expect_test_helpers_core
open! Weak_pointer

type contents = int ref [@@deriving sexp_of]

type heap_block = contents Heap_block.t [@@deriving sexp_of]

let heap_block name int =
  let b = ref int |> Heap_block.create_exn in
  Gc.Expert.add_finalizer_last b (fun _ -> print_s [%message "finalized" name]);
  b
;;

let print t =
  print_s [%message
    ""
      ~_:(t : int ref t)
      ~is_some:(is_some t : bool)
      ~get:(get t : contents Heap_block.t option)]
;;

let%expect_test "[create], [get], [set], clearing via GC" [@tags "no-js"] =
  let t = create () in
  print t;
  [%expect {|
    (() (is_some false) (get ())) |}];
  let b = heap_block "b" 13 in
  set t b;
  print t;
  [%expect {|
    ((13) (is_some true) (get (13))) |}];
  Gc.compact ();
  print t;
  [%expect {|
    ((13) (is_some true) (get (13))) |}];
  print_s [%sexp (b : heap_block)];
  [%expect {|
    13 |}];
  Gc.compact ();
  print t;
  [%expect {|
    (finalized b)
    (() (is_some false) (get ())) |}];
;;

let%expect_test "multiple [set]s and clearing" [@tags "no-js"] =
  let t = create () in
  let b1 = heap_block "b1" 13 in
  let b2 = heap_block "b2" 14 in
  set t b1;
  set t b2;
  Gc.compact ();
  [%expect {|
    (finalized b1) |}];
  print t;
  [%expect {|
    ((14) (is_some true) (get (14))) |}];
  print_s [%sexp (b2 : heap_block)];
  [%expect {|
    14 |}];
  Gc.compact ();
  [%expect {|
    (finalized b2) |}];
  print t;
  [%expect {|
    (() (is_some false) (get ())) |}];
;;
