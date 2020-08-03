open! Core_kernel
open! Expect_test_helpers_core
open! Weak_array

type block = int ref Heap_block.t [@@deriving sexp_of]

let block int = ref int |> Heap_block.create_exn
let print t = print_s [%sexp (t : int ref t)]

let%expect_test "[length]" =
  for len = 0 to 3 do
    print_s [%sexp (length (create ~len) : int)]
  done;
  [%expect {|
    0
    1
    2
    3 |}]
;;

let%expect_test "[get], [set]" =
  let t = create ~len:1 in
  print t;
  [%expect {|
    (()) |}];
  set t 0 None;
  print t;
  [%expect {|
    (()) |}];
  show_raise (fun () -> set t (-1) None);
  [%expect {|
    (raised (Invalid_argument Weak.set)) |}];
  show_raise (fun () -> set t 1 None);
  [%expect {|
    (raised (Invalid_argument Weak.set)) |}];
  print_s [%sexp (get t 0 : _ Heap_block.t option)];
  [%expect {|
    () |}];
  show_raise (fun () -> ignore (get t (-1) : _ option));
  [%expect {|
    (raised (Invalid_argument Weak.get)) |}];
  show_raise (fun () -> ignore (get t 1 : _ option));
  [%expect {|
    (raised (Invalid_argument Weak.get)) |}];
  let b = block 13 in
  set t 0 (Some b);
  print t;
  [%expect {|
    ((13)) |}];
  ignore (b : block);
  set t 0 None;
  print t;
  [%expect {|
    (()) |}]
;;

let%expect_test "[is_none], [is_some]" =
  let t = create ~len:1 in
  let print () = print_s [%message (is_none t 0 : bool) (is_some t 0 : bool)] in
  print ();
  [%expect {|
    (("is_none t 0" true)
     ("is_some t 0" false)) |}];
  let b = block 13 in
  set t 0 (Some b);
  print ();
  [%expect {|
    (("is_none t 0" false)
     ("is_some t 0" true)) |}];
  ignore (b : block)
;;

let%expect_test ("clearing, with no finalizer attached"[@tags "no-js"]) =
  let t = create ~len:1 in
  let b = block 13 in
  set t 0 (Some b);
  print t;
  [%expect {|
    ((13)) |}];
  Gc.compact ();
  print t;
  [%expect {|
    (()) |}];
  Gc.compact ();
  print t;
  [%expect {|
    (()) |}]
;;

(* This test demonstrates a difference between OCaml 4.02 and 4.03.  In 4.02, a weak
   pointer with an attached finalizer is cleared after the first compaction.  In 4.03, the
   weak is not cleared until the second compaction. *)
let%expect_test ("clearing, with a finalizer attached"[@tags "no-js"]) =
  let t = create ~len:1 in
  let b = block 13 in
  set t 0 (Some b);
  print t;
  [%expect {|
    ((13)) |}];
  Gc.Expert.add_finalizer b (fun b -> print_s [%message "finalized" (b : block)]);
  Gc.compact ();
  [%expect {|
    (finalized (b 13)) |}];
  print t;
  [%expect {|
    ((13)) |}];
  Gc.compact ();
  print t;
  [%expect {|
    (()) |}]
;;
