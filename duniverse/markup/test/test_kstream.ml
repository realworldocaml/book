(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Markup__Kstream

let exn = Failure "failure"

let then_exn l =
  let s = of_list l in
  (fun throw _ k -> next s throw (fun () -> throw exn) k) |> make

let failed_wrong = wrong_k "failed"
let failed = assert_equal exn

let internal_tests = [
  ("kstream.internal.make" >:: fun _ ->
    let s = (fun _ _ k -> k "foo") |> make in
    next s failed_wrong (wrong_k "empty") (assert_equal "foo"));

  ("kstream.internal.of_list,next" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    next s failed_wrong (wrong_k "empty (1)") (assert_equal ~msg:"1" 1);
    next s failed_wrong (wrong_k "empty (2)") (assert_equal ~msg:"2" 2);
    next s failed_wrong (wrong_k "empty (3)") (assert_equal ~msg:"3" 3);
    next s failed_wrong ignore (wrong_k "not empty");
    next s failed_wrong ignore (wrong_k "not empty"));

  ("kstream.internal.next.exn" >:: fun _ ->
    let s = (fun throw _ _ -> throw exn) |> make in
    next s failed (wrong_k "empty") (wrong_k "not empty"));

  ("kstream.internal.to_list" >:: fun _ ->
    to_list (of_list [1; 2; 3]) failed_wrong (assert_equal [1; 2; 3]));

  ("kstream.internal.to_list.exn" >:: fun _ ->
    to_list (then_exn [1]) failed (wrong_k "did not fail"));

  ("kstream.internal.next_option" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    next_option s failed_wrong (assert_equal ~msg:"1" (Some 1));
    next_option s failed_wrong (assert_equal ~msg:"2" (Some 2));
    next_option s failed_wrong (assert_equal ~msg:"3" (Some 3));
    next_option s failed_wrong (assert_equal ~msg:"empty" None);
    next_option s failed_wrong (assert_equal ~msg:"still empty" None));

  ("kstream.internal.next_option.exn" >:: fun _ ->
    let s = then_exn [1] in
    next_option s failed_wrong (assert_equal ~msg:"1" (Some 1));
    next_option s failed (wrong_k "did not fail"));

  ("kstream.internal.next_expected" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    next_expected s failed_wrong (assert_equal ~msg:"1" 1);
    next_expected s failed_wrong (assert_equal ~msg:"2" 2);
    next_expected s failed_wrong (assert_equal ~msg:"3" 3);
    next_expected s
      (assert_equal (Failure "stream empty")) (wrong_k "not empty"));

  ("kstream.internal.next_expected.exn" >:: fun _ ->
    let s = then_exn [1] in
    next_expected s failed_wrong (assert_equal ~msg:"1" 1);
    next_expected s failed (wrong_k "did not fail"));

  ("kstream.internal.next_n" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    next_n 2 s failed_wrong (assert_equal ~msg:"1,2" [1; 2]);
    next_n 2 s failed_wrong (assert_equal ~msg:"3" [3]);
    next_n 2 s failed_wrong (assert_equal ~msg:"empty" []);
    next_n (-1) s (assert_equal (Invalid_argument "n is negative"))
      (wrong_k "did not fail"));

  ("kstream.internal.next_n.exn" >:: fun _ ->
    let s = then_exn [1; 2; 3] in
    next_n 2 s failed_wrong (assert_equal ~msg:"1,2" [1; 2]);
    next_n 2 s failed (wrong_k "did not fail"));

  ("kstream.internal.push" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    push s 4;
    push s 5;
    to_list s failed_wrong (assert_equal [5; 4; 1; 2; 3]));

  ("kstream.internal.push_option" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    push_option s (None);
    push_option s (Some 5);
    to_list s failed_wrong (assert_equal [5; 1; 2; 3]));

  ("kstream.internal.push_list" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    push_list s [4; 5];
    push_list s [6; 7; 8];
    push_list s [];
    to_list s failed_wrong (assert_equal [6; 7; 8; 4; 5; 1; 2; 3]));

  ("kstream.internal.peek" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek s failed_wrong (wrong_k "empty (1)") (assert_equal ~msg:"1" 1);
    peek s failed_wrong (wrong_k "empty (1b)") (assert_equal ~msg:"1b" 1);
    next s failed_wrong ignore ignore;
    peek s failed_wrong (wrong_k "empty (2)") (assert_equal ~msg:"2" 2);
    to_list s failed_wrong ignore;
    peek s failed_wrong ignore (wrong_k "not empty"));

  ("kstream.internal.peek.exn" >:: fun _ ->
    let s = then_exn [1] in
    peek s failed_wrong (wrong_k "empty (1)") (assert_equal ~msg:"1" 1);
    next s failed_wrong ignore ignore;
    peek s failed (wrong_k "empty") (wrong_k "not empty"));

  ("kstream.internal.peek_option" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek_option s failed_wrong (assert_equal ~msg:"1" (Some 1));
    peek_option s failed_wrong (assert_equal ~msg:"1b" (Some 1));
    next s failed_wrong ignore ignore;
    peek_option s failed_wrong (assert_equal ~msg:"2" (Some 2));
    to_list s failed_wrong ignore;
    peek_option s failed_wrong (assert_equal ~msg:"empty" None));

  ("kstream.internal.peek_option.exn" >:: fun _ ->
    let s = then_exn [1] in
    peek_option s failed_wrong (assert_equal ~msg:"1" (Some 1));
    next s failed_wrong ignore ignore;
    peek_option s failed (wrong_k "did not fail"));

  ("kstream.internal.peek_expected" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek_expected s failed_wrong (assert_equal ~msg:"1" 1);
    peek_expected s failed_wrong (assert_equal ~msg:"1b" 1);
    next s failed_wrong ignore ignore;
    peek_expected s failed_wrong (assert_equal ~msg:"2" 2);
    to_list s failed_wrong ignore;
    peek_expected s (assert_equal (Failure "stream empty"))
      (wrong_k "did not fail"));

  ("kstream.internal.peek_expected.exn" >:: fun _ ->
    let s = then_exn [1] in
    peek_expected s failed_wrong (assert_equal ~msg:"1" 1);
    next s failed_wrong ignore ignore;
    peek_expected s failed (wrong_k "did not fail"));

  ("kstream.internal.peek_n" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek_n 2 s failed_wrong (assert_equal ~msg:"1,2" [1; 2]);
    peek_n 2 s failed_wrong (assert_equal ~msg:"1,2" [1; 2]);
    next_n 2 s failed_wrong ignore;
    peek_n 2 s failed_wrong (assert_equal ~msg:"3" [3]);
    to_list s failed_wrong ignore;
    peek_n 2 s failed_wrong (assert_equal ~msg:"empty" []);
    peek_n (-1) s (assert_equal (Invalid_argument "n is negative"))
      (wrong_k "did not fail"));

  ("kstream.internal.peek_n.exn" >:: fun _ ->
    let s = then_exn [1; 2; 3] in
    peek_n 2 s failed_wrong (assert_equal ~msg:"1,2" [1; 2]);
    next_n 2 s failed_wrong ignore;
    peek_n 2 s failed (wrong_k "did not fail"));

  ("kstream.internal.tap" >:: fun _ ->
    let buffer = Buffer.create 4 in
    let s = of_list ['f'; 'o'; 'o'; 'b'; 'a'; 'r'] in
    let restore = tap (Buffer.add_char buffer) s in
    peek_n 3 s failed_wrong ignore;
    next_n 3 s failed_wrong ignore;
    next s failed_wrong ignore ignore;
    restore ();
    to_list s failed_wrong ignore;
    assert_equal (Buffer.contents buffer) "foob");

  ("kstream.internal.tap.exn" >:: fun _ ->
    let buffer = Buffer.create 4 in
    let s = then_exn ['f'; 'o'; 'o'; 'b'] in
    (tap (Buffer.add_char buffer) s |> ignore) [@ocaml.warning "-5"];
    to_list s failed (wrong_k "did not fail");
    assert_equal (Buffer.contents buffer) "foob");

  ("kstream.internal.checkpoint" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    let s', restore = checkpoint s in
    next s' failed_wrong (wrong_k "empty") (assert_equal 1);
    peek_n 2 s' failed_wrong (assert_equal [2; 3]);
    restore ();
    to_list s failed_wrong (assert_equal ~msg:"restore" [1; 2; 3]);

    let s = of_list [1; 2; 3] in
    push s 0;
    let s', restore = checkpoint s in
    next_n 2 s' failed_wrong (assert_equal [0; 1]);
    restore ());

  ("kstream.internal.checkpoint.exn" >:: fun _ ->
    let s = then_exn [1; 2; 3] in
    checkpoint s |> ignore;
    to_list s failed (wrong_k "did not fail"));

  ("kstream.internal.construct" >:: fun _ ->
    let called = ref false in
    let s = construct (fun _ k -> called := true; k (of_list [1; 2; 3])) in
    assert_bool "not called" (not !called);
    next s failed_wrong (wrong_k "empty") (assert_equal ~msg:"1" 1);
    assert_bool "called" !called;
    to_list s failed_wrong (assert_equal ~msg:"2,3" [2; 3]));

  ("kstream.internal.construct.exn" >:: fun _ ->
    let s = construct (fun throw _ -> throw exn) in
    next s failed (wrong_k "empty") (wrong_k "not empty"));

  ("kstream.internal.construct.compose" >:: fun _ ->
    let constructor1_calls = ref 0 in
    let constructor2_calls = ref 0 in
    let s =
      construct (fun _ k ->
        constructor1_calls := !constructor1_calls + 1;
        k (construct (fun _ k ->
          constructor2_calls := !constructor2_calls + 1;
          k (of_list [1; 2; 3]))))
    in
    next_option s failed_wrong (assert_equal (Some 1));
    next_option s failed_wrong (assert_equal (Some 2));
    assert_equal ~msg:"constructor 1" !constructor1_calls 1;
    assert_equal ~msg:"constructor 2" !constructor2_calls 1);

  ("kstream.internal.map" >:: fun _ ->
    let s = of_list [1; 2; 3] |> map (fun v _ k -> k (v + 1)) in
    to_list s failed_wrong (assert_equal [2; 3; 4]));

  ("kstream.internal.map.exn" >:: fun _ ->
    let s = then_exn [1; 2; 3] |> map (fun v _ k -> k (v + 1)) in
    to_list s failed (wrong_k "did not fail");

    let s = of_list [1; 2; 3] |> map (fun _ throw _ -> throw exn) in
    to_list s failed (wrong_k "did not fail"));

  ("kstream.internal.transform" >:: fun _ ->
    let nth_double n =
      transform (fun acc v _ k ->
        if acc = n then k ([v; v], None)
        else k ([], Some (acc + 1)))
        0
    in

    let s = of_list [1; 2; 3] |> nth_double 1 in
    to_list s failed_wrong (assert_equal [2; 2]));

  ("kstream.internal.fold" >:: fun _ ->
    fold (fun v v' _ k -> k (v + v')) 0 (of_list [1; 2; 3]) failed_wrong
      (assert_equal 6));

  ("kstream.internal.fold.exn" >:: fun _ ->
    fold (fun v v' _ k -> k (v + v')) 0 (then_exn [1; 2; 3]) failed
      (wrong_k "did not fail");

    fold (fun _ _ throw _ -> throw exn) 0 (of_list [1; 2; 3]) failed
      (wrong_k "did not fail"));

  ("kstream.internal.iter" >:: fun _ ->
    let sum = ref 0 in
    iter (fun v _ k -> sum := !sum + v; k ()) (of_list [1; 2; 3]) failed_wrong
      ignore;
    assert_equal !sum 6);

  ("kstream.internal.iter.exn" >:: fun _ ->
    iter (fun v _ k -> k (ignore v)) (then_exn [1; 2; 3]) failed
      (wrong_k "did not fail");

    iter (fun _ throw _ -> throw exn) (of_list [1; 2; 3]) failed
      (wrong_k "did not fail"));

  ("kstream.internal.filter_map" >:: fun _ ->
    let s =
      filter_map (fun v _ k ->
        k (if v mod 2 = 0 then Some (string_of_int v) else None))
        (of_list [1; 2; 3; 4])
    in
    to_list s failed_wrong (assert_equal ["2"; "4"]));

  ("kstream.internal.filter_map.exn" >:: fun _ ->
    let s = filter_map (fun v _ k -> k (Some v)) (then_exn [1; 2; 3; 4]) in
    to_list s failed (wrong_k "did not fail");

    let s = filter_map (fun _ throw _ -> throw exn) (of_list [1; 2; 3; 4]) in
    to_list s failed (wrong_k "did not fail"));

  ("kstream.internal.filter" >:: fun _ ->
    let s = filter (fun v _ k -> k (v mod 2 = 0)) (of_list [1; 2; 3; 4]) in
    to_list s failed_wrong (assert_equal [2; 4]));

  ("kstream.internal.filter.exn" >:: fun _ ->
    let s = filter (fun _ _ k -> k true) (then_exn [1; 2; 3; 4]) in
    to_list s failed (wrong_k "did not fail");

    let s = filter (fun _ throw _ -> throw exn) (of_list [1; 2; 3; 4]) in
    to_list s failed (wrong_k "did not fail"));

  ("kstream.internal.enumerate" >:: fun _ ->
    let s = enumerate (of_list ['f'; 'o'; 'o']) in
    to_list s failed_wrong (assert_equal [0, 'f'; 1, 'o'; 2, 'o']));

  ("kstream.internal.enumerate.exn" >:: fun _ ->
    let s = enumerate (then_exn [1; 2; 3]) in
    to_list s failed (wrong_k "did not fail"));

  ("kstream.internal.tail_call" >:: fun _ ->
    let s = make (fun _ _ k -> k 1337) in
    let limit = 100000 in
    fold
      (fun count _ throw k ->
        if count >= limit then throw Exit
        else k (count + 1))
      0 s
      (function
        | Exit -> ()
        | exn -> raise exn)
      (wrong_k "finished"))
]

open Markup

let synchronous_interface_tests = [
  ("kstream.sync.stream,next" >:: fun _ ->
    let emitted = ref false in
    let s =
      stream (fun () ->
        if not !emitted then (emitted := true; Some "foo") else None)
    in
    next s |> assert_equal ~msg:"foo" (Some "foo");
    next s |> assert_equal ~msg:"empty" None);

  ("kstream.sync.of_list" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    next s |> assert_equal ~msg:"1" (Some 1);
    next s |> assert_equal ~msg:"2" (Some 2);
    next s |> assert_equal ~msg:"3" (Some 3);
    next s |> assert_equal ~msg:"empty" None;
    next s |> assert_equal ~msg:"still empty" None);

  ("kstream.sync.to_list" >:: fun _ ->
    of_list [1; 2; 3] |> to_list |> assert_equal [1; 2; 3]);

  ("kstream.sync.peek" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek s |> assert_equal ~msg:"1" (Some 1);
    peek s |> assert_equal ~msg:"1 again" (Some 1);
    next s |> ignore;
    peek s |> assert_equal ~msg:"2" (Some 2);
    to_list s |> ignore;
    peek s |> assert_equal ~msg:"empty" None);

  ("kstream.sync.fold" >:: fun _ ->
    of_list [1; 2; 3] |> fold (+) 0 |> assert_equal 6);

  ("kstream.sync.map" >:: fun _ ->
    of_list [1; 2; 3] |> map string_of_int |> to_list
    |> assert_equal ["1"; "2"; "3"]);

  ("kstream.sync.filter" >:: fun _ ->
    of_list [1; 2; 3; 4] |> filter (fun v -> v mod 2 = 0) |> to_list
    |> assert_equal [2; 4]);

  ("kstream.sync.filter_map" >:: fun _ ->
    of_list [1; 2; 3; 4] |> filter_map (fun v ->
      if v mod 2 = 0 then Some (string_of_int v) else None)
    |> to_list |> assert_equal ["2"; "4"]);

  ("kstream.sync.iter" >:: fun _ ->
    let sum = ref 0 in
    of_list [1; 2; 3] |> iter (fun v -> sum := !sum + v);
    assert_equal !sum 6);

  ("kstream.sync.drain" >:: fun _ ->
    let s = of_list [1; 2; 3] in
    peek s |> assert_equal ~msg:"not empty" (Some 1);
    drain s;
    peek s |> assert_equal ~msg:"empty" None);

  ("kstream.sync.exn" >:: fun _ ->
    let s = (fun () -> raise exn) |> stream in
    try drain s;
    with exn' -> assert_equal exn' exn)
]

let tests = internal_tests @ synchronous_interface_tests
