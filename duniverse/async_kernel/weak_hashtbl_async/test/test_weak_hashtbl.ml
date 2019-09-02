open! Core
open! Async
open! Import
open! Weak_hashtbl_async

let%test_unit _ =
  (* automatic reclamation, multiple times *)
  Thread_safe.block_on_async_exn (fun () ->
    let t = create (module Int) in
    let heap_block i = Heap_block.create_exn (ref i) in
    let b1 = heap_block 1 in
    let key1 = 13 in
    let key2 = 14 in
    ignore (find_or_add t key1 ~default:(fun () -> b1) : _ Heap_block.t);
    ignore (find_or_add t key2 ~default:(fun () -> heap_block 2) : _ Heap_block.t);
    Gc.full_major ();
    after (sec 0.)
    (* let a cycle happen, to do the reclamation *)
    >>= fun () ->
    assert (phys_equal (Option.value_exn (find t key1)) b1);
    assert (Option.is_none (find t key2));
    assert (not (key_is_using_space t key2));
    let key3 = 15 in
    ignore (find_or_add t key3 ~default:(fun () -> heap_block 3) : _ Heap_block.t);
    Gc.full_major ();
    after (sec 0.)
    (* let a cycle happen, to do the reclamation *)
    >>= fun () ->
    assert (Option.is_none (find t key3));
    assert (not (key_is_using_space t key3));
    return ())
;;
