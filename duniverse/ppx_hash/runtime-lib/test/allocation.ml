open Core

let words_of_float = if Sys.word_size_in_bits = 64 then 2 else 3

let check_allocation : expect:int -> (unit -> 'a) -> 'a =
  fun ~expect f ->
  (* It costs [fudge] words of allocation to discover the allocation! *)
  let fudge = 18 + (3 * words_of_float) in
  let n0 = Int.of_float (Gc.stat ()).Gc.Stat.minor_words in
  let n1 = Int.of_float (Gc.stat ()).Gc.Stat.minor_words in
  [%test_result: Int.t] ~expect:(n0 + fudge) n1;
  let res = f () in
  let n2 = Int.of_float (Gc.stat ()).Gc.Stat.minor_words in
  let n = n2 - n1 - fudge in
  if n <> expect then failwithf "check_allocation: expect=%d, got=%d" expect n ();
  res
;;

let check_no_allocation f = check_allocation ~expect:0 f

module F (Hash : Base.Hash.S) = struct
  module Ppx_hash_lib = struct
    (* override default which is used by generated code *)
    module Std = struct
      module Hash = Base.Hash.F (Hash)
    end
  end

  include Ppx_hash_lib.Std
  open Hash.Builtin

  type tree =
    | Leaf of int
    | Node of tree * tree
  [@@deriving sexp_of, hash]

  let create_full ~depth =
    assert (depth >= 0);
    let rec build n d =
      if d = 0
      then n + 1, Leaf n
      else (
        let n, t1 = build n (d - 1) in
        let n, t2 = build n (d - 1) in
        n, Node (t1, t2))
    in
    let _, t = build 100 depth in
    t
  ;;

  let the_tree = create_full ~depth:3
end

module Test_alloc (X : sig
    module Hash : Base.Hash.S with type hash_value = int

    val size_of_state : int
    val seed1 : Hash.seed
    val seed2 : Hash.seed
  end) =
struct
  include F (X.Hash)

  let%test_unit _ =
    let state = check_allocation ~expect:X.size_of_state (fun () -> Hash.alloc ()) in
    let _state = check_no_allocation (fun () -> hash_fold_tree state the_tree) in
    ()
  ;;

  let run_seeded seed = Hash.run ~seed hash_fold_tree the_tree

  let%test_unit _ = assert (not (run_seeded X.seed1 = run_seeded X.seed2))

  let%test_unit _ =
    ignore (check_allocation ~expect:X.size_of_state (fun () -> hash_tree the_tree))
  ;;

  let mk_reentrant_folder n =
    let i = ref n in
    let rec loop state x =
      decr i;
      if !i > 0 then ignore (Hash.run loop x);
      let state = hash_fold_tree state x in
      state
    in
    Hash.run loop
  ;;

  let%test_unit _ =
    let n = 100 in
    let reentrant_folder = mk_reentrant_folder n in
    ignore
      (check_allocation ~expect:(n * X.size_of_state) (fun () ->
         reentrant_folder the_tree))
  ;;

  let res1 = hash_tree the_tree
  let res2 = hash_tree the_tree

  let%test_unit "hashing is stable2" = [%test_eq: int] res1 res2
end

module Test_alloc_internalhash = Test_alloc (struct
    module Hash = Base.Hash

    let size_of_state = 0
    let seed1 = 1
    let seed2 = 2
  end)

module Test_alloc_siphash = Test_alloc (struct
    module Hash = Siphash_lib.Siphash

    let size_of_state = if Sys.word_size_in_bits = 64 then 5 else 9
    let seed1 = "1"
    let seed2 = "2"
  end)
