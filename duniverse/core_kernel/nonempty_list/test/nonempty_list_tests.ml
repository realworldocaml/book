open Core
open Composition_infix
open Nonempty_list

include (
  Base_test_helpers.Test_container.Test_S1_allow_skipping_tests (struct
    include Nonempty_list

    let of_list l = if List.is_empty l then `Skip_test else `Ok (of_list_exn l)
  end) :
  sig end)

(* Note that at least one expect test must be included for the above functor's tests to
   run.
*)

let t : _ Nonempty_list.t = [ 1; 2; 3; 4 ]

let%expect_test "sexp representations" =
  print_s [%sexp (t : int Nonempty_list.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V2.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V1.t)];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "reduce" =
  print_s [%sexp (reduce t ~f:( + ) : int)];
  [%expect {| 10 |}]
;;

let%expect_test "reverse" =
  print_s [%sexp (reverse t : int t)];
  [%expect {| (4 3 2 1) |}]
;;

let%expect_test "append" =
  print_s [%sexp (append t [ 5; 6; 7 ] : int t)];
  [%expect {| (1 2 3 4 5 6 7) |}]
;;

let%expect_test "unzip" =
  let t = [ 1, 'a'; 2, 'b'; 3, 'c'; 4, 'd' ] in
  print_s [%sexp (unzip t : int t * char t)];
  [%expect {| ((1 2 3 4) (a b c d)) |}]
;;

let%expect_test "return" =
  print_s [%sexp (return 1 : int t)];
  [%expect {| (1) |}]
;;

let%expect_test "bind" =
  let t = [ 1; 2; 3 ] >>= (List.init ~f:Fn.id >> of_list_exn) in
  print_s [%sexp (t : int t)];
  [%expect {| (0 0 1 0 1 2) |}]
;;

let%expect_test "zip" =
  let test a b = print_s [%sexp (zip a b : (int * int) t List.Or_unequal_lengths.t)] in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok ((1 2)))
    (Ok ((1 3) (2 4)))
    Unequal_lengths
    Unequal_lengths |}]
;;

let%expect_test "zip_exn" =
  let test a b =
    print_s [%sexp (Or_error.try_with (fun () -> zip_exn a b) : (int * int) t Or_error.t)]
  in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok ((1 2)))
    (Ok ((1 3) (2 4)))
    (Error (Invalid_argument "length mismatch in zip_exn: 1 <> 2"))
    (Error (Invalid_argument "length mismatch in zip_exn: 2 <> 1")) |}]
;;

let%expect_test "concat" =
  let test lists = print_s [%sexp (concat lists : int t)] in
  test [ [ 1 ] ];
  test [ [ 1 ]; [ 2 ]; [ 3 ] ];
  test [ [ 1; 2; 3 ] ];
  test [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ];
  [%expect {|
    (1)
    (1 2 3)
    (1 2 3)
    (1 2 3 4 5 6) |}]
;;

let%expect_test "last" =
  let test list = print_s [%sexp (last list : int)] in
  test [ 1 ];
  test [ 1; 2 ];
  test [ 1; 2; 3 ];
  [%expect {|
    1
    2
    3 |}]
;;

let%expect_test "drop_last" =
  let test list = print_s [%sexp (drop_last list : int list)] in
  test [ 1 ];
  test [ 1; 2 ];
  test [ 1; 2; 3 ];
  [%expect {|
    ()
    (1)
    (1 2) |}]
;;

let%expect_test "to_sequence" =
  let test t = print_s [%sexp (to_sequence t : int Sequence.t)] in
  test [ 1 ];
  test [ 1; 2; 3 ];
  test [ 0; 2; 4; 6 ];
  [%expect {|
    (1)
    (1 2 3)
    (0 2 4 6) |}]
;;

let%expect_test "sort" =
  let test t = print_s [%sexp (sort ~compare:Int.compare t : int t)] in
  test [ 1 ];
  test [ 2; 4; 1; 4 ];
  [%expect {|
    (1)
    (1 2 4 4) |}]
;;

let%expect_test "stable_sort" =
  let test t =
    print_s
      [%sexp
        (stable_sort ~compare:(Comparable.lift ~f:fst Int.compare) t : (int * string) t)]
  in
  test [ 1, "_" ];
  test [ 2, "_"; 4, "a"; 1, "_"; 4, "b" ];
  test [ 2, "_"; 4, "b"; 1, "_"; 4, "a" ];
  [%expect {|
    ((1 _))
    ((1 _) (2 _) (4 a) (4 b))
    ((1 _) (2 _) (4 b) (4 a)) |}]
;;

let%expect_test "dedup_and_sort" =
  let test t = print_s [%sexp (dedup_and_sort ~compare:Int.compare t : int t)] in
  test [ 1 ];
  test [ 2; 4; 1; 4 ];
  [%expect {|
    (1)
    (1 2 4) |}]
;;

let%expect_test "min_elt' max_elt'" =
  let compare = Int.compare in
  let l = [ 2; 3; 1; 4 ] in
  print_s [%sexp (min_elt' ~compare l : int)];
  [%expect {| 1 |}];
  print_s [%sexp (max_elt' ~compare l : int)];
  [%expect {| 4 |}]
;;

let%expect_test "map_of_alist_multi" =
  let test alist =
    print_s [%sexp (map_of_alist_multi alist ~comparator:(module Int) : int t Int.Map.t)]
  in
  test [];
  test [ 0, 0; 0, 1; 1, 1 ];
  [%expect {|
    ()
    ((0 (1 0)) (1 (1))) |}]
;;

let%expect_test "map_of_sequence_multi" =
  let test alist =
    print_s
      [%sexp (map_of_sequence_multi alist ~comparator:(module Int) : int t Int.Map.t)]
  in
  test Sequence.empty;
  test (Sequence.of_list [ 0, 0; 0, 1; 1, 1 ]);
  [%expect {|
    ()
    ((0 (1 0)) (1 (1))) |}]
;;

let%expect_test "combine_errors" =
  let test rs = print_s [%sexp (combine_errors rs : (int t, int t) Result.t)] in
  test [ Ok 1 ];
  [%expect {| (Ok (1)) |}];
  test [ Error 1 ];
  [%expect {| (Error (1)) |}];
  test [ Ok 1; Error 2 ];
  [%expect {| (Error (2)) |}];
  test [ Error 1; Ok 2 ];
  [%expect {| (Error (1)) |}];
  test [ Ok 1; Ok 2; Ok 3; Ok 4 ];
  [%expect {| (Ok (1 2 3 4)) |}];
  test [ Error 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (1 2 3 4)) |}];
  test [ Ok 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (2 3 4)) |}]
;;

let%expect_test "combine_errors_unit" =
  let test rs = print_s [%sexp (combine_errors_unit rs : (unit, int t) Result.t)] in
  test [ Ok () ];
  [%expect {| (Ok ()) |}];
  test [ Error 1 ];
  [%expect {| (Error (1)) |}];
  test [ Ok (); Error 2 ];
  [%expect {| (Error (2)) |}];
  test [ Error 1; Ok () ];
  [%expect {| (Error (1)) |}];
  test [ Ok (); Ok (); Ok (); Ok () ];
  [%expect {| (Ok ()) |}];
  test [ Error 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (1 2 3 4)) |}];
  test [ Ok (); Error 2; Error 3; Error 4 ];
  [%expect {| (Error (2 3 4)) |}]
;;

let%expect_test "basic accessor" =
  Accessor.iteri Nonempty_list_accessor.eachi t ~f:(fun [ i ] x ->
    printf "index: %d, value %d\n" i x);
  [%expect
    {|
    index: 0, value 1
    index: 1, value 2
    index: 2, value 3
    index: 3, value 4 |}]
;;

(* Demonstrate how accessors allow types to be easily commuted: Here a ['a Or_error.t
   Nonempty_list.t] becomes a ['a Nonempty_list Or_error.t]. *)
let%expect_test "commute types" =
  let commute results = Accessor_base.Or_error.all Nonempty_list_accessor.each results in
  print_s [%sexp (commute [ Ok 1; Ok 2; Ok 3 ] : int t Or_error.t)];
  [%expect {| (Ok (1 2 3)) |}];
  print_s [%sexp (commute [ Ok 1; Ok 2; Or_error.error_string "foo" ] : int t Or_error.t)];
  [%expect {| (Error foo) |}]
;;

let%expect_test "stable types" =
  let test sexp_of_t t_of_sexp =
    let sexp = sexp_of_t sexp_of_int t in
    print_s sexp;
    print_s ([%sexp_of: bool] ([%equal: int t] t (t_of_sexp int_of_sexp sexp)))
  in
  test Stable.V1.sexp_of_t Stable.V1.t_of_sexp;
  test Stable.V2.sexp_of_t Stable.V2.t_of_sexp;
  test Stable.V3.sexp_of_t Stable.V3.t_of_sexp;
  test sexp_of_t t_of_sexp;
  [%expect
    {|
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true |}];
  let test bin_writer_t bin_read_t =
    let bytes = Bin_prot.Writer.to_bytes (bin_writer_t Int.bin_writer_t) t in
    bytes |> [%sexp_of: bytes] |> print_s;
    let buf = Bin_prot.Common.create_buf (Bytes.length bytes) in
    Bin_prot.Common.blit_bytes_buf bytes buf ~len:(Bytes.length bytes);
    let t' = bin_read_t Int.bin_read_t buf ~pos_ref:(ref 0) in
    print_s ([%sexp_of: bool] ([%equal: int t] t t'))
  in
  test Stable.V1.bin_writer_t Stable.V1.bin_read_t;
  test Stable.V2.bin_writer_t Stable.V2.bin_read_t;
  test Stable.V3.bin_writer_t Stable.V3.bin_read_t;
  test Unstable.bin_writer_t Unstable.bin_read_t;
  [%expect
    {|
    "\001\003\002\003\004"
    true
    "\001\003\002\003\004"
    true
    "\004\001\002\003\004"
    true
    "\004\001\002\003\004"
    true |}]
;;

let%expect_test "folds" =
  let module M = struct
    type t =
      | Init
      | F of t * t
      | Leaf of int
    [@@deriving equal, sexp_of, variants]

    let rec flatten = function
      | Init -> []
      | F (a, b) -> List.concat_map [ a; b ] ~f:flatten
      | Leaf i -> [ i ]
    ;;
  end
  in
  let test nums =
    let leaves = Nonempty_list.map nums ~f:M.leaf in
    let reduced = Nonempty_list.reduce leaves ~f:M.f in
    let folded = Nonempty_list.fold leaves ~init:M.init ~f:M.f in
    let folded_right = Nonempty_list.fold_right leaves ~init:M.init ~f:M.f in
    let nums = Nonempty_list.to_list nums in
    assert ([%equal: int list] nums (M.flatten reduced));
    assert ([%equal: int list] nums (M.flatten folded));
    assert ([%equal: int list] nums (M.flatten folded_right));
    print_s [%message (reduced : M.t) (folded : M.t) (folded_right : M.t)]
  in
  test [ 1 ];
  [%expect
    {|
    ((reduced (Leaf 1)) (folded (F Init (Leaf 1)))
     (folded_right (F (Leaf 1) Init)))
  |}];
  test [ 1; 2 ];
  [%expect
    {|
    ((reduced (F (Leaf 1) (Leaf 2))) (folded (F (F Init (Leaf 1)) (Leaf 2)))
     (folded_right (F (Leaf 1) (F (Leaf 2) Init))))
  |}];
  test [ 1; 2; 3 ];
  [%expect
    {|
    ((reduced (F (F (Leaf 1) (Leaf 2)) (Leaf 3)))
     (folded (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)))
     (folded_right (F (Leaf 1) (F (Leaf 2) (F (Leaf 3) Init)))))
  |}];
  test [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ];
  [%expect
    {|
    ((reduced
      (F
       (F
        (F
         (F (F (F (F (F (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5))
          (Leaf 6))
         (Leaf 7))
        (Leaf 8))
       (Leaf 9)))
     (folded
      (F
       (F
        (F
         (F (F (F (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5))
          (Leaf 6))
         (Leaf 7))
        (Leaf 8))
       (Leaf 9)))
     (folded_right
      (F (Leaf 1)
       (F (Leaf 2)
        (F (Leaf 3)
         (F (Leaf 4)
          (F (Leaf 5) (F (Leaf 6) (F (Leaf 7) (F (Leaf 8) (F (Leaf 9) Init)))))))))))
  |}];
  ()
;;

let%expect_test "folding_map" =
  print_s
    [%sexp
      (folding_map [ 1; 2; 3; 4; 5; 6 ] ~init:0 ~f:(fun acc x -> acc + x, acc) : int t)];
  [%expect {| (0 1 3 6 10 15) |}]
;;

let%expect_test "mapi" =
  print_s [%sexp (mapi [ "a"; "b"; "c"; "d" ] ~f:(fun i x -> i, x) : (int * string) t)];
  [%expect {| ((0 a) (1 b) (2 c) (3 d)) |}]
;;

let%expect_test "rev_append" =
  let test xs ys = rev_append xs ys |> [%sexp_of: int Nonempty_list.t] |> print_s in
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| (3 2 1 4) |}];
  test [ 1; 2 ] [ 3; 4 ];
  [%expect {| (2 1 3 4) |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}];
  test [] [ 1; 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "Reversed.rev" =
  let test xs = Reversed.rev xs |> [%sexp_of: int Nonempty_list.t] |> print_s in
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ];
  [%expect {| (3 2 1) |}];
  test [ 1; 2 ];
  [%expect {| (2 1) |}];
  test [ 1 ];
  [%expect {| (1) |}]
;;

let%expect_test "Reversed.rev_append" =
  let test xs ys =
    Reversed.rev_append xs ys |> [%sexp_of: int Nonempty_list.t] |> print_s
  in
  test [ 1; 2; 3; 4 ] [];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| (3 2 1 4) |}];
  test [ 1; 2 ] [ 3; 4 ];
  [%expect {| (2 1 3 4) |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "Reversed.rev_map" =
  let test xs =
    Reversed.rev_map ~f:Int.to_string xs |> [%sexp_of: string Nonempty_list.t] |> print_s
  in
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ];
  [%expect {| (3 2 1) |}];
  test [ 1; 2 ];
  [%expect {| (2 1) |}];
  test [ 1 ];
  [%expect {| (1) |}]
;;

let%expect_test "Reversed.rev_mapi" =
  let test xs =
    Reversed.rev_mapi xs ~f:(fun i n -> sprintf "%d/%d" n i)
    |> [%sexp_of: string Nonempty_list.t]
    |> print_s
  in
  test [ 5; 6; 7; 8 ];
  [%expect {| (8/3 7/2 6/1 5/0) |}];
  test [ 5; 6; 7 ];
  [%expect {| (7/2 6/1 5/0) |}];
  test [ 5; 6 ];
  [%expect {| (6/1 5/0) |}];
  test [ 5 ];
  [%expect {| (5/0) |}]
;;

(* Since the behavior of [Option] functions differs fundamentally between empty and
   non-empty lists, explicitly ensure that we test both cases in addition to whatever
   tests quickcheck generates (even though it's extremely likely that the quickcheck tests
   do contain empty lists. *)
let run_on_empty_and_nonempty_lists f =
  Quickcheck.test [%quickcheck.generator: int list] ~examples:[ []; [ 1 ] ] ~f
;;

let%expect_test "Option" =
  run_on_empty_and_nonempty_lists (fun l ->
    match%optional.Nonempty_list.Option l with
    | None -> assert (List.is_empty l)
    | Some nonempty ->
      assert ([%equal: int Nonempty_list.t] nonempty (Nonempty_list.of_list_exn l)))
;;

let%expect_test "Option does not allocate" =
  run_on_empty_and_nonempty_lists (fun l ->
    let round_tripped =
      Expect_test_helpers_core.require_no_allocation [%here] (fun () ->
        match%optional.Nonempty_list.Option l with
        | None -> Sys.opaque_identity Nonempty_list.Option.none
        | Some nonempty ->
          Sys.opaque_identity
            (Nonempty_list.Option.some (Sys.opaque_identity nonempty)))
    in
    assert (phys_equal l round_tripped))
;;
