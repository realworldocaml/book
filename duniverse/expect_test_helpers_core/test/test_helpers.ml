open! Core
open! Import

let%expect_test "[print_and_check_stable_type] shows [Shape.Digest] even for empty \
                 examples"
  =
  print_and_check_stable_type [%here] (module Int) [];
  [%expect {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac) |}]
;;

let%expect_test "[print_and_check_stable_type]" =
  print_and_check_stable_type [%here] (module Int) [ 0; 21; Int.max_value_30_bits ];
  [%expect
    {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   21)
     (bin_io "\021"))
    ((sexp   1073741823)
     (bin_io "\253\255\255\255?")) |}]
;;

let%expect_test "[print_and_check_stable_type] with broken round-trip" =
  let module Broken = struct
    type t = int [@@deriving compare]

    let to_serializeable t = Int.to_string_hum t
    let of_serializeable _ = 42

    include
      Sexpable.Of_sexpable
        (String)
        (struct
          type t = int

          let to_sexpable = to_serializeable
          let of_sexpable = of_serializeable
        end)

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (String)
        (struct
          type t = int

          let to_binable = to_serializeable
          let of_binable = of_serializeable
        end)
  end
  in
  print_and_check_stable_type [%here] ~cr:Comment (module Broken) [ 42; 23 ];
  [%expect
    {|
    (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
    ((sexp   42)
     (bin_io "\00242"))
    ((sexp   23)
     (bin_io "\00223"))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       23)
      (sexp           23)
      (sexp_roundtrip 42))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("bin-io serialization failed to round-trip"
     (original         23)
     (bin_io           "\00223")
     (bin_io_roundtrip 42)) |}]
;;

let%expect_test "[print_and_check_stable_type] with exceeded max-length" =
  print_and_check_stable_type
    [%here]
    ~cr:Comment
    (module Int)
    [ 0; Int.max_value_30_bits ]
    ~max_binable_length:1;
  [%expect
    {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   0)
     (bin_io "\000"))
    ((sexp   1073741823)
     (bin_io "\253\255\255\255?"))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("bin-io serialization exceeds max binable length"
     (original           1073741823)
     (bin_io             "\253\255\255\255?")
     (bin_io_length      5)
     (max_binable_length 1)) |}]
;;

let%expect_test "[print_and_check_stable_type] with conversion that raises" =
  let module Broken = struct
    type t = int [@@deriving sexp, bin_io]

    let compare x y = raise_s [%message "compare" (x : int) (y : int)]
  end
  in
  print_and_check_stable_type [%here] ~cr:Comment (module Broken) [ 1; 2; 3 ];
  [%expect
    {|
    (bin_shape_digest 698cfa4093fe5e51523842d37b92aeac)
    ((sexp   1)
     (bin_io "\001"))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" (
      compare
      (x 1)
      (y 1))) |}]
;;

let%expect_test "[print_s] bug, apparently" =
  "(\"sets are not equal\"(first (1 2))(second (2))(\"in first but not in second\"(1)))"
  |> Sexp.of_string
  |> print_s;
  [%expect
    {|
    ("sets are not equal"
      (first (1 2))
      (second                       (2))
      ("in first but not in second" (1))) |}]
;;

let%expect_test "[require_no_allocation] ignores non-allocating functions" =
  require_no_allocation [%here] (fun () -> ());
  [%expect {| |}]
;;

let%expect_test ("[require_no_allocation] shows breach and expected, but does not show \
                  allocation" [@tags "no-js"])
  =
  ignore
    (Expect_test_helpers_core_private.require_allocation_does_not_exceed
       ~cr:Comment
       (Minor_words 0)
       [%here]
       (fun () -> (List.map [@inlined never]) [ 1; 2; 3 ] ~f:(fun i -> i + 1))
     : int list);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("allocation exceeded limit" (allocation_limit (Minor_words 0))) |}]
;;

let%expect_test ("[require_allocation_does_not_exceed] shows breach but not allocation" [@tags
                   "no-js"])
  =
  ignore
    (Expect_test_helpers_core_private.require_allocation_does_not_exceed
       ~cr:Comment
       (Minor_words 1)
       [%here]
       (fun () -> (List.map [@inlined never]) [ 1; 2; 3 ] ~f:(fun i -> i + 1))
     : int list);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("allocation exceeded limit" (allocation_limit (Minor_words 1))) |}]
;;

let%expect_test "[print_and_check_container_sexps] success" =
  print_and_check_container_sexps [%here] (module Int) [ 1; 10; 100 ];
  [%expect
    {|
    (Set (1 10 100))
    (Map (
      (1   0)
      (10  1)
      (100 2)))
    (Hash_set (1 10 100))
    (Table (
      (1   0)
      (10  1)
      (100 2))) |}]
;;

let%expect_test "[print_and_check_container_sexps] failure" =
  print_and_check_container_sexps
    ~cr:Comment
    [%here]
    (module struct
      include Int

      let sexp_of_t = Int.Hex.sexp_of_t
    end)
    [ 1; 10; 100 ];
  [%expect
    {|
    (Set (1 10 100))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("set sexp does not match sorted list sexp"
      (set_sexp         (1   10  100))
      (sorted_list_sexp (0x1 0xa 0x64)))
    (Map (
      (1   0)
      (10  1)
      (100 2)))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("map sexp does not match sorted alist sexp"
     (map_sexp (
       (1   0)
       (10  1)
       (100 2)))
     (sorted_alist_sexp (
       (0x1  0)
       (0xa  1)
       (0x64 2))))
    (Hash_set (1 10 100))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("hash_set sexp does not match sorted list sexp"
     (hash_set_sexp    (1   10  100))
     (sorted_list_sexp (0x1 0xa 0x64)))
    (Table (
      (1   0)
      (10  1)
      (100 2)))
    (* require-failed: lib/expect_test_helpers/core/test/test_helpers.ml:LINE:COL. *)
    ("table sexp does not match sorted alist sexp"
     (table_sexp (
       (1   0)
       (10  1)
       (100 2)))
     (sorted_alist_sexp (
       (0x1  0)
       (0xa  1)
       (0x64 2)))) |}]
;;

let%expect_test "remove_time_span" =
  remove_time_spans "hello240ms 300ms world 8.4s 0.1d .9h 30m122320002ns .0002us"
  |> print_endline;
  [%expect "hello240ms SPAN world SPAN SPAN .SPAN SPAN .SPAN"]
;;
