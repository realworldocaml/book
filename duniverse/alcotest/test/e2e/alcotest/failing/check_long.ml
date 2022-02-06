open Alcotest_stdlib_ext

(** These tests check the wrapping behaviour of [Alcotest.check]'s emitted diff. *)

let id_case typ typ_str v1 v2 =
  Alcotest.test_case typ_str `Quick (fun () -> Alcotest.check typ typ_str v1 v2)

let test_char_list =
  let alphabet = List.init 26 (fun i -> Char.chr (0x61 + i)) in
  id_case Alcotest.(list char) "list" (alphabet @ [ 'A' ]) (alphabet @ [ 'B' ])

let test_int64_array =
  let ns = Array.init 100 Int64.of_int in
  id_case
    Alcotest.(array int64)
    "array"
    (Array.append ns Int64.[| max_int |])
    (Array.append ns Int64.[| min_int |])

type with_testables = E : 'a Alcotest.testable * 'a * 'a -> with_testables

(* Take two values and wrap them in [n]-many [Some]s. *)
let nested_option n : with_testables -> with_testables =
 fun t ->
  let rec inner = function
    | 0 -> t
    | n ->
        let (E (typ', v1', v2')) = inner (n - 1) in
        E (Alcotest.option typ', Some v1', Some v2')
  in
  inner n

let test_nested_options =
  let (E (typ, v1, v2)) = nested_option 35 (E (Alcotest.int, 1, 2)) in
  id_case typ "nested options" v1 v2

let () =
  let open Alcotest in
  run ~verbose:true __FILE__
    [ ("wrapping", [ test_char_list; test_int64_array; test_nested_options ]) ]
