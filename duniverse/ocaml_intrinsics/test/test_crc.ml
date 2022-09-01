[%%import "config.h"]
[%%ifdef JSC_ARCH_SIXTYFOUR]

open Core
open Expect_test_helpers_core
module P = Ocaml_intrinsics.Crc

let crc data = P.int_crc ~initial:0 ~data
let crc_cumulative ~acc data = P.int_crc ~initial:acc ~data

let%expect_test "crc32" =
  let examples = [ Int.min_value; -1234; -1; 0; 1; 1234; Int.max_value ] in
  List.iter examples ~f:(fun x ->
    let crc = crc x in
    let cumulative_crc = crc_cumulative ~acc:0 x in
    require_equal [%here] (module Int) crc cumulative_crc;
    print_s [%message (x : int) (crc : int) (cumulative_crc : int)]);
  [%expect
    {|
    ((x              -4611686018427387904)
     (crc            3280807620)
     (cumulative_crc 3280807620))
    ((x              -1234)
     (crc            2195985753)
     (cumulative_crc 2195985753))
    ((x              -1)
     (crc            3293575501)
     (cumulative_crc 3293575501))
    ((x              0)
     (crc            0)
     (cumulative_crc 0))
    ((x              1)
     (crc            1228700967)
     (cumulative_crc 1228700967))
    ((x              1234)
     (crc            2649713533)
     (cumulative_crc 2649713533))
    ((x              4611686018427387903)
     (crc            130211721)
     (cumulative_crc 130211721)) |}];
  List.iter examples ~f:(fun x ->
    let acc = 1 in
    let cumulative_crc = crc_cumulative ~acc x in
    print_s [%message (x : int) (acc : int) (cumulative_crc : int)]);
  [%expect
    {|
    ((x              -4611686018427387904)
     (acc            1)
     (cumulative_crc 2326879203))
    ((x              -1234)
     (acc            1)
     (cumulative_crc 3419957374))
    ((x              -1)
     (acc            1)
     (cumulative_crc 2373157994))
    ((x              0)
     (acc            1)
     (cumulative_crc 1228700967))
    ((x              1)
     (acc            1)
     (cumulative_crc 0))
    ((x              1234)
     (acc            1)
     (cumulative_crc 3570603610))
    ((x              4611686018427387903)
     (acc            1)
     (cumulative_crc 1325310638)) |}]
;;

let%expect_test "iterated_crc" =
  let test ~initial ~iterations ~data =
    let crc = P.iterated_crc_exn ~initial ~iterations ~data in
    print_s [%message (initial : int) (iterations : int) (data : int) (crc : int)]
  in
  test ~initial:0 ~iterations:0 ~data:1000;
  [%expect
    {|
    ((initial    0)
     (iterations 0)
     (data       1000)
     (crc        3644803405)) |}];
  test ~initial:0 ~iterations:100 ~data:1000;
  [%expect
    {|
    ((initial    0)
     (iterations 100)
     (data       1000)
     (crc        3734759765)) |}];
  test ~initial:100 ~iterations:100 ~data:1000;
  [%expect
    {|
      ((initial    100)
       (iterations 100)
       (data       1000)
       (crc        350685120)) |}]
;;

let%expect_test "iterated_crc_exn" =
  let test ~initial ~iterations ~data =
    let crc = P.iterated_crc_exn ~initial ~iterations ~data in
    print_s [%message (initial : int) (iterations : int) (data : int) (crc : int)]
  in
  (try
     test ~initial:0 ~iterations:(-1) ~data:1000;
     print_endline "Should have raised Invalid_argument"
   with
   | Invalid_argument s -> print_endline s);
  [%expect {| iterated_crc: iterations=-1 is invalid, must be non-negative value |}]
;;

[%%endif]
