open! Import
open Ref

let%test_unit "[set_temporarily] without raise" =
  let r = ref 0 in
  [%test_result: int] ~expect:1 (set_temporarily r 1 ~f:(fun () -> !r));
  [%test_result: int] ~expect:0 !r
;;

let%test_unit "[set_temporarily] with raise" =
  let r = ref 0 in
  try Nothing.unreachable_code (set_temporarily r 1 ~f:(fun () -> failwith "")) with
  | _ -> [%test_result: int] ~expect:0 !r
;;

let%test_unit "[set_temporarily] where [f] sets the ref" =
  let r = ref 0 in
  set_temporarily r 1 ~f:(fun () -> r := 2);
  [%test_result: int] ~expect:0 !r
;;

let%expect_test "[sets_temporarily] without raise" =
  let r1 = ref 1 in
  let r2 = ref 2 in
  let test and_values =
    let i1 = !r1 in
    let i2 = !r2 in
    sets_temporarily and_values ~f:(fun () ->
      print_s [%message (r1 : int ref) (r2 : int ref)]);
    require_equal
      [%here]
      (module struct
        type t = int * int [@@deriving equal, sexp_of]
      end)
      (!r1, !r2)
      (i1, i2)
  in
  test [];
  [%expect {|
    ((r1 1)
     (r2 2)) |}];
  test [ T (r1, 13) ];
  [%expect {|
    ((r1 13)
     (r2 2)) |}];
  test [ T (r1, 13); T (r1, 17) ];
  [%expect {|
    ((r1 17)
     (r2 2)) |}];
  test [ T (r1, 13); T (r2, 17) ];
  [%expect {|
    ((r1 13)
     (r2 17)) |}]
;;

let%expect_test "[sets_temporarily] with raise" =
  let r = ref 0 in
  (try
     Nothing.unreachable_code (sets_temporarily [ T (r, 1) ] ~f:(fun () -> failwith ""))
   with
   | _ -> print_s [%message (r : int ref)]);
  [%expect {| (r 0) |}]
;;
