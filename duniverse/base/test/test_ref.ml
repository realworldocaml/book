open! Core_kernel
open Ref

let%test_unit "[set_temporarily] without raise" =
  let r = ref 0 in
  [%test_result: int] ~expect:1 (set_temporarily r 1 ~f:(fun () -> !r));
  [%test_result: int] ~expect:0 !r;
;;

let%test_unit "[set_temporarily] with raise" =
  let r = ref 0 in
  try
    never_returns (set_temporarily r 1 ~f:(fun () -> failwith ""));
  with _ -> [%test_result: int] ~expect:0 !r
;;

let%test_unit "[set_temporarily] where [f] sets the ref" =
  let r = ref 0 in
  set_temporarily r 1 ~f:(fun () -> r := 2);
  [%test_result: int] ~expect:0 !r;
;;
