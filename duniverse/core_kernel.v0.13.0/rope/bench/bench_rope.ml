open! Core_kernel
open! Rope

(* This set of benchmarks can trigger some pathological GC behavior where compaction
   takes ~70% of the time.
   In particuar the "long" benchmarks in [d9eaee43371c] (since removed) were doing that.
*)
let () = Gc.disable_compaction ~allocation_policy:`Don't_change ()

let construct shape rope =
  let result =
    match shape with
    | `Balanced ->
      let rec go = function
        | 0 -> rope
        | n -> go (n - 1) ^ go (n - 1)
      in
      go 11
    | (`Right_skewed | `Left_skewed) as shape ->
      let combine a b =
        match shape with
        | `Right_skewed -> a ^ b
        | `Left_skewed -> b ^ a
      in
      let rec go acc = function
        | 0 -> acc
        | n -> go (combine acc rope) (n - 1)
      in
      go (of_string "") 2048
  in
  (* We check the num_bases to confirm that the comparison with [small_string_list] is a
     fair one. *)
  [%test_result: int]
    ~expect:(For_testing.num_bases rope * 2048)
    (For_testing.num_bases result);
  result
;;

let construct' shape string = construct shape (of_string string)
let small_string = "bla"
let small_left = construct' `Left_skewed small_string
let small_right = construct' `Right_skewed small_string
let small_balanced = construct' `Balanced small_string
let small_left_composite = construct `Left_skewed (of_string "b" ^ of_string "la")
let small_right_composite = construct `Left_skewed (of_string "b" ^ of_string "la")
let small_string_list = List.init 2048 ~f:(const small_string)

(* We need this for the [%test_result] thing above to actually cause compilation to fail.
   Otherwise you only notice when actually running the benchmarks. *)
let%test_unit "module does not raise" = ()

let%bench "small String.concat" =
  (* benchmark to compare against *)
  String.concat small_string_list
;;

let%bench "small List.rev + String.concat" =
  (* This test is intended to correspond roughly to [Pipe.to_list >>| String.concat],
     which produces the same result as
     {[
       Pipe.fold_without_pushback ~init:Rope.empty
         ~f:(fun rope string -> Rope.(^) rope (Rope.of_string string))
     ]}
  *)
  String.concat (List.rev small_string_list)
;;

let%bench "small on the right -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall small_right
;;

let%bench "small on the right -> to_string" = to_string small_right

let%bench "small on the left -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall small_left
;;

let%bench "small on the left -> to_string" = to_string small_left

let%bench "small-composite on the left -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall small_left_composite
;;

let%bench "small-composite on the left -> to_string" = to_string small_left_composite

let%bench "small-composite on the right -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall small_right_composite
;;

let%bench "small-composite on the right -> to_string" = to_string small_right_composite

let%bench "small balanced -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall small_balanced
;;

let%bench "small balanced -> to_string" = to_string small_balanced

let medium_string =
  "lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor sit amet"
;;

let medium_left = construct' `Left_skewed medium_string
let medium_right = construct' `Right_skewed medium_string
let medium_balanced = construct' `Balanced medium_string
let medium_string_list = List.init 2048 ~f:(const medium_string)

let%bench "medium String.concat" = String.concat medium_string_list
let%bench "medium List.rev + String.concat" = String.concat (List.rev medium_string_list)

let%bench "medium on the right -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall medium_right
;;

let%bench "medium on the right -> to_string" = to_string medium_right

let%bench "medium on the left -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall medium_left
;;

let%bench "medium on the left -> to_string" = to_string medium_left

let%bench "medium balanced -> For_testing.to_string_tailcall" =
  For_testing.to_string_tailcall medium_balanced
;;

let%bench "medium balanced -> to_string" = to_string medium_balanced

(* We used to have some 'long' benchmarks here, but they were quite slow and didn't
   seem to tell us anything helpful. *)
