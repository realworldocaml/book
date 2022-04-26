open Core

(* $MDX part-begin=helper *)
let test_interval i points =
  let in_, out =
    List.partition_tf points ~f:(fun x -> Int_interval.contains i x)
  in
  let to_string l =
    List.map ~f:Int.to_string l |> String.concat ~sep:", "
  in
  print_endline
    (String.concat
       ~sep:"\n"
       [ (if Int_interval.is_empty i then "empty" else "non-empty")
       ; "in:  " ^ to_string in_
       ; "out: " ^ to_string out
       ])
(* $MDX part-end *)

(* $MDX part-begin=ordinary *)
let%expect_test "ordinary interval" =
  test_interval (Int_interval.create 3 6) (List.range 1 10);
  [%expect
    {|
    non-empty
    in:  3, 4, 5, 6
    out: 1, 2, 7, 8, 9 |}]
(* $MDX part-end *)

(* $MDX part-begin=empty *)
let%expect_test "empty interval" =
  test_interval (Int_interval.create 6 3) (List.range 1 10);
  [%expect {|
    empty
    in:
    out: 1, 2, 3, 4, 5, 6, 7, 8, 9 |}]
(* $MDX part-end *)

(* $MDX part-begin=sexp_of *)
let%expect_test "test to_sexp" =
  let t lo hi =
    let i = Int_interval.create lo hi in
    print_s [%sexp (i : Int_interval.t)]
  in
  t 3 6;
  [%expect {| (Range 3 6) |}];
  t 4 4;
  [%expect {| (Range 4 4) |}];
  t 6 3;
  [%expect {| Empty |}]
(* $MDX part-end *)

(* $MDX part-begin=of_sexp *)
let%expect_test "test (range 6 3)" =
  let i = Int_interval.t_of_sexp (Sexp.of_string "(Range 6 3)") in
  test_interval i (List.range 1 10);
  [%expect
    {|
    empty
    in:
    out: 1, 2, 3, 4, 5, 6, 7, 8, 9 |}]
(* $MDX part-end *)
