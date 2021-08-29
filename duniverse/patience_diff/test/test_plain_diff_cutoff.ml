open! Base
open Import

let%expect_test "exercise cutoff code path" =
  let arr1 = [| 0; 1; 2; 3; 4; 5; 6; 42; 43; 7; 8; 9 |] in
  let arr2 = [| 9; 8; 7; 42; 43; 6; 5; 4; 3; 2; 1; 0 |] in
  let do_diff ~cutoff =
    Plain_diff.iter_matches
      ?cutoff
      arr1
      arr2
      ~f:(fun (i, j) ->
        assert (arr1.(i) = arr2.(j));
        print_s [%sexp (arr1.(i) : int), (i, j : int * int)])
      ~hashable:(module Int)
  in
  do_diff ~cutoff:None;
  [%expect {|
    (42 (7 3))
    (43 (8 4)) |}];
  do_diff ~cutoff:(Some 3);
  (* worse diff, but correct *)
  [%expect {| (9 (11 0)) |}]
;;
