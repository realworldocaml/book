open! Import

let%expect_test "hash coherence" =
  check_hash_coherence [%here] (module Bool) [ false; true ];
  [%expect {| |}]
;;

let%expect_test "Bool.Non_short_circuiting.(||)" =
  let ( || ) = Bool.Non_short_circuiting.( || ) in
  assert (true || true);
  assert (true || false);
  assert (false || true);
  assert (not (false || false));
  assert (
    true
    ||
    (print_endline "rhs";
     true));
  [%expect {|rhs|}];
  assert (
    false
    ||
    (print_endline "rhs";
     true));
  [%expect {|rhs|}]
;;

let%expect_test "Bool.Non_short_circuiting.(&&)" =
  let ( && ) = Bool.Non_short_circuiting.( && ) in
  assert (true && true);
  assert (not (true && false));
  assert (not (false && true));
  assert (not (false && false));
  assert (
    true
    &&
    (print_endline "rhs";
     true));
  [%expect {|rhs|}];
  assert (
    not
      (false
       &&
       (print_endline "rhs";
        true)));
  [%expect {|rhs|}]
;;
