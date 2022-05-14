open! Core
open! Expect_test_helpers_base

let map = List.init 20 ~f:(fun i -> i + 1, (i + 1) * 100) |> Int.Map.of_alist_exn

let%expect_test "test with diff_printer" =
  (* The diff printer makes each output after the first short and simple. *)
  let show map = print_s [%sexp (map : int Int.Map.t)] in
  show map;
  let print_diff =
    Expect_test_patdiff.diff_printer ~context:1 (Some [%expect.output]) |> unstage
  in
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 1000)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:10 ~data:999 in
  show map;
  print_diff [%expect.output];
  [%expect
    {|
    -9,3 +9,3
       (9  900)
    -| (10 1000)
    +| (10 999)
       (11 1100) |}];
  let map = Map.set map ~key:0 ~data:(-1) in
  show map;
  print_diff [%expect.output];
  [%expect
    {|
    -1,2 +1,3
    -|((1  100)
    +|((0  -1)
    +| (1  100)
       (2  200) |}]
;;

let%expect_test "test with diff_printer_s" =
  (* The diff printer makes each output after the first short and simple. *)
  let print_diff =
    let diff_printer =
      Expect_test_patdiff.diff_printer_s ~context:1 (Some [%sexp (map : int Int.Map.t)])
      |> unstage
    in
    fun map -> diff_printer [%sexp (map : int Int.Map.t)]
  in
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 1000)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:10 ~data:999 in
  print_diff map;
  [%expect
    {|
    -9,3 +9,3
       (9  900)
    -| (10 1000)
    +| (10 999)
       (11 1100) |}];
  let map = Map.set map ~key:0 ~data:(-1) in
  print_diff map;
  [%expect
    {|
    -1,2 +1,3
    -|((1  100)
    +|((0  -1)
    +| (1  100)
       (2  200) |}]
;;

let%expect_test "test without diff_printer or diff_printer_s" =
  (* Without the diff printer, output is verbose and finding differences is tedious. *)
  let show map = print_s [%sexp (map : int Int.Map.t)] in
  show map;
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 1000)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:10 ~data:999 in
  show map;
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 999)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:0 ~data:(-1) in
  show map;
  [%expect
    {|
    ((0  -1)
     (1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 999)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}]
;;

let%expect_test "passing [None] to diff_printer behaves like diff_printer, but delays \
                 the initial print until the first use of the callback"
  =
  let show map = print_s [%sexp (map : int Int.Map.t)] in
  let print_diff = Expect_test_patdiff.diff_printer ~context:1 None |> unstage in
  [%expect {||}];
  show map;
  print_diff [%expect.output];
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 1000)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:10 ~data:999 in
  show map;
  print_diff [%expect.output];
  [%expect
    {|
    -9,3 +9,3
       (9  900)
    -| (10 1000)
    +| (10 999)
       (11 1100) |}];
  let map = Map.set map ~key:0 ~data:(-1) in
  show map;
  print_diff [%expect.output];
  [%expect
    {|
    -1,2 +1,3
    -|((1  100)
    +|((0  -1)
    +| (1  100)
       (2  200) |}]
;;

let%expect_test "passing [None] to diff_printer_s behaves like diff_printer_s, but \
                 delays the initial print until the first use of the callback"
  =
  let print_diff =
    let diff_printer = Expect_test_patdiff.diff_printer_s ~context:1 None |> unstage in
    fun map -> diff_printer [%sexp (map : int Int.Map.t)]
  in
  [%expect {||}];
  print_diff map;
  [%expect
    {|
    ((1  100)
     (2  200)
     (3  300)
     (4  400)
     (5  500)
     (6  600)
     (7  700)
     (8  800)
     (9  900)
     (10 1000)
     (11 1100)
     (12 1200)
     (13 1300)
     (14 1400)
     (15 1500)
     (16 1600)
     (17 1700)
     (18 1800)
     (19 1900)
     (20 2000)) |}];
  let map = Map.set map ~key:10 ~data:999 in
  print_diff map;
  [%expect
    {|
    -9,3 +9,3
       (9  900)
    -| (10 1000)
    +| (10 999)
       (11 1100) |}];
  let map = Map.set map ~key:0 ~data:(-1) in
  print_diff map;
  [%expect
    {|
    -1,2 +1,3
    -|((1  100)
    +|((0  -1)
    +| (1  100)
       (2  200) |}]
;;
