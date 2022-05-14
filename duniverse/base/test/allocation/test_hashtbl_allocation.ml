open! Base
open Expect_test_helpers_core

let () = Int_conversions.sexp_of_int_style := `Underscores

let%expect_test "find_and_call_1_and_2" =
  let test x =
    let t = Hashtbl.create (module Int) ~size:16 ~growth_allowed:false in
    for i = 0 to x - 1 do
      Hashtbl.add_exn t ~key:i ~data:(i * 7)
    done;
    let if_found a b = assert (a = b) in
    let if_not_found a b =
      assert (a = x);
      assert (b = x * 7)
    in
    require_no_allocation [%here] (fun () ->
      for i = 0 to x do
        Hashtbl.find_and_call1 t i ~a:(i * 7) ~if_found ~if_not_found
      done);
    let if_found ~key ~data:a b =
      assert (a = b);
      assert (key = a / 7)
    in
    let if_not_found a b =
      assert (a = x);
      assert (b = x * 7)
    in
    require_no_allocation [%here] (fun () ->
      for i = 0 to x do
        Hashtbl.findi_and_call1 t i ~a:(i * 7) ~if_found ~if_not_found
      done);
    let if_found a b c =
      assert (a = b);
      assert (b = c / 2)
    in
    let if_not_found a b c =
      assert (a = x);
      assert (b = x * 7);
      assert (c = x * 14)
    in
    require_no_allocation [%here] (fun () ->
      for i = 0 to x do
        Hashtbl.find_and_call2 t i ~a:(i * 7) ~b:(i * 14) ~if_found ~if_not_found
      done);
    let if_found ~key ~data:a b c =
      assert (a = b);
      assert (b = c / 2);
      assert (key = a / 7)
    in
    let if_not_found a b c =
      assert (a = x);
      assert (b = x * 7);
      assert (c = x * 14)
    in
    require_no_allocation [%here] (fun () ->
      for i = 0 to x do
        Hashtbl.findi_and_call2 t i ~a:(i * 7) ~b:(i * 14) ~if_found ~if_not_found
      done);
    print_s (Int.sexp_of_t x)
  in
  (* try various load factors, to exercise all branches of matching on the structure of
     the avl tree *)
  test 1;
  test 3;
  test 10;
  test 17;
  test 25;
  test 29;
  test 33;
  test 3133;
  [%expect {|
    1
    3
    10
    17
    25
    29
    33
    3_133 |}]
;;

let%expect_test ("find_or_add shouldn't allocate" [@tags "no-js"]) =
  let default = Fn.const () in
  let t = Hashtbl.create (module Int) ~size:16 ~growth_allowed:false in
  Hashtbl.add_exn t ~key:100 ~data:();
  require_no_allocation [%here] (fun () -> Hashtbl.find_or_add t 100 ~default);
  [%expect {| |}]
;;
