open! Base
open Expect_test_helpers_core

type int_hashtbl = int Hashtbl.M(Int).t [@@deriving sexp]

let%test "Hashtbl.merge succeeds with first-class-module interface" =
  let t1 = Hashtbl.create (module Int) in
  let t2 = Hashtbl.create (module Int) in
  let result =
    Hashtbl.merge t1 t2 ~f:(fun ~key:_ ->
      function
      | `Left x -> x
      | `Right x -> x
      | `Both _ -> assert false)
    |> Hashtbl.to_alist
  in
  List.equal Poly.equal result []
;;

let%test_module _ =
  (module Hashtbl_tests.Make (struct
       include Hashtbl

       let create_poly ?size () = Poly.create ?size ()
       let of_alist_poly_exn l = Poly.of_alist_exn l
       let of_alist_poly_or_error l = Poly.of_alist_or_error l
     end))
;;

let%expect_test "Hashtbl.find_exn" =
  let table = Hashtbl.of_alist_exn (module String) [ "one", 1; "two", 2; "three", 3 ] in
  let test_success key =
    require_does_not_raise [%here] (fun () ->
      print_s [%sexp (Hashtbl.find_exn table key : int)])
  in
  test_success "one";
  [%expect {| 1 |}];
  test_success "two";
  [%expect {| 2 |}];
  test_success "three";
  [%expect {| 3 |}];
  let test_failure key =
    require_does_raise [%here] (fun () -> Hashtbl.find_exn table key)
  in
  test_failure "zero";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" zero)) |}];
  test_failure "four";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" four)) |}]
;;

let%expect_test "[t_of_sexp] error on duplicate" =
  let sexp = Sexplib.Sexp.of_string "((0 a)(1 b)(2 c)(1 d))" in
  (match [%of_sexp: string Hashtbl.M(String).t] sexp with
   | t -> print_cr [%here] [%message "did not raise" (t : string Hashtbl.M(String).t)]
   | exception (Sexp.Of_sexp_error _ as exn) -> print_s (sexp_of_exn exn)
   | exception exn -> print_cr [%here] [%message "wrong kind of exception" (exn : exn)]);
  [%expect {| (Of_sexp_error "Hashtbl.t_of_sexp: duplicate key" (invalid_sexp 1)) |}]
;;

let%expect_test "[choose], [choose_exn]" =
  let test ?size l =
    let t =
      l |> List.map ~f:(fun i -> i, i) |> Hashtbl.of_alist_exn ?size (module Int)
    in
    print_s
      [%message
        ""
          ~input:(t : int_hashtbl)
          ~choose:(Hashtbl.choose t : (_ * _) option)
          ~choose_exn:
            (Or_error.try_with (fun () -> Hashtbl.choose_exn t) : (_ * _) Or_error.t)]
  in
  test [];
  [%expect
    {|
    ((input  ())
     (choose ())
     (choose_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))) |}];
  test [] ~size:100;
  [%expect
    {|
    ((input  ())
     (choose ())
     (choose_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))) |}];
  test [ 1 ];
  [%expect
    {|
    ((input  ((1 1)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))) |}];
  test [ 1 ] ~size:100;
  [%expect
    {|
    ((input  ((1 1)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))) |}];
  test [ 1; 2 ];
  [%expect
    {|
    ((input (
       (1 1)
       (2 2)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))) |}];
  test [ 1; 2 ] ~size:100;
  [%expect
    {|
    ((input (
       (1 1)
       (2 2)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))) |}]
;;

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
