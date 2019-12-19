open! Import
open! Ppx_compare_lib

module Unit = struct
  type t = unit [@@deriving compare, sexp_of]
end

module type T = sig
  type t [@@deriving compare, sexp_of]
end

let test (type a) (module T : T with type t = a) ordered =
  List.iteri ordered ~f:(fun i ti ->
    List.iteri ordered ~f:(fun j tj ->
      require
        [%here]
        (Ordering.equal
           (Ordering.of_int (T.compare ti tj))
           (Ordering.of_int (Int.compare i j)))
        ~if_false_then_print_s:(lazy [%message "" ~_:(ti : T.t) ~_:(tj : T.t)])))
;;

let%expect_test "bool, char, unit" =
  test (module Bool) [ false; true ];
  test (module Char) [ '\000'; 'a'; 'b' ];
  [%expect {| |}];
  test (module Unit) [ () ];
  [%expect {| |}]
;;

module type Min_zero_max = sig
  include T

  val min_value : t
  val max_value : t
  val zero : t
end

let test_min_zero_max (type a) (module T : Min_zero_max with type t = a) =
  test (module T) [ T.min_value; T.zero; T.max_value ]
;;

let%expect_test _ =
  test_min_zero_max (module Float);
  test_min_zero_max (module Int);
  test_min_zero_max (module Int32);
  test_min_zero_max (module Int64);
  test_min_zero_max (module Nativeint)
;;

let%expect_test "option" =
  test
    (module struct
      type t = int option [@@deriving compare, sexp_of]
    end)
    [ None; Some 0; Some 1 ]
;;

let%expect_test "ref" =
  test
    (module struct
      type t = int ref [@@deriving compare, sexp_of]
    end)
    ([ -1; 0; 1 ] |> List.map ~f:ref)
;;

module type Sequence = sig
  type 'a t [@@deriving compare, sexp_of]

  val of_list : 'a list -> 'a t
end

let test_sequence (module T : Sequence) ordered =
  test
    (module struct
      type t = int T.t [@@deriving compare, sexp_of]
    end)
    (ordered |> List.map ~f:T.of_list)
;;

let%expect_test "array, list" =
  test_sequence (module Array) [ []; [ 1 ]; [ 2 ]; [ 1; 2 ]; [ 2; 1 ] ];
  test_sequence (module List) [ []; [ 1 ]; [ 1; 2 ]; [ 2 ]; [ 2; 1 ] ]
;;

let%expect_test "[compare_abstract]" =
  show_raise (fun () -> compare_abstract ~type_name:"TY" () ());
  [%expect
    {|
    (raised (
      Failure
      "Compare called on the type TY, which is abstract in an implementation.")) |}]
;;
