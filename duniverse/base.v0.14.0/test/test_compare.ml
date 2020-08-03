open! Base
open Expect_test_helpers_core

module type S = sig
  type t [@@deriving sexp_of]

  include Comparable.Polymorphic_compare with type t := t
end

(* Test the consistency of derived comparison operators with [compare] because many of
   them are hand-optimized in [Base]. *)
let test (type a) here (module T : S with type t = a) list =
  let op (type b) (module Result : S with type t = b) operator ~actual ~expect =
    With_return.with_return (fun failed ->
      List.iter list ~f:(fun arg1 ->
        List.iter list ~f:(fun arg2 ->
          let actual = actual arg1 arg2 in
          let expect = expect arg1 arg2 in
          if not (Result.compare actual expect = 0)
          then (
            print_cr
              here
              [%message
                "comparison failed"
                  (operator : string)
                  (arg1 : T.t)
                  (arg2 : T.t)
                  (actual : Result.t)
                  (expect : Result.t)];
            failed.return ()))))
  in
  let module C = Comparable.Make (T) in
  op (module Bool) "equal" ~actual:T.equal ~expect:C.equal;
  op (module T) "min" ~actual:T.min ~expect:C.min;
  op (module T) "max" ~actual:T.max ~expect:C.max;
  op (module Bool) "(=)" ~actual:T.( = ) ~expect:C.( = );
  op (module Bool) "(<)" ~actual:T.( < ) ~expect:C.( < );
  op (module Bool) "(>)" ~actual:T.( > ) ~expect:C.( > );
  op (module Bool) "(<>)" ~actual:T.( <> ) ~expect:C.( <> );
  op (module Bool) "(<=)" ~actual:T.( <= ) ~expect:C.( <= );
  op (module Bool) "(>=)" ~actual:T.( >= ) ~expect:C.( >= );
  op
    (module Bool)
    "Comparable.equal"
    ~actual:(Comparable.equal T.compare)
    ~expect:C.equal;
  op (module T) "Comparable.min" ~actual:(Comparable.min T.compare) ~expect:C.min;
  op (module T) "Comparable.max" ~actual:(Comparable.max T.compare) ~expect:C.max
;;

let%expect_test "Base" =
  test
    [%here]
    (module struct
      include Base

      type t = int [@@deriving sexp_of]
    end)
    Int.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Unit" =
  test [%here] (module Unit) Unit.all;
  [%expect {||}]
;;

let%expect_test "Bool" =
  test [%here] (module Bool) Bool.all;
  [%expect {||}]
;;

let%expect_test "Char" =
  test [%here] (module Char) Char.all;
  [%expect {||}]
;;

let%expect_test "Float" =
  test [%here] (module Float) Float.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Int" =
  test [%here] (module Int) Int.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Int32" =
  test [%here] (module Int32) Int32.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Int64" =
  test [%here] (module Int64) Int64.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Nativeint" =
  test
    [%here]
    (module Nativeint)
    Nativeint.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;

let%expect_test "Int63" =
  test [%here] (module Int63) Int63.[ min_value; minus_one; zero; one; max_value ];
  [%expect {||}]
;;
