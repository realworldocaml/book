open! Base
open Base_quickcheck

(* We define numeric types for rational, real, and complex values derived from ints and
   floats. We derive quickcheck definitions for our types. *)

type rational =
  | Integer of int
  | Rational of
      { numerator : int
      ; denominator : int
      }
[@@deriving compare, quickcheck, sexp_of]

type real =
  | Rational of rational
  | Float of float
[@@deriving compare, quickcheck, sexp_of]

type number =
  | Real of real
  | Complex of real * real
[@@deriving compare, quickcheck, sexp_of]

(* We define addition for our types. *)

let numerator = function
  | Integer n -> n
  | Rational x -> x.numerator
;;

let denominator = function
  | Integer _ -> 1
  | Rational x -> x.denominator
;;

let add_rational x y =
  match x, y with
  | Integer x, Integer y -> Integer (x + y)
  | _ ->
    let nx = numerator x in
    let dx = denominator x in
    let ny = numerator y in
    let dy = denominator y in
    let numerator = (nx * dy) + (ny * dx) in
    let denominator = dy * dx in
    Rational { numerator; denominator }
;;

let rational_float = function
  | Integer int -> Float.of_int int
  | Rational x -> Float.of_int x.numerator /. Float.of_int x.denominator
;;

let real_float = function
  | Rational rational -> rational_float rational
  | Float float -> float
;;

let add_real x y =
  match x, y with
  | Rational x, Rational y -> Rational (add_rational x y)
  | _ -> Float (real_float x +. real_float y)
;;

let real_part = function
  | Real real -> real
  | Complex (real, _) -> real
;;

let imaginary_part = function
  | Real _ -> Rational (Integer 0)
  | Complex (_, imaginary) -> imaginary
;;

let add_number x y =
  match x, y with
  | Real x, Real y -> Real (add_real x y)
  | _ ->
    let real = add_real (real_part x) (real_part y) in
    let imaginary = add_real (imaginary_part x) (imaginary_part y) in
    Complex (real, imaginary)
;;

(* We test some properties of our addition operation. *)

let%test_unit "commutativity" =
  Test.run_exn
    (module struct
      type t = number * number [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (x, y) -> [%test_eq: number] (add_number x y) (add_number y x))
;;

let%test_unit "left identity" =
  Test.run_exn
    (module struct
      type t = number [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun x -> [%test_eq: number] x (add_number (Real (Rational (Integer zero))) x))
;;

let%test_unit "right identity" =
  Test.run_exn
    (module struct
      type t = number [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun x -> [%test_eq: number] x (add_number x (Real (Rational (Integer zero)))))
;;

(* Our implementation does not satisfy associativity. For example, floating point rounding
   errors can break it. *)
let%test_unit "associativity is broken" =
  assert (
    Exn.does_raise (fun () ->
      Test.run_exn
        (module struct
          type t = number * number * number [@@deriving quickcheck, sexp_of]
        end)
        ~f:(fun (x, y, z) ->
          [%test_eq: number]
            (add_number x (add_number y z))
            (add_number (add_number x y) z))))
;;
