(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes

module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Common(S)
  open M

  (*
    Test primitive operations on complex numbers.

    Arguments and return values are currently mediated through pointers,
    since libffi doesn't support passing complex numbers.
  *)
  let test_complex_primitive_operations _ =
    let wrap' typ1 typ2 f l r =
      let rv = allocate_n ~count:1 typ2 in
      f (allocate typ1 l) (allocate typ2 r) rv;
      !@rv
    in
    let wrap typ f l r = wrap' typ typ f l r in

    let addz64 = wrap complex64 add_complexd
    and mulz64 = wrap complex64 mul_complexd
    and rotz64 = wrap' complex64 double rotdist_complexd
    and addz32 = wrap complex32 add_complexf
    and mulz32 = wrap complex32 mul_complexf
    and rotz32 = wrap' complex32 float rotdist_complexf
    and addzld = wrap complexld add_complexld
    and mulzld = wrap complexld mul_complexld
    and rotzld = wrap' complexld ldouble rotdist_complexld
    in

    begin
      let open Complex in

      let eps64 = 1e-12 in
      let complex64_eq { re = lre; im = lim } { re = rre; im = rim } =
        abs_float (lre -. rre) < eps64 && abs_float (lim -. rim) < eps64 in

      let eps32 = 1e-6 in
      let complex32_eq { re = lre; im = lim } { re = rre; im = rim } =
        abs_float (lre -. rre) < eps32 && abs_float (lim -. rim) < eps32 in

      let l = { re = 3.5; im = -1.0 } and r = { re = 2.0; im = 2.7 } in

      assert_equal ~cmp:complex64_eq (Complex.add l r) (addz64 l r);
      assert_equal ~cmp:complex64_eq (Complex.mul l r) (mulz64 l r);

      assert_equal ~cmp:complex32_eq (Complex.add l r) (addz32 l r);
      assert_equal ~cmp:complex32_eq (Complex.mul l r) (mulz32 l r);

      (* test long double complex *)
      let re x = LDouble.(to_float (ComplexL.re x)) in
      let im x = LDouble.(to_float (ComplexL.im x)) in
      let to_complexld c = LDouble.(ComplexL.make (of_float c.re) (of_float c.im)) in
      let of_complexld c = { re = re c; im = im c } in

      let l', r' = to_complexld l, to_complexld r in
      assert_equal ~cmp:complex64_eq (Complex.add l r) (of_complexld @@ addzld l' r');
      assert_equal ~cmp:complex64_eq (Complex.mul l r) (of_complexld @@ mulzld l' r');

      (* The rotdist test is designed to check passing and returning long doubles.
         The function rotates a complex number by the given angle in radians,
         then returns the manhatten distance (sum of absolute value of real and
         imaginary parts) *)
      let rot x a = 
        let open Complex in
        let y = mul x { re = cos a; im = sin a } in
        abs_float y.re +. abs_float y.im
      in
      let rotzld x r = 
        let open LDouble in
        to_float (rotzld (ComplexL.make (of_float x.re) (of_float x.im)) (of_float r)) 
      in
      let test_rotdist f eps x r = 
        let a = rot x r in
        let b = f x r in
        assert_bool "rotdist" (abs_float (a -. b) < eps)
      in
      test_rotdist rotzld eps64 { re = 2.3; im = -0.6; } 1.4;
      test_rotdist rotz64 eps64 { re = 2.3; im = -0.6; } 1.4;
      test_rotdist rotz32 eps32 { re = 2.3; im = -0.6; } 1.4;

    end
end


module Build_stub_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                            and type 'a return = 'a) =
struct
  module N = Functions.Stubs(S)
  open N

  include Common_tests(S)

  (*
    Test primitive operations on complex numbers passed by value.
  *)
  let test_complex_primitive_value_operations _ =
    begin
      let open Complex in

      let eps64 = 1e-12 in
      let complex64_eq { re = lre; im = lim } { re = rre; im = rim } =
        abs_float (lre -. rre) < eps64 && abs_float (lim -. rim) < eps64 in

      let eps32 = 1e-6 in
      let complex32_eq { re = lre; im = lim } { re = rre; im = rim } =
        abs_float (lre -. rre) < eps32 && abs_float (lim -. rim) < eps32 in

      let l = { re = 3.5; im = -1.0 } and r = { re = 2.0; im = 2.7 } in

      assert_equal ~cmp:complex64_eq (Complex.add l r) (add_complexd_val l r);
      assert_equal ~cmp:complex64_eq (Complex.mul l r) (mul_complexd_val l r);

      assert_equal ~cmp:complex32_eq (Complex.add l r) (add_complexf_val l r);
      assert_equal ~cmp:complex32_eq (Complex.mul l r) (mul_complexf_val l r);

      let zinf = { re = 0.; im = infinity } in
      let res = add_complexd_val zinf zinf in
      assert_equal 0. res.re;
      assert_equal 0. (add_complexf_val zinf zinf).re;
      let ozinf = Obj.repr zinf in
      let ores = Obj.repr res in
      assert_equal (Obj.tag ozinf) (Obj.tag ores);
      assert_equal (Obj.size ozinf) (Obj.size ores);

      (* test long double complex *)
      let re x = LDouble.(to_float (ComplexL.re x)) in
      let im x = LDouble.(to_float (ComplexL.im x)) in
      let to_complexld c = LDouble.(ComplexL.make (of_float c.re) (of_float c.im)) in
      let of_complexld c = { re = re c; im = im c } in

      let l', r' = to_complexld l, to_complexld r in
      assert_equal ~cmp:complex64_eq (Complex.add l r) (of_complexld @@ add_complexld_val l' r');
      assert_equal ~cmp:complex64_eq (Complex.mul l r) (of_complexld @@ mul_complexld_val l' r');

      assert_equal 0. (re (to_complexld zinf));

      (* rot-dist test *)
      let rot x a = 
        let open Complex in
        let y = mul x { re = cos a; im = sin a } in
        abs_float y.re +. abs_float y.im
      in
      let rotdist_complexld_val x r = 
        let open LDouble in
        to_float (rotdist_complexld_val (ComplexL.make (of_float x.re) (of_float x.im)) (of_float r)) 
      in
      let test_rotdist f eps x r = 
        let a = rot x r in
        let b = f x r in
        assert_bool "rotdist" (abs_float (a -. b) < eps)
      in
      test_rotdist rotdist_complexld_val eps64 { re = 2.3; im = -0.6; } 1.4;
      test_rotdist rotdist_complexd_val eps64 { re = 2.3; im = -0.6; } 1.4;
      test_rotdist rotdist_complexf_val eps32 { re = 2.3; im = -0.6; } 1.4;
    end
end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Build_stub_tests(Generated_bindings)


let suite = "Complex number tests" >:::
  ["basic operations on complex numbers (foreign)"
   >:: Foreign_tests.test_complex_primitive_operations;

   "basic operations on complex numbers (stubs)"
   >:: Stub_tests.test_complex_primitive_operations;

   "basic operations on complex numbers passed by value(stubs)"
   >:: Stub_tests.test_complex_primitive_value_operations;
  ]


let _ =
  run_test_tt_main suite
