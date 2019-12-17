(*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

let flts = 
  [
    1.1234;
    -94.1239823897423;
    0.000000000000012;
    0.12130981239081238973249872349871346123873264876324;
    -3.92
  ]

let op1 f a = LDouble.(to_float (f (of_float a)))
let op2 f a b = LDouble.(to_float (f (of_float a) (of_float b)))

let chk_float x y = 
  match classify_float x, classify_float y with
  | FP_normal, FP_normal 
  | FP_subnormal, FP_subnormal 
  | FP_zero, FP_normal
  | FP_zero, FP_subnormal
  | FP_normal, FP_zero
  | FP_subnormal, FP_zero -> abs_float (x -. y) < 1e-12
  | x, y when x=y -> true (* infinite, zero, nan *)
  | _ -> false

let chk1 fop lop a = 
  let x = fop a in
  let y = op1 lop a in
  chk_float x y
  
let chk2 fop lop a b = 
  let x = fop a b in
  let y = op2 lop a b in
  chk_float x y

let test_op2 _ = 
  let assert_chk2 n f l = 
    List.iter (fun a -> List.iter (fun b -> assert_bool n @@ chk2 f l a b) flts) flts 
  in
  assert_chk2 "add" (+.) LDouble.add;
  assert_chk2 "sub" (-.) LDouble.sub;
  assert_chk2 "mul" ( *. ) LDouble.mul;
  assert_chk2 "div" ( /. ) LDouble.div;
  assert_chk2 "pow" ( ** ) LDouble.pow;
  assert_chk2 "atan2" atan2 LDouble.atan2;
  assert_chk2 "hypot" hypot LDouble.hypot;
  (*assert_chk2 "rem" ??? LDouble.rem;*)
  assert_chk2 "copysign" copysign LDouble.copysign

let test_op1 _ = 
  let assert_chk1 n f l = 
    List.iter (fun a -> assert_bool n @@ chk1 f l a) flts
  in
  assert_chk1 "neg" (fun x -> -. x) LDouble.neg;
  assert_chk1 "sqrt" sqrt LDouble.sqrt;
  assert_chk1 "exp" exp LDouble.exp;
  assert_chk1 "log" log LDouble.log;
  assert_chk1 "log10" log10 LDouble.log10;
  assert_chk1 "expm1" expm1 LDouble.expm1;
  assert_chk1 "log1p" log1p LDouble.log1p;
  assert_chk1 "cos" cos LDouble.cos;
  assert_chk1 "sin" sin LDouble.sin;
  assert_chk1 "tan" tan LDouble.tan;
  assert_chk1 "acos" acos LDouble.acos;
  assert_chk1 "asin" asin LDouble.asin;
  assert_chk1 "atan" atan LDouble.atan;
  assert_chk1 "cosh" cosh LDouble.cosh;
  assert_chk1 "sinh" sinh LDouble.sinh;
  assert_chk1 "tanh" tanh LDouble.tanh;
  (*assert_chk1 "acosh" acosh LDouble.acosh;
  assert_chk1 "asinh" asinh LDouble.asinh;
  assert_chk1 "atanh" atanh LDouble.atanh;*)
  assert_chk1 "ceil" ceil LDouble.ceil;
  assert_chk1 "floor" floor LDouble.floor

let test_opw _ = 
  let chk_frexp a = 
    let x, i = frexp a in
    let y, j = LDouble.(frexp (of_float a)) in
    let y = LDouble.to_float y in
    assert_bool "frexp" (chk_float x y && i=j)
  in
  let chk_modf a = 
    let w,x = modf a in
    let y,z = LDouble.(modf (of_float a)) in
    let y,z = LDouble.(to_float y, to_float z) in
    assert_bool "modf" (chk_float w y && chk_float x z)
  in
  let chk_ldexp a b = 
    let x = ldexp a b in
    let y = LDouble.(to_float (ldexp (of_float a) b)) in
    assert_bool "ldexp" (chk_float x y)
  in
  List.iter chk_frexp flts;
  (* XXX work around bug.  see comment for LDouble.modf *)
  (if not Sys.win32 then List.iter chk_modf flts);
  List.iter (fun a -> List.iter (fun b -> chk_ldexp a b) [2;5;8]) flts

let test_classify _ = 
  assert_bool "min" LDouble.(classify min_float = FP_normal);
  assert_bool "max" LDouble.(classify max_float = FP_normal);
  assert_bool "epsilon" LDouble.(classify max_float = FP_normal);
  assert_bool "nan" LDouble.(classify nan = FP_nan);
  assert_bool "inf" LDouble.(classify infinity = FP_infinite);
  assert_bool "-inf" LDouble.(classify neg_infinity = FP_infinite)

let test_conv _ = 
  List.iter (fun a -> assert_bool "to/of_float" (a = LDouble.(to_float (of_float a)))) flts;
  assert_bool "to_int" (3 = LDouble.(to_int (of_float 3.45)));
  assert_bool "to_int" (-34 = LDouble.(to_int (of_float (-34.999))));
  assert_bool "mant_dig" (LDouble.mant_dig >= 53);
  if Sys.word_size = 32 then (
    let max = float_of_int max_int in
    let min = float_of_int min_int in
    assert_bool "to_int" (max_int = LDouble.(to_int (of_float max)));
    assert_bool "to_int" (min_int = LDouble.(to_int (of_float min)));
    assert_bool "to_int_max" (max_int = LDouble.(to_int (of_int max_int)));
    assert_bool "to_int_min" (min_int = LDouble.(to_int (of_int min_int)));
  )
  else (
    let max =  9007199254740991. in (* 2^53 - 1. Largest integer that fits into the mantissa of a double *)
    let min = -9007199254740991. in
    assert_bool "to_int" (Int64.to_int (-9007199254740991L) = LDouble.(to_int (of_float min)));
    assert_bool "to_int" (Int64.to_int 9007199254740991L = LDouble.(to_int (of_float max)));
    let max,min =
      if LDouble.mant_dig >= 62 then
        max_int,(-max_int)
      else
        let rec iter ac i = if i = 0 then ac else iter (ac * 2) (pred i) in
        let max = (iter 1 LDouble.mant_dig) - 1 in
        max,(max * (-1))
    in
    assert_bool "to_int_max" (max = LDouble.(to_int (of_int max)));
    assert_bool "to_int_min" (min = LDouble.(to_int (of_int min)));
  );
  assert_bool "of_string" (3.5 = LDouble.(to_float (of_string "3.5")));
  assert_bool "to_string" ("3.500000" = LDouble.(to_string (of_float 3.5)))

let test_complex _ = 
  let module C = Complex in
  let cplx = 
    [
      { C.re = 2.9;    im = 4.26 };
      { C.re = 0.32;   im = -7.6 };
      { C.re = -35.1;  im = 12.3 };
      { C.re = -0.002; im = -9.1 };
    ]
  in
  let chk_complex ?(prec=1e-12) x y = C.norm (C.sub x y) < prec in
  let assert_chk2 ?prec name opc opl = 
    List.iter (fun a -> List.iter (fun b -> 
      let open ComplexL in
      assert_bool name (chk_complex ?prec (opc a b) (to_complex (opl (of_complex a) (of_complex b))))
    ) cplx) cplx
  in
  let assert_chk1 ?prec name opc opl = 
    List.iter (fun a -> 
      let open ComplexL in
      assert_bool name (chk_complex ?prec (opc a) (to_complex (opl (of_complex a))))
    ) cplx
  in
  let assert_chkf name opc opl = 
    List.iter (fun a -> 
      let open ComplexL in
      assert_bool name (chk_float (opc a) (LDouble.to_float (opl (of_complex a))))
    ) cplx
  in
  let assert_polar () = 
    let open ComplexL in
    assert_bool "polar" 
      (chk_complex (C.polar 3.4 1.2) 
         (to_complex (polar (LDouble.of_float 3.4) (LDouble.of_float 1.2))))
  in

  assert_chk2 "add" C.add ComplexL.add;
  assert_chk2 "sub" C.sub ComplexL.sub;
  assert_chk2 "mul" C.mul ComplexL.mul;
  assert_chk2 "div" C.div ComplexL.div;
  (* fairly large errors accrue here, so reduce precision *)
  assert_chk2 "pow" ~prec:1e-3 C.pow ComplexL.pow;
  assert_chk1 "neg" C.neg ComplexL.neg;
  assert_chk1 "conj" C.conj ComplexL.conj;
  assert_chk1 "inv" C.inv ComplexL.inv;
  assert_chk1 "sqrt" C.sqrt ComplexL.sqrt;
  assert_chk1 "exp" C.exp ComplexL.exp;
  assert_chk1 "log" C.log ComplexL.log;
  assert_chkf "norm2" C.norm2 ComplexL.norm2;
  assert_chkf "norm" C.norm ComplexL.norm;
  assert_chkf "arg" C.arg ComplexL.arg;
  assert_polar ()

let test_marshal _ = 
  let same_repr x y =
    let open Obj in
    let x = magic x in
    let y = magic y in
    is_block x && is_block y && size x = size y && tag x = tag y
  in
  let assert_ldouble x = 
    let (_,xc,_) as x = "foo", LDouble.of_float x, 1 in
    let s = Marshal.to_string x [] in
    let ((_,yc,_) : string * LDouble.t * int) as y = Marshal.from_string s 0 in
    assert_bool "marshal ldouble" (x=y);
    assert_bool "marshal ldouble repr" (same_repr xc yc)
  in
  let assert_complex x = 
    let (_,xc,_) as x = "f00", ComplexL.of_complex x, 1 in
    let s = Marshal.to_string x [] in
    let ((_,yc,_) : string * ComplexL.t * int) as y = Marshal.from_string s 0 in
    assert_bool "marshal ldouble complex" (x=y);
    assert_bool "marshal ldouble complex repr" (same_repr xc yc)
  in
  assert_ldouble 23.11234;
  assert_ldouble (-23.9345);
  assert_complex { Complex.re = 11.23; im = -46.7764 };
  assert_complex { Complex.re = 0.00037; im = 881.222314 }

let test_comparisons _ = 
  let open LDouble in
  begin (* < *)
    assert_equal false (neg_infinity < nan);
    assert_equal false (nan < neg_infinity);
    assert_equal false (infinity < nan);
    assert_equal false (nan < infinity);
    assert_equal false (of_float 1.0 < nan);
    assert_equal false (nan < of_float 1.0);
    assert_equal false (of_float (-1.0) < nan);
    assert_equal false (nan < of_float (-1.0));
  end;
  begin (* = *)
    assert_equal false (nan = nan);
    assert_equal false (of_float 1.0 = nan);
    assert_equal false (infinity = nan);
  end;
  begin (* compare *)
    assert_equal 0 (compare nan nan);
    assert_equal 1 (compare (of_float 1.0) nan);
    assert_equal 1 (compare infinity nan);
    assert_equal 1 (compare neg_infinity nan);

    assert_equal 0 (compare nan nan);
    assert_equal (-1) (compare nan (of_float 1.0));
    assert_equal (-1) (compare nan infinity);
    assert_equal (-1) (compare nan neg_infinity);
  end;
  begin (* ComplexL compare *)
    let b re im = ComplexL.of_complex {Complex.re = re; im} in
    assert_equal false (b 2.0 3.9 = b 2.0 3.7);
    assert_equal false (b 3.9 2.0 = b 2.1 2.0);
    assert_equal true (b 0.0 1.0 = b 0.0 1.0)
  end

let test_int_conversions _ =
  begin
    assert_equal max_int (LDouble.to_int
			    (LDouble.of_int max_int))
      ~printer:string_of_int;

    assert_equal min_int (LDouble.to_int
			    (LDouble.of_int min_int))
      ~printer:string_of_int;
  end
    

let suite = "LDouble tests" >:::
  [
    "test functions with 2 args" >:: test_op2;
    "test functions with 1 args" >:: test_op1;
    "test functions with weird args" >:: test_opw;
    "test classify" >:: test_classify;
    "test conversion" >:: test_conv;
    "test complex api" >:: test_complex;
    "test marshal" >:: test_marshal;
    "test comparisons" >:: test_comparisons;
    "test int conversions" >:: test_int_conversions;
  ]

let _ = run_test_tt_main suite

