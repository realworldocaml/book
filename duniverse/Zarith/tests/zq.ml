(* Simple tests for the Z and Q modules.


   This file is part of the Zarith library
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.

   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

*)


(* testing Z *)

module I = Z

let pr ch x =
  output_string ch (I.to_string x);
  flush ch

let pr2 ch (x,y) =
  Printf.fprintf ch "%s, %s" (I.to_string x) (I.to_string y);
  flush ch

let pr3 ch (x,y,z) =
  Printf.fprintf ch "%s, %s, %s"
    (I.to_string x) (I.to_string y) (I.to_string z);
  flush ch

let prfloat ch (x,y : float * float) =
  if x = y then
    Printf.fprintf ch "OK"
  else
    Printf.fprintf ch "WRONG! (expected %g, got %g)" y x

let prmarshal ch (x,y : I.t * I.t) =
  (if I.equal x y then
     Printf.fprintf ch "OK"
   else
     Printf.fprintf ch "WRONG! (expected %a, got %a)" pr y pr x);
  flush ch

let pow2 n =
  let rec doit acc n =
    if n<=0 then acc else doit (I.add acc acc) (n-1)
  in
  doit I.one n

let fact n =
  let rec doit acc n =
  if n<=1 then acc
  else doit (I.mul acc (I.of_int n)) (n-1)
  in
  doit I.one n

let pow a b =
  let rec doit b =
    if b <= 0 then I.one else
    let acc = doit (b lsr 1) in
    if b land 1 = 1 then I.mul (I.mul acc acc) (I.of_int a)
    else I.mul acc acc
  in
  doit b

let cvt_int x = try string_of_int (I.to_int x) with I.Overflow -> "ovf"
let cvt_int32 x = try Int32.to_string (I.to_int32 x) with I.Overflow -> "ovf"
let cvt_int64 x = try Int64.to_string (I.to_int64 x) with I.Overflow -> "ovf"
let cvt_nativeint x = try Nativeint.to_string (I.to_nativeint x) with I.Overflow -> "ovf"

let p2 = I.of_int 2
let p30 = pow2 30
let p62 = pow2 62
let p300 = pow2 300
let p120 = pow2 120
let p121 = pow2 121
let maxi = I.of_int max_int
let mini = I.of_int min_int
let maxi32 = I.of_int32 Int32.max_int
let mini32 = I.of_int32 Int32.min_int
let maxi64 = I.of_int64 Int64.max_int
let mini64 = I.of_int64 Int64.min_int
let maxni = I.of_nativeint Nativeint.max_int
let minni = I.of_nativeint Nativeint.min_int

let chk_bits x =
  Printf.printf "to_bits %a\n =" pr x;
  String.iter (fun c -> Printf.printf " %02x" (Char.code c)) (I.to_bits x);
  Printf.printf "\n";
  assert(I.equal (I.abs x) (I.of_bits (I.to_bits x)));
  assert((I.to_bits x) = (I.to_bits (I.neg x)));
  Printf.printf "marshal round trip %a\n =" pr x;
  let y = Marshal.(from_string (to_string x []) 0) in
  Printf.printf " %a\n" prmarshal (y, x)

let chk_extract (x, o, l) =
  let expected =
    I.logand (I.shift_right x o) (I.pred (I.shift_left (I.of_int 1) l))
  and actual =
    I.extract x o l in
  Printf.printf "extract %a %d %d = %a " pr x o l pr actual;
  if I.equal actual expected
  then Printf.printf "(passed)\n"
  else Printf.printf "(FAILED, expected %a)\n" pr expected

let chk_signed_extract (x, o, l) =
  let uns_res = I.extract x o l in
  let expected =
    if I.compare uns_res (I.shift_left (I.of_int 1) (l-1)) >= 0
    then I.sub uns_res (I.shift_left (I.of_int 1) l)
    else uns_res in
  let actual =
    I.signed_extract x o l in
  Printf.printf "signed_extract %a %d %d = %a " pr x o l pr actual;
  if I.equal actual expected
  then Printf.printf "(passed)\n"
  else Printf.printf "(FAILED, expected %a)\n" pr expected

let chk_numbits_tz x =
  Printf.printf "numbits / trailing_zeros %a " pr x;
  let n = I.numbits x and z = I.trailing_zeros x in
  if
    if I.equal x I.zero then
      n = 0 && z = max_int
    else
      n > 0 && z >= 0 && z < n
      && I.leq (I.shift_left I.one (n-1)) (I.abs x)
      && I.lt (I.abs x) (I.shift_left I.one n)
      && (z = 0 || I.equal (I.extract x 0 z) I.zero)
      && I.testbit x z
  then Printf.printf "(passed)\n"
  else Printf.printf "(FAILED)\n"

let chk_testbit x =
  Printf.printf "testbit %a " pr x;
  let n = I.numbits x in
  let ok = ref true in
  for i = 0 to n + 64 do
    let actual = I.testbit x i
    and expected = I.extract x i 1 in
    if not (I.equal expected (if actual then I.one else I.zero))
    then begin Printf.printf "(error on %d) " i; ok := false end
  done;
  if !ok
  then Printf.printf "(passed)\n"
  else Printf.printf "(FAILED)\n"

let test_Z() =
  Printf.printf "0\n = %a\n" pr I.zero;
  Printf.printf "1\n = %a\n" pr I.one;
  Printf.printf "-1\n = %a\n" pr I.minus_one;
  Printf.printf "42\n = %a\n" pr (I.of_int 42);
  Printf.printf "1+1\n = %a\n" pr (I.add I.one I.one);
  Printf.printf "1-1\n = %a\n" pr (I.sub I.one I.one);
  Printf.printf "- 1\n = %a\n" pr (I.neg I.one);
  Printf.printf "0-1\n = %a\n" pr (I.sub I.zero I.one);
  Printf.printf "max_int\n = %a\n" pr maxi;
  Printf.printf "min_int\n = %a\n" pr mini;
  Printf.printf "-max_int\n = %a\n" pr (I.neg maxi);
  Printf.printf "-min_int\n = %a\n" pr (I.neg mini);
  Printf.printf "2^300\n = %a\n" pr p300;
  Printf.printf "2^120\n = %a\n" pr p120;
  Printf.printf "2^300+2^120\n = %a\n" pr (I.add p300 p120);
  Printf.printf "2^300-2^120\n = %a\n" pr (I.sub p300 p120);
  Printf.printf "2^300+(-(2^120))\n = %a\n" pr (I.add p300 (I.neg p120));
  Printf.printf "2^120-2^300\n = %a\n" pr (I.sub p120 p300);
  Printf.printf "2^120+(-(2^300))\n = %a\n" pr (I.add p120 (I.neg p300));
  Printf.printf "-(2^120)+(-(2^300))\n = %a\n" pr (I.add (I.neg p120) (I.neg p300));
  Printf.printf "-(2^120)-2^300\n = %a\n" pr (I.sub (I.neg p120) p300);
  Printf.printf "2^300-2^300\n = %a\n" pr (I.sub p300 p300);
  Printf.printf "2^121\n = %a\n" pr p121;
  Printf.printf "2^121+2^120\n = %a\n" pr (I.add p121 p120);
  Printf.printf "2^121-2^120\n = %a\n" pr (I.sub p121 p120);
  Printf.printf "2^121+(-(2^120))\n = %a\n" pr (I.add p121 (I.neg p120));
  Printf.printf "2^120-2^121\n = %a\n" pr (I.sub p120 p121);
  Printf.printf "2^120+(-(2^121))\n = %a\n" pr (I.add p120 (I.neg p121));
  Printf.printf "-(2^120)+(-(2^121))\n = %a\n" pr (I.add (I.neg p120) (I.neg p121));
  Printf.printf "-(2^120)-2^121\n = %a\n" pr (I.sub (I.neg p120) p121);
  Printf.printf "2^121+0\n = %a\n" pr (I.add p121 I.zero);
  Printf.printf "2^121-0\n = %a\n" pr (I.sub p121 I.zero);
  Printf.printf "0+2^121\n = %a\n" pr (I.add I.zero p121);
  Printf.printf "0-2^121\n = %a\n" pr (I.sub I.zero p121);
  Printf.printf "2^300+1\n = %a\n" pr (I.add p300 I.one);
  Printf.printf "2^300-1\n = %a\n" pr (I.sub p300 I.one);
  Printf.printf "1+2^300\n = %a\n" pr (I.add I.one p300);
  Printf.printf "1-2^300\n = %a\n" pr (I.sub I.one p300);
  Printf.printf "2^300+(-1)\n = %a\n" pr (I.add p300 I.minus_one);
  Printf.printf "2^300-(-1)\n = %a\n" pr (I.sub p300 I.minus_one);
  Printf.printf "(-1)+2^300\n = %a\n" pr (I.add I.minus_one p300);
  Printf.printf "(-1)-2^300\n = %a\n" pr (I.sub I.minus_one p300);
  Printf.printf "-(2^300)+1\n = %a\n" pr (I.add (I.neg p300) I.one);
  Printf.printf "-(2^300)-1\n = %a\n" pr (I.sub (I.neg p300) I.one);
  Printf.printf "1+(-(2^300))\n = %a\n" pr (I.add I.one (I.neg p300));
  Printf.printf "1-(-(2^300))\n = %a\n" pr (I.sub I.one (I.neg p300));
  Printf.printf "-(2^300)+(-1)\n = %a\n" pr (I.add (I.neg p300) I.minus_one);
  Printf.printf "-(2^300)-(-1)\n = %a\n" pr (I.sub (I.neg p300) I.minus_one);
  Printf.printf "(-1)+(-(2^300))\n = %a\n" pr (I.add I.minus_one (I.neg p300));
  Printf.printf "(-1)-(-(2^300))\n = %a\n" pr (I.sub I.minus_one (I.neg p300));
  Printf.printf "max_int+1\n = %a\n" pr (I.add maxi I.one);
  Printf.printf "min_int-1\n = %a\n" pr (I.sub mini I.one);
  Printf.printf "-max_int-1\n = %a\n" pr (I.sub (I.neg maxi) I.one);
  Printf.printf "-min_int-1\n = %a\n" pr (I.sub (I.neg mini) I.one);
  Printf.printf "5! = %a\n" pr (fact 5);
  Printf.printf "12! = %a\n" pr (fact 12);
  Printf.printf "15! = %a\n" pr (fact 15);
  Printf.printf "20! = %a\n" pr (fact 20);
  Printf.printf "25! = %a\n" pr (fact 25);
  Printf.printf "50! = %a\n" pr (fact 50);
  Printf.printf "2^300*2^120\n = %a\n" pr (I.mul p300 p120);
  Printf.printf "2^120*2^300\n = %a\n" pr (I.mul p120 p300);
  Printf.printf "2^300*(-(2^120))\n = %a\n" pr (I.mul p300 (I.neg p120));
  Printf.printf "2^120*(-(2^300))\n = %a\n" pr (I.mul p120 (I.neg p300));
  Printf.printf "-(2^120)*(-(2^300))\n = %a\n" pr (I.mul (I.neg p120) (I.neg p300));
  Printf.printf "2^121*2^120\n = %a\n" pr (I.mul p121 p120);
  Printf.printf "2^120*2^121\n = %a\n" pr (I.mul p120 p121);
  Printf.printf "2^121*0\n = %a\n" pr (I.mul p121 I.zero);
  Printf.printf "0*2^121\n = %a\n" pr (I.mul I.zero p121);
  Printf.printf "2^300*1\n = %a\n" pr (I.mul p300 I.one);
  Printf.printf "1*2^300\n = %a\n" pr (I.mul I.one p300);
  Printf.printf "2^300*(-1)\n = %a\n" pr (I.mul p300 I.minus_one);
  Printf.printf "(-1)*2^300\n = %a\n" pr (I.mul I.minus_one p300);
  Printf.printf "-(2^300)*1\n = %a\n" pr (I.mul (I.neg p300) I.one);
  Printf.printf "1*(-(2^300))\n = %a\n" pr (I.mul I.one (I.neg p300));
  Printf.printf "-(2^300)*(-1)\n = %a\n" pr (I.mul (I.neg p300) I.minus_one);
  Printf.printf "(-1)*(-(2^300))\n = %a\n" pr (I.mul I.minus_one (I.neg p300));
  Printf.printf "1*(2^30)\n = %a\n" pr (I.mul I.one p30);
  Printf.printf "1*(2^62)\n = %a\n" pr (I.mul I.one p62);
  Printf.printf "(2^30)*(2^30)\n = %a\n" pr (I.mul p30 p30);
  Printf.printf "(2^62)*(2^62)\n = %a\n" pr (I.mul p62 p62);
  Printf.printf "0+1\n = %a\n" pr (I.succ I.zero);
  Printf.printf "1+1\n = %a\n" pr (I.succ I.one);
  Printf.printf "-1+1\n = %a\n" pr (I.succ I.minus_one);
  Printf.printf "2+1\n = %a\n" pr (I.succ p2);
  Printf.printf "-2+1\n = %a\n" pr (I.succ (I.neg p2));
  Printf.printf "(2^300)+1\n = %a\n" pr (I.succ p300);
  Printf.printf "-(2^300)+1\n = %a\n" pr (I.succ (I.neg p300));
  Printf.printf "0-1\n = %a\n" pr (I.pred I.zero);
  Printf.printf "1-1\n = %a\n" pr (I.pred I.one);
  Printf.printf "-1-1\n = %a\n" pr (I.pred I.minus_one);
  Printf.printf "2-1\n = %a\n" pr (I.pred p2);
  Printf.printf "-2-1\n = %a\n" pr (I.pred (I.neg p2));
  Printf.printf "(2^300)-1\n = %a\n" pr (I.pred p300);
  Printf.printf "-(2^300)-1\n = %a\n" pr (I.pred (I.neg p300));
  Printf.printf "max_int+1\n = %a\n" pr (I.succ maxi);
  Printf.printf "min_int-1\n = %a\n" pr (I.pred mini);
  Printf.printf "-max_int-1\n = %a\n" pr (I.pred (I.neg maxi));
  Printf.printf "-min_int-1\n = %a\n" pr (I.pred (I.neg mini));
  Printf.printf "abs(0)\n = %a\n" pr (I.abs I.zero);
  Printf.printf "abs(1)\n = %a\n" pr (I.abs I.one);
  Printf.printf "abs(-1)\n = %a\n" pr (I.abs I.minus_one);
  Printf.printf "abs(min_int)\n = %a\n" pr (I.abs mini);
  Printf.printf "abs(2^300)\n = %a\n" pr (I.abs p300);
  Printf.printf "abs(-(2^300))\n = %a\n" pr (I.abs (I.neg p300));
  Printf.printf "max_natint\n = %a\n" pr maxni;
  Printf.printf "max_int32\n = %a\n" pr maxi32;
  Printf.printf "max_int64\n = %a\n" pr maxi64;
  Printf.printf "to_int 1\n = %s\n" (cvt_int I.one);
  Printf.printf "to_int max_int\n = %s\n" (cvt_int maxi);
  Printf.printf "to_int max_natint\n = %s\n" (cvt_int maxni);
  Printf.printf "to_int max_int32\n = %s\n" (cvt_int maxi32);
  Printf.printf "to_int max_int64\n = %s\n" (cvt_int maxi64);
  Printf.printf "to_int32 1\n = %s\n" (cvt_int32 I.one);
  Printf.printf "to_int32 max_int\n = %s\n" (cvt_int32 maxi);
  Printf.printf "to_int32 max_natint\n = %s\n" (cvt_int32 maxni);
  Printf.printf "to_int32 max_int32\n = %s\n" (cvt_int32 maxi32);
  Printf.printf "to_int32 max_int64\n = %s\n" (cvt_int32 maxi64);
  Printf.printf "to_int64 1\n = %s\n" (cvt_int64 I.one);
  Printf.printf "to_int64 max_int\n = %s\n" (cvt_int64 maxi);
  Printf.printf "to_int64 max_natint\n = %s\n" (cvt_int64 maxni);
  Printf.printf "to_int64 max_int32\n = %s\n" (cvt_int64 maxi32);
  Printf.printf "to_int64 max_int64\n = %s\n" (cvt_int64 maxi64);
  Printf.printf "to_natint 1\n = %s\n" (cvt_nativeint I.one);
  Printf.printf "to_natint max_int\n = %s\n" (cvt_nativeint maxi);
  Printf.printf "to_natint max_natint\n = %s\n" (cvt_nativeint maxni);
  Printf.printf "to_natint max_int32\n = %s\n" (cvt_nativeint maxi32);
  Printf.printf "to_natint max_int64\n = %s\n" (cvt_nativeint maxi64);
  Printf.printf "to_int -min_int\n = %s\n" (cvt_int (I.neg mini));
  Printf.printf "to_int -min_natint\n = %s\n" (cvt_int (I.neg minni));
  Printf.printf "to_int -min_int32\n = %s\n" (cvt_int (I.neg mini32));
  Printf.printf "to_int -min_int64\n = %s\n" (cvt_int (I.neg mini64));
  Printf.printf "to_int32 -min_int\n = %s\n" (cvt_int32 (I.neg mini));
  Printf.printf "to_int32 -min_natint\n = %s\n" (cvt_int32 (I.neg minni));
  Printf.printf "to_int32 -min_int32\n = %s\n" (cvt_int32 (I.neg mini32));
  Printf.printf "to_int32 -min_int64\n = %s\n" (cvt_int32(I.neg  mini64));
  Printf.printf "to_int64 -min_int\n = %s\n" (cvt_int64 (I.neg mini));
  Printf.printf "to_int64 -min_natint\n = %s\n" (cvt_int64 (I.neg minni));
  Printf.printf "to_int64 -min_int32\n = %s\n" (cvt_int64 (I.neg mini32));
  Printf.printf "to_int64 -min_int64\n = %s\n" (cvt_int64 (I.neg mini64));
  Printf.printf "to_natint -min_int\n = %s\n" (cvt_nativeint (I.neg mini));
  Printf.printf "to_natint -min_natint\n = %s\n" (cvt_nativeint (I.neg minni));
  Printf.printf "to_natint -min_int32\n = %s\n" (cvt_nativeint (I.neg mini32));
  Printf.printf "to_natint -min_int64\n = %s\n" (cvt_nativeint (I.neg mini64));
  Printf.printf "of_float 1.\n = %a\n" pr (I.of_float 1.);
  Printf.printf "of_float -1.\n = %a\n" pr (I.of_float (-. 1.));
  Printf.printf "of_float pi\n = %a\n" pr (I.of_float (2. *. acos 0.));
  Printf.printf "of_float 2^30\n = %a\n" pr (I.of_float (ldexp 1. 30));
  Printf.printf "of_float 2^31\n = %a\n" pr (I.of_float (ldexp 1. 31));
  Printf.printf "of_float 2^32\n = %a\n" pr (I.of_float (ldexp 1. 32));
  Printf.printf "of_float 2^33\n = %a\n" pr (I.of_float (ldexp 1. 33));
  Printf.printf "of_float -2^30\n = %a\n" pr (I.of_float (-.(ldexp 1. 30)));
  Printf.printf "of_float -2^31\n = %a\n" pr (I.of_float (-.(ldexp 1. 31)));
  Printf.printf "of_float -2^32\n = %a\n" pr (I.of_float (-.(ldexp 1. 32)));
  Printf.printf "of_float -2^33\n = %a\n" pr (I.of_float (-.(ldexp 1. 33)));
  Printf.printf "of_float 2^61\n = %a\n" pr (I.of_float (ldexp 1. 61));
  Printf.printf "of_float 2^62\n = %a\n" pr (I.of_float (ldexp 1. 62));
  Printf.printf "of_float 2^63\n = %a\n" pr (I.of_float (ldexp 1. 63));
  Printf.printf "of_float 2^64\n = %a\n" pr (I.of_float (ldexp 1. 64));
  Printf.printf "of_float 2^65\n = %a\n" pr (I.of_float (ldexp 1. 65));
  Printf.printf "of_float -2^61\n = %a\n" pr (I.of_float (-.(ldexp 1. 61)));
  Printf.printf "of_float -2^62\n = %a\n" pr (I.of_float (-.(ldexp 1. 62)));
  Printf.printf "of_float -2^63\n = %a\n" pr (I.of_float (-.(ldexp 1. 63)));
  Printf.printf "of_float -2^64\n = %a\n" pr (I.of_float (-.(ldexp 1. 64)));
  Printf.printf "of_float -2^65\n = %a\n" pr (I.of_float (-.(ldexp 1. 65)));
  Printf.printf "of_float 2^120\n = %a\n" pr (I.of_float (ldexp 1. 120));
  Printf.printf "of_float 2^300\n = %a\n" pr (I.of_float (ldexp 1. 300));
  Printf.printf "of_float -2^120\n = %a\n" pr (I.of_float (-.(ldexp 1. 120)));
  Printf.printf "of_float -2^300\n = %a\n" pr (I.of_float (-.(ldexp 1. 300)));
  Printf.printf "of_float 0.5\n = %a\n" pr (I.of_float 0.5);
  Printf.printf "of_float -0.5\n = %a\n" pr (I.of_float (-. 0.5));
  Printf.printf "of_float 200.5\n = %a\n" pr (I.of_float 200.5);
  Printf.printf "of_float -200.5\n = %a\n" pr (I.of_float (-. 200.5));
  Printf.printf "to_float 0\n = %a\n" prfloat (I.to_float I.zero, 0.0);
  Printf.printf "to_float 1\n = %a\n" prfloat (I.to_float I.one, 1.0);
  Printf.printf "to_float -1\n = %a\n" prfloat (I.to_float I.minus_one, -1.0);
  Printf.printf "to_float 2^120\n = %a\n" prfloat (I.to_float p120, ldexp 1.0 120);
  Printf.printf "to_float -2^120\n = %a\n" prfloat (I.to_float (I.neg p120), -. (ldexp 1.0 120));
  Printf.printf "to_float (2^120-1)\n = %a\n" prfloat (I.to_float (I.pred p120), ldexp 1.0 120);
  Printf.printf "to_float (-2^120+1)\n = %a\n" prfloat (I.to_float (I.succ (I.neg p120)), -. (ldexp 1.0 120));
  Printf.printf "to_float 2^63\n = %a\n" prfloat (I.to_float (pow2 63), ldexp 1.0 63);
  Printf.printf "to_float -2^63\n = %a\n" prfloat (I.to_float (I.neg (pow2 63)), -. (ldexp 1.0 63));
  Printf.printf "to_float (2^63-1)\n = %a\n" prfloat (I.to_float (I.pred (pow2 63)), ldexp 1.0 63);
  Printf.printf "to_float (-2^63-1)\n = %a\n" prfloat (I.to_float (I.pred (I.neg (pow2 63))), -. (ldexp 1.0 63));
  Printf.printf "to_float (-2^63+1)\n = %a\n" prfloat (I.to_float (I.succ (I.neg (pow2 63))), -. (ldexp 1.0 63));
  Printf.printf "to_float 2^300\n = %a\n" prfloat (I.to_float p300, ldexp 1.0 300);
  Printf.printf "to_float -2^300\n = %a\n" prfloat (I.to_float (I.neg p300), -. (ldexp 1.0 300));
  Printf.printf "to_float (2^300-1)\n = %a\n" prfloat (I.to_float (I.pred p300), ldexp 1.0 300);
  Printf.printf "to_float (-2^300+1)\n = %a\n" prfloat (I.to_float (I.succ (I.neg p300)), -. (ldexp 1.0 300));
  Printf.printf "of_string 12\n = %a\n" pr (I.of_string "12");
  Printf.printf "of_string 0x12\n = %a\n" pr (I.of_string "0x12");
  Printf.printf "of_string 0b10\n = %a\n" pr (I.of_string "0b10");
  Printf.printf "of_string 0o12\n = %a\n" pr (I.of_string "0o12");
  Printf.printf "of_string -12\n = %a\n" pr (I.of_string "-12");
  Printf.printf "of_string -0x12\n = %a\n" pr (I.of_string "-0x12");
  Printf.printf "of_string -0b10\n = %a\n" pr (I.of_string "-0b10");
  Printf.printf "of_string -0o12\n = %a\n" pr (I.of_string "-0o12");
  Printf.printf "of_string 000123456789012345678901234567890\n = %a\n" pr (I.of_string "000123456789012345678901234567890");
  Printf.printf "2^120 / 2^300 (trunc)\n = %a\n" pr (I.div p120 p300);
  Printf.printf "max_int / 2 (trunc)\n = %a\n" pr (I.div maxi p2);
  Printf.printf "(2^300+1) / 2^120 (trunc)\n = %a\n" pr (I.div (I.succ p300) p120);
  Printf.printf "(-(2^300+1)) / 2^120 (trunc)\n = %a\n" pr (I.div (I.neg (I.succ p300)) p120);
  Printf.printf "(2^300+1) / (-(2^120)) (trunc)\n = %a\n" pr (I.div (I.succ p300) (I.neg p120));
  Printf.printf "(-(2^300+1)) / (-(2^120)) (trunc)\n = %a\n" pr (I.div (I.neg (I.succ p300)) (I.neg p120));
  Printf.printf "2^120 / 2^300 (ceil)\n = %a\n" pr (I.cdiv p120 p300);
  Printf.printf "max_int / 2 (ceil)\n = %a\n" pr (I.cdiv maxi p2);
  Printf.printf "(2^300+1) / 2^120 (ceil)\n = %a\n" pr (I.cdiv (I.succ p300) p120);
  Printf.printf "(-(2^300+1)) / 2^120 (ceil)\n = %a\n" pr (I.cdiv (I.neg (I.succ p300)) p120);
  Printf.printf "(2^300+1) / (-(2^120)) (ceil)\n = %a\n" pr (I.cdiv (I.succ p300) (I.neg p120));
  Printf.printf "(-(2^300+1)) / (-(2^120)) (ceil)\n = %a\n" pr (I.cdiv (I.neg (I.succ p300)) (I.neg p120));
  Printf.printf "2^120 / 2^300 (floor)\n = %a\n" pr (I.fdiv p120 p300);
  Printf.printf "max_int / 2 (floor)\n = %a\n" pr (I.fdiv maxi p2);
  Printf.printf "(2^300+1) / 2^120 (floor)\n = %a\n" pr (I.fdiv (I.succ p300) p120);
  Printf.printf "(-(2^300+1)) / 2^120 (floor)\n = %a\n" pr (I.fdiv (I.neg (I.succ p300)) p120);
  Printf.printf "(2^300+1) / (-(2^120)) (floor)\n = %a\n" pr (I.fdiv (I.succ p300) (I.neg p120));
  Printf.printf "(-(2^300+1)) / (-(2^120)) (floor)\n = %a\n" pr (I.fdiv (I.neg (I.succ p300)) (I.neg p120));
  Printf.printf "2^120 %% 2^300\n = %a\n" pr (I.rem p120 p300);
  Printf.printf "max_int %% 2\n = %a\n" pr (I.rem maxi p2);
  Printf.printf "(2^300+1) %% 2^120\n = %a\n" pr (I.rem (I.succ p300) p120);
  Printf.printf "(-(2^300+1)) %% 2^120\n = %a\n" pr (I.rem (I.neg (I.succ p300)) p120);
  Printf.printf "(2^300+1) %% (-(2^120))\n = %a\n" pr (I.rem (I.succ p300) (I.neg p120));
  Printf.printf "(-(2^300+1)) %% (-(2^120))\n = %a\n" pr (I.rem (I.neg (I.succ p300)) (I.neg p120));
  Printf.printf "2^120 /,%% 2^300\n = %a\n" pr2 (I.div_rem p120 p300);
  Printf.printf "max_int /,%% 2\n = %a\n" pr2 (I.div_rem maxi p2);
  Printf.printf "(2^300+1) /,%% 2^120\n = %a\n" pr2 (I.div_rem (I.succ p300) p120);
  Printf.printf "(-(2^300+1)) /,%% 2^120\n = %a\n" pr2 (I.div_rem (I.neg (I.succ p300)) p120);
  Printf.printf "(2^300+1) /,%% (-(2^120))\n = %a\n" pr2 (I.div_rem (I.succ p300) (I.neg p120));
  Printf.printf "(-(2^300+1)) /,%% (-(2^120))\n = %a\n" pr2 (I.div_rem (I.neg (I.succ p300)) (I.neg p120));
  Printf.printf "1 & 2\n = %a\n" pr (I.logand I.one p2);
  Printf.printf "1 & 2^300\n = %a\n" pr (I.logand I.one p300);
  Printf.printf "2^120 & 2^300\n = %a\n" pr (I.logand p120 p300);
  Printf.printf "2^300 & 2^120\n = %a\n" pr (I.logand p300 p120);
  Printf.printf "2^300 & 2^300\n = %a\n" pr (I.logand p300 p300);
  Printf.printf "2^300 & 0\n = %a\n" pr (I.logand p300 I.zero);
  Printf.printf "-2^120 & 2^300\n = %a\n" pr (I.logand (I.neg p120) p300);
  Printf.printf " 2^120 & -2^300\n = %a\n" pr (I.logand p120 (I.neg p300));
  Printf.printf "-2^120 & -2^300\n = %a\n" pr (I.logand (I.neg p120) (I.neg p300));
  Printf.printf "-2^300 & 2^120\n = %a\n" pr (I.logand (I.neg p300) p120);
  Printf.printf " 2^300 & -2^120\n = %a\n" pr (I.logand p300 (I.neg p120));
  Printf.printf "-2^300 & -2^120\n = %a\n" pr (I.logand (I.neg p300) (I.neg p120));
  Printf.printf "1 | 2\n = %a\n" pr (I.logor I.one p2);
  Printf.printf "1 | 2^300\n = %a\n" pr (I.logor I.one p300);
  Printf.printf "2^120 | 2^300\n = %a\n" pr (I.logor p120 p300);
  Printf.printf "2^300 | 2^120\n = %a\n" pr (I.logor p300 p120);
  Printf.printf "2^300 | 2^300\n = %a\n" pr (I.logor p300 p300);
  Printf.printf "2^300 | 0\n = %a\n" pr (I.logor p300 I.zero);
  Printf.printf "-2^120 | 2^300\n = %a\n" pr (I.logor (I.neg p120) p300);
  Printf.printf " 2^120 | -2^300\n = %a\n" pr (I.logor p120 (I.neg p300));
  Printf.printf "-2^120 | -2^300\n = %a\n" pr (I.logor (I.neg p120) (I.neg p300));
  Printf.printf "-2^300 | 2^120\n = %a\n" pr (I.logor (I.neg p300) p120);
  Printf.printf " 2^300 | -2^120\n = %a\n" pr (I.logor p300 (I.neg p120));
  Printf.printf "-2^300 | -2^120\n = %a\n" pr (I.logor (I.neg p300) (I.neg p120));
  Printf.printf "1 ^ 2\n = %a\n" pr (I.logxor I.one p2);
  Printf.printf "1 ^ 2^300\n = %a\n" pr (I.logxor I.one p300);
  Printf.printf "2^120 ^ 2^300\n = %a\n" pr (I.logxor p120 p300);
  Printf.printf "2^300 ^ 2^120\n = %a\n" pr (I.logxor p300 p120);
  Printf.printf "2^300 ^ 2^300\n = %a\n" pr (I.logxor p300 p300);
  Printf.printf "2^300 ^ 0\n = %a\n" pr (I.logxor p300 I.zero);
  Printf.printf "-2^120 ^ 2^300\n = %a\n" pr (I.logxor (I.neg p120) p300);
  Printf.printf " 2^120 ^ -2^300\n = %a\n" pr (I.logxor p120 (I.neg p300));
  Printf.printf "-2^120 ^ -2^300\n = %a\n" pr (I.logxor (I.neg p120) (I.neg p300));
  Printf.printf "-2^300 ^ 2^120\n = %a\n" pr (I.logxor (I.neg p300) p120);
  Printf.printf " 2^300 ^ -2^120\n = %a\n" pr (I.logxor p300 (I.neg p120));
  Printf.printf "-2^300 ^ -2^120\n = %a\n" pr (I.logxor (I.neg p300) (I.neg p120));
  Printf.printf "~0\n = %a\n" pr (I.lognot I.zero);
  Printf.printf "~1\n = %a\n" pr (I.lognot I.one);
  Printf.printf "~2\n = %a\n" pr (I.lognot p2);
  Printf.printf "~2^300\n = %a\n" pr (I.lognot p300);
  Printf.printf "~(-1)\n = %a\n" pr (I.lognot I.minus_one);
  Printf.printf "~(-2)\n = %a\n" pr (I.lognot (I.neg p2));
  Printf.printf "~(-(2^300))\n = %a\n" pr (I.lognot (I.neg p300));
  Printf.printf "0 >> 1\n = %a\n" pr (I.shift_right I.zero 1);
  Printf.printf "0 >> 100\n = %a\n" pr (I.shift_right I.zero 100);
  Printf.printf "2 >> 1\n = %a\n" pr (I.shift_right p2 1);
  Printf.printf "2 >> 2\n = %a\n" pr (I.shift_right p2 2);
  Printf.printf "2 >> 100\n = %a\n" pr (I.shift_right p2 100);
  Printf.printf "2^300 >> 1\n = %a\n" pr (I.shift_right p300 1);
  Printf.printf "2^300 >> 2\n = %a\n" pr (I.shift_right p300 2);
  Printf.printf "2^300 >> 100\n = %a\n" pr (I.shift_right p300 100);
  Printf.printf "2^300 >> 200\n = %a\n" pr (I.shift_right p300 200);
  Printf.printf "2^300 >> 300\n = %a\n" pr (I.shift_right p300 300);
  Printf.printf "2^300 >> 400\n = %a\n" pr (I.shift_right p300 400);
  Printf.printf "-1 >> 1\n = %a\n" pr (I.shift_right I.minus_one 1);
  Printf.printf "-2 >> 1\n = %a\n" pr (I.shift_right (I.neg p2) 1);
  Printf.printf "-2 >> 2\n = %a\n" pr (I.shift_right (I.neg p2) 2);
  Printf.printf "-2 >> 100\n = %a\n" pr (I.shift_right (I.neg p2) 100);
  Printf.printf "-2^300 >> 1\n = %a\n" pr (I.shift_right (I.neg p300) 1);
  Printf.printf "-2^300 >> 2\n = %a\n" pr (I.shift_right (I.neg p300) 2);
  Printf.printf "-2^300 >> 100\n = %a\n" pr (I.shift_right (I.neg p300) 100);
  Printf.printf "-2^300 >> 200\n = %a\n" pr (I.shift_right (I.neg p300) 200);
  Printf.printf "-2^300 >> 300\n = %a\n" pr (I.shift_right (I.neg p300) 300);
  Printf.printf "-2^300 >> 400\n = %a\n" pr (I.shift_right (I.neg p300) 400);
  Printf.printf "0 >>0 1\n = %a\n" pr (I.shift_right_trunc I.zero 1);
  Printf.printf "0 >>0 100\n = %a\n" pr (I.shift_right_trunc I.zero 100);
  Printf.printf "2 >>0 1\n = %a\n" pr (I.shift_right_trunc p2 1);
  Printf.printf "2 >>0 2\n = %a\n" pr (I.shift_right_trunc p2 2);
  Printf.printf "2 >>0 100\n = %a\n" pr (I.shift_right_trunc p2 100);
  Printf.printf "2^300 >>0 1\n = %a\n" pr (I.shift_right_trunc p300 1);
  Printf.printf "2^300 >>0 2\n = %a\n" pr (I.shift_right_trunc p300 2);
  Printf.printf "2^300 >>0 100\n = %a\n" pr (I.shift_right_trunc p300 100);
  Printf.printf "2^300 >>0 200\n = %a\n" pr (I.shift_right_trunc p300 200);
  Printf.printf "2^300 >>0 300\n = %a\n" pr (I.shift_right_trunc p300 300);
  Printf.printf "2^300 >>0 400\n = %a\n" pr (I.shift_right_trunc p300 400);
  Printf.printf "-1 >>0 1\n = %a\n" pr (I.shift_right_trunc I.minus_one 1);
  Printf.printf "-2 >>0 1\n = %a\n" pr (I.shift_right_trunc (I.neg p2) 1);
  Printf.printf "-2 >>0 2\n = %a\n" pr (I.shift_right_trunc (I.neg p2) 2);
  Printf.printf "-2 >>0 100\n = %a\n" pr (I.shift_right_trunc (I.neg p2) 100);
  Printf.printf "-2^300 >>0 1\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 1);
  Printf.printf "-2^300 >>0 2\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 2);
  Printf.printf "-2^300 >>0 100\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 100);
  Printf.printf "-2^300 >>0 200\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 200);
  Printf.printf "-2^300 >>0 300\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 300);
  Printf.printf "-2^300 >>0 400\n = %a\n" pr (I.shift_right_trunc (I.neg p300) 400);
  Printf.printf "0 << 1\n = %a\n" pr (I.shift_left I.zero 1);
  Printf.printf "0 << 100\n = %a\n" pr (I.shift_left I.zero 100);
  Printf.printf "2 << 1\n = %a\n" pr (I.shift_left p2 1);
  Printf.printf "2 << 32\n = %a\n" pr (I.shift_left p2 32);
  Printf.printf "2 << 64\n = %a\n" pr (I.shift_left p2 64);
  Printf.printf "2 << 299\n = %a\n" pr (I.shift_left p2 299);
  Printf.printf "2^120 << 1\n = %a\n" pr (I.shift_left p120 1);
  Printf.printf "2^120 << 180\n = %a\n" pr (I.shift_left p120 180);
  Printf.printf "compare 1 2\n = %i\n" (I.compare I.one p2);
  Printf.printf "compare 1 1\n = %i\n" (I.compare I.one I.one);
  Printf.printf "compare 2 1\n = %i\n" (I.compare p2 I.one);
  Printf.printf "compare 2^300 2^120\n = %i\n" (I.compare p300 p120);
  Printf.printf "compare 2^120 2^120\n = %i\n" (I.compare p120 p120);
  Printf.printf "compare 2^120 2^300\n = %i\n" (I.compare p120 p300);
  Printf.printf "compare 2^121 2^120\n = %i\n" (I.compare p121 p120);
  Printf.printf "compare 2^120 2^121\n = %i\n" (I.compare p120 p121);
  Printf.printf "compare 2^300 -2^120\n = %i\n" (I.compare p300 (I.neg p120));
  Printf.printf "compare 2^120 -2^120\n = %i\n" (I.compare p120 (I.neg p120));
  Printf.printf "compare 2^120 -2^300\n = %i\n" (I.compare p120 (I.neg p300));
  Printf.printf "compare -2^300 2^120\n = %i\n" (I.compare (I.neg p300) p120);
  Printf.printf "compare -2^120 2^120\n = %i\n" (I.compare (I.neg p120) p120);
  Printf.printf "compare -2^120 2^300\n = %i\n" (I.compare (I.neg p120) p300);
  Printf.printf "compare -2^300 -2^120\n = %i\n" (I.compare (I.neg p300) (I.neg p120));
  Printf.printf "compare -2^120 -2^120\n = %i\n" (I.compare (I.neg p120) (I.neg p120));
  Printf.printf "compare -2^120 -2^300\n = %i\n" (I.compare (I.neg p120) (I.neg p300));
  Printf.printf "equal 1 2\n = %B\n" (I.equal I.one p2);
  Printf.printf "equal 1 1\n = %B\n" (I.equal I.one I.one);
  Printf.printf "equal 2 1\n = %B\n" (I.equal p2 I.one);
  Printf.printf "equal 2^300 2^120\n = %B\n" (I.equal p300 p120);
  Printf.printf "equal 2^120 2^120\n = %B\n" (I.equal p120 p120);
  Printf.printf "equal 2^120 2^300\n = %B\n" (I.equal p120 p300);
  Printf.printf "equal 2^121 2^120\n = %B\n" (I.equal p121 p120);
  Printf.printf "equal 2^120 2^121\n = %B\n" (I.equal p120 p121);
  Printf.printf "equal 2^120 -2^120\n = %B\n" (I.equal p120 (I.neg p120));
  Printf.printf "equal -2^120 2^120\n = %B\n" (I.equal (I.neg p120) p120);
  Printf.printf "equal -2^120 -2^120\n = %B\n" (I.equal (I.neg p120) (I.neg p120));
  Printf.printf "sign 0\n = %i\n" (I.sign I.zero);
  Printf.printf "sign 1\n = %i\n" (I.sign I.one);
  Printf.printf "sign -1\n = %i\n" (I.sign I.minus_one);
  Printf.printf "sign 2^300\n = %i\n" (I.sign p300);
  Printf.printf "sign -2^300\n = %i\n" (I.sign (I.neg p300));
  Printf.printf "gcd 0 -137\n = %a\n" pr (I.gcd (I.of_int 0) (I.of_int (-137)));
  Printf.printf "gcd 12 27\n = %a\n" pr (I.gcd (I.of_int 12) (I.of_int 27));
  Printf.printf "gcd 27 12\n = %a\n" pr (I.gcd (I.of_int 27) (I.of_int 12));
  Printf.printf "gcd 27 27\n = %a\n" pr (I.gcd (I.of_int 27) (I.of_int 27));
  Printf.printf "gcd -12 27\n = %a\n" pr (I.gcd (I.of_int (-12)) (I.of_int 27));
  Printf.printf "gcd 12 -27\n = %a\n" pr (I.gcd (I.of_int 12) (I.of_int (-27)));
  Printf.printf "gcd -12 -27\n = %a\n" pr (I.gcd (I.of_int (-12)) (I.of_int (-27)));
  Printf.printf "gcd 0 2^300\n = %a\n" pr (I.gcd (I.of_int 0) p300);
  Printf.printf "gcd 2^120 2^300\n = %a\n" pr (I.gcd p120 p300);
  Printf.printf "gcd 2^300 2^120\n = %a\n" pr (I.gcd p300 p120);
  Printf.printf "gcdext 12 27\n = %a\n" pr3 (I.gcdext (I.of_int 12) (I.of_int 27));
  Printf.printf "gcdext 27 12\n = %a\n" pr3 (I.gcdext (I.of_int 27) (I.of_int 12));
  Printf.printf "gcdext 27 27\n = %a\n" pr3 (I.gcdext (I.of_int 27) (I.of_int 27));
  Printf.printf "gcdext -12 27\n = %a\n" pr3 (I.gcdext (I.of_int (-12)) (I.of_int 27));
  Printf.printf "gcdext 12 -27\n = %a\n" pr3 (I.gcdext (I.of_int 12) (I.of_int (-27)));
  Printf.printf "gcdext -12 -27\n = %a\n" pr3 (I.gcdext (I.of_int (-12)) (I.of_int (-27)));
  Printf.printf "gcdext 2^120 2^300\n = %a\n" pr3 (I.gcdext p120 p300);
  Printf.printf "gcdext 2^300 2^120\n = %a\n" pr3 (I.gcdext p300 p120);
  Printf.printf "is_odd 0\n = %b\n" (I.is_odd (Z.of_int 0));
  Printf.printf "is_odd 1\n = %b\n" (I.is_odd (Z.of_int 1));
  Printf.printf "is_odd 2\n = %b\n" (I.is_odd (Z.of_int 2));
  Printf.printf "is_odd 3\n = %b\n" (I.is_odd (Z.of_int 3));
  Printf.printf "is_odd 2^120\n = %b\n" (I.is_odd p120);
  Printf.printf "is_odd 2^120+1\n = %b\n" (I.is_odd (Z.succ p120));
  Printf.printf "is_odd 2^300\n = %b\n" (I.is_odd p300);
  Printf.printf "is_odd 2^300+1\n = %b\n" (I.is_odd (Z.succ p300));
  Printf.printf "sqrt 0\n = %a\n" pr (I.sqrt I.zero);
  Printf.printf "sqrt 1\n = %a\n" pr (I.sqrt I.one);
  Printf.printf "sqrt 2\n = %a\n" pr (I.sqrt p2);
  Printf.printf "sqrt 2^120\n = %a\n" pr (I.sqrt p120);
  Printf.printf "sqrt 2^121\n = %a\n" pr (I.sqrt p121);
  Printf.printf "sqrt_rem 0\n = %a\n" pr2 (I.sqrt_rem I.zero);
  Printf.printf "sqrt_rem 1\n = %a\n" pr2 (I.sqrt_rem I.one);
  Printf.printf "sqrt_rem 2\n = %a\n" pr2 (I.sqrt_rem p2);
  Printf.printf "sqrt_rem 2^120\n = %a\n" pr2 (I.sqrt_rem p120);
  Printf.printf "sqrt_rem 2^121\n = %a\n" pr2 (I.sqrt_rem p121);
  Printf.printf "popcount 0\n = %i\n" (I.popcount I.zero);
  Printf.printf "popcount 1\n = %i\n" (I.popcount I.one);
  Printf.printf "popcount 2\n = %i\n" (I.popcount p2);
  Printf.printf "popcount max_int32\n = %i\n" (I.popcount maxi32);
  Printf.printf "popcount 2^120\n = %i\n" (I.popcount p120);
  Printf.printf "popcount (2^120-1)\n = %i\n" (I.popcount (I.pred p120));
  Printf.printf "hamdist 0 0\n = %i\n" (I.hamdist I.zero I.zero);
  Printf.printf "hamdist 0 1\n = %i\n" (I.hamdist I.zero I.one);
  Printf.printf "hamdist 0 2^300\n = %i\n" (I.hamdist I.zero p300);
  Printf.printf "hamdist 2^120 2^120\n = %i\n" (I.hamdist p120 p120);
  Printf.printf "hamdist 2^120 (2^120-1)\n = %i\n" (I.hamdist p120 (I.pred p120));
  Printf.printf "hamdist 2^120 2^300\n = %i\n" (I.hamdist p120 p300);
  Printf.printf "hamdist (2^120-1) (2^300-1)\n = %i\n" (I.hamdist (I.pred p120) (I.pred p300));
  (* always 0 when not using custom blocks *)
  Printf.printf "hash(2^120)\n = %i\n" (Hashtbl.hash p120);
  Printf.printf "hash(2^121)\n = %i\n" (Hashtbl.hash p121);
  Printf.printf "hash(2^300)\n = %i\n" (Hashtbl.hash p300);
  (* fails if not using custom blocks *)
  Printf.printf "2^120 = 2^300\n = %B\n" (p120 = p300);
  Printf.printf "2^120 = 2^120\n = %B\n" (p120 = p120);
  Printf.printf "2^120 = 2^120\n = %B\n" (p120 = (pow2 120));
  Printf.printf "2^120 > 2^300\n = %B\n" (p120 > p300);
  Printf.printf "2^120 < 2^300\n = %B\n" (p120 < p300);
  Printf.printf "2^120 = 1\n = %B\n" (p120 = I.one);
  (* In OCaml < 3.12.1, the order is not consistent with integers when
     comparing mpn_ and ints with OCaml's polymorphic compare operator.
     In OCaml >= 3.12.1, the results are consistent.
   *)
  Printf.printf "2^120 > 1\n = %B\n" (p120 > I.one);
  Printf.printf "2^120 < 1\n = %B\n" (p120 < I.one);
  Printf.printf "-2^120 > 1\n = %B\n" ((I.neg p120) > I.one);
  Printf.printf "-2^120 < 1\n = %B\n" ((I.neg p120) < I.one);
  Printf.printf "demarshal 2^120, 2^300, 1\n = %a\n" pr3
    (Marshal.from_string (Marshal.to_string (p120,p300,I.one) []) 0);
  Printf.printf "demarshal -2^120, -2^300, -1\n = %a\n" pr3
    (Marshal.from_string (Marshal.to_string (I.neg p120,I.neg p300,I.minus_one) []) 0);
  Printf.printf "format %%i 0 = /%s/\n" (I.format "%i" I.zero);
  Printf.printf "format %%i 1 = /%s/\n" (I.format "%i" I.one);
  Printf.printf "format %%i -1 = /%s/\n" (I.format "%i" I.minus_one);
  Printf.printf "format %%i 2^30 = /%s/\n" (I.format "%i" p30);
  Printf.printf "format %%i -2^30 = /%s/\n" (I.format "%i" (I.neg p30));
  Printf.printf "format %% i 1 = /%s/\n" (I.format "% i" I.one);
  Printf.printf "format %%+i 1 = /%s/\n" (I.format "%+i" I.one);
  Printf.printf "format %%x 0 = /%s/\n" (I.format "%x" I.zero);
  Printf.printf "format %%x 1 = /%s/\n" (I.format "%x" I.one);
  Printf.printf "format %%x -1 = /%s/\n" (I.format "%x" I.minus_one);
  Printf.printf "format %%x 2^30 = /%s/\n" (I.format "%x" p30);
  Printf.printf "format %%x -2^30 = /%s/\n" (I.format "%x" (I.neg p30));
  Printf.printf "format %%X 0 = /%s/\n" (I.format "%X" I.zero);
  Printf.printf "format %%X 1 = /%s/\n" (I.format "%X" I.one);
  Printf.printf "format %%X -1 = /%s/\n" (I.format "%X" I.minus_one);
  Printf.printf "format %%X 2^30 = /%s/\n" (I.format "%X" p30);
  Printf.printf "format %%X -2^30 = /%s/\n" (I.format "%X" (I.neg p30));
  Printf.printf "format %%o 0 = /%s/\n" (I.format "%o" I.zero);
  Printf.printf "format %%o 1 = /%s/\n" (I.format "%o" I.one);
  Printf.printf "format %%o -1 = /%s/\n" (I.format "%o" I.minus_one);
  Printf.printf "format %%o 2^30 = /%s/\n" (I.format "%o" p30);
  Printf.printf "format %%o -2^30 = /%s/\n" (I.format "%o" (I.neg p30));
  Printf.printf "format %%10i 0 = /%s/\n" (I.format "%10i" I.zero);
  Printf.printf "format %%10i 1 = /%s/\n" (I.format "%10i" I.one);
  Printf.printf "format %%10i -1 = /%s/\n" (I.format "%10i" I.minus_one);
  Printf.printf "format %%10i 2^30 = /%s/\n" (I.format "%10i" p30);
  Printf.printf "format %%10i -2^30 = /%s/\n" (I.format "%10i" (I.neg p30));
  Printf.printf "format %%-10i 0 = /%s/\n" (I.format "%-10i" I.zero);
  Printf.printf "format %%-10i 1 = /%s/\n" (I.format "%-10i" I.one);
  Printf.printf "format %%-10i -1 = /%s/\n" (I.format "%-10i" I.minus_one);
  Printf.printf "format %%-10i 2^30 = /%s/\n" (I.format "%-10i" p30);
  Printf.printf "format %%-10i -2^30 = /%s/\n" (I.format "%-10i" (I.neg p30));
  Printf.printf "format %%+10i 0 = /%s/\n" (I.format "%+10i" I.zero);
  Printf.printf "format %%+10i 1 = /%s/\n" (I.format "%+10i" I.one);
  Printf.printf "format %%+10i -1 = /%s/\n" (I.format "%+10i" I.minus_one);
  Printf.printf "format %%+10i 2^30 = /%s/\n" (I.format "%+10i" p30);
  Printf.printf "format %%+10i -2^30 = /%s/\n" (I.format "%+10i" (I.neg p30));
  Printf.printf "format %% 10i 0 = /%s/\n" (I.format "% 10i" I.zero);
  Printf.printf "format %% 10i 1 = /%s/\n" (I.format "% 10i" I.one);
  Printf.printf "format %% 10i -1 = /%s/\n" (I.format "% 10i" I.minus_one);
  Printf.printf "format %% 10i 2^30 = /%s/\n" (I.format "% 10i" p30);
  Printf.printf "format %% 10i -2^30 = /%s/\n" (I.format "% 10i" (I.neg p30));
  Printf.printf "format %%010i 0 = /%s/\n" (I.format "%010i" I.zero);
  Printf.printf "format %%010i 1 = /%s/\n" (I.format "%010i" I.one);
  Printf.printf "format %%010i -1 = /%s/\n" (I.format "%010i" I.minus_one);
  Printf.printf "format %%010i 2^30 = /%s/\n" (I.format "%010i" p30);
  Printf.printf "format %%010i -2^30 = /%s/\n" (I.format "%010i" (I.neg p30));
  Printf.printf "format %%#x 0 = /%s/\n" (I.format "%#x" I.zero);
  Printf.printf "format %%#x 1 = /%s/\n" (I.format "%#x" I.one);
  Printf.printf "format %%#x -1 = /%s/\n" (I.format "%#x" I.minus_one);
  Printf.printf "format %%#x 2^30 = /%s/\n" (I.format "%#x" p30);
  Printf.printf "format %%#x -2^30 = /%s/\n" (I.format "%#x" (I.neg p30));
  Printf.printf "format %%#X 0 = /%s/\n" (I.format "%#X" I.zero);
  Printf.printf "format %%#X 1 = /%s/\n" (I.format "%#X" I.one);
  Printf.printf "format %%#X -1 = /%s/\n" (I.format "%#X" I.minus_one);
  Printf.printf "format %%#X 2^30 = /%s/\n" (I.format "%#X" p30);
  Printf.printf "format %%#X -2^30 = /%s/\n" (I.format "%#X" (I.neg p30));
  Printf.printf "format %%#o 0 = /%s/\n" (I.format "%#o" I.zero);
  Printf.printf "format %%#o 1 = /%s/\n" (I.format "%#o" I.one);
  Printf.printf "format %%#o -1 = /%s/\n" (I.format "%#o" I.minus_one);
  Printf.printf "format %%#o 2^30 = /%s/\n" (I.format "%#o" p30);
  Printf.printf "format %%#o -2^30 = /%s/\n" (I.format "%#o" (I.neg p30));
  Printf.printf "format %%#10x 0 = /%s/\n" (I.format "%#10x" I.zero);
  Printf.printf "format %%#10x 1 = /%s/\n" (I.format "%#10x" I.one);
  Printf.printf "format %%#10x -1 = /%s/\n" (I.format "%#10x" I.minus_one);
  Printf.printf "format %%#10x 2^30 = /%s/\n" (I.format "%#10x" p30);
  Printf.printf "format %%#10x -2^30 = /%s/\n" (I.format "%#10x" (I.neg p30));
  Printf.printf "format %%#10X 0 = /%s/\n" (I.format "%#10X" I.zero);
  Printf.printf "format %%#10X 1 = /%s/\n" (I.format "%#10X" I.one);
  Printf.printf "format %%#10X -1 = /%s/\n" (I.format "%#10X" I.minus_one);
  Printf.printf "format %%#10X 2^30 = /%s/\n" (I.format "%#10X" p30);
  Printf.printf "format %%#10X -2^30 = /%s/\n" (I.format "%#10X" (I.neg p30));
  Printf.printf "format %%#10o 0 = /%s/\n" (I.format "%#10o" I.zero);
  Printf.printf "format %%#10o 1 = /%s/\n" (I.format "%#10o" I.one);
  Printf.printf "format %%#10o -1 = /%s/\n" (I.format "%#10o" I.minus_one);
  Printf.printf "format %%#10o 2^30 = /%s/\n" (I.format "%#10o" p30);
  Printf.printf "format %%#10o -2^30 = /%s/\n" (I.format "%#10o" (I.neg p30));
  Printf.printf "format %%#-10x 0 = /%s/\n" (I.format "%#-10x" I.zero);
  Printf.printf "format %%#-10x 1 = /%s/\n" (I.format "%#-10x" I.one);
  Printf.printf "format %%#-10x -1 = /%s/\n" (I.format "%#-10x" I.minus_one);
  Printf.printf "format %%#-10x 2^30 = /%s/\n" (I.format "%#-10x" p30);
  Printf.printf "format %%#-10x -2^30 = /%s/\n" (I.format "%#-10x" (I.neg p30));
  Printf.printf "format %%#-10X 0 = /%s/\n" (I.format "%#-10X" I.zero);
  Printf.printf "format %%#-10X 1 = /%s/\n" (I.format "%#-10X" I.one);
  Printf.printf "format %%#-10X -1 = /%s/\n" (I.format "%#-10X" I.minus_one);
  Printf.printf "format %%#-10X 2^30 = /%s/\n" (I.format "%#-10X" p30);
  Printf.printf "format %%#-10X -2^30 = /%s/\n" (I.format "%#-10X" (I.neg p30));
  Printf.printf "format %%#-10o 0 = /%s/\n" (I.format "%#-10o" I.zero);
  Printf.printf "format %%#-10o 1 = /%s/\n" (I.format "%#-10o" I.one);
  Printf.printf "format %%#-10o -1 = /%s/\n" (I.format "%#-10o" I.minus_one);
  Printf.printf "format %%#-10o 2^30 = /%s/\n" (I.format "%#-10o" p30);
  Printf.printf "format %%#-10o -2^30 = /%s/\n" (I.format "%#-10o" (I.neg p30));

  let extract_testdata =
    let a = I.of_int 42
    and b = I.of_int (-42)
    and c = I.of_string "3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701" in
    [a,0,1; a,0,5; a,0,32; a,0,64;
     a,1,1; a,1,5; a,1,32; a,1,63; a,1,64; a,1,127; a,1,128;
     a,69,12;
     b,0,1; b,0,5; b,0,32; b,0,64;
     b,1,1; b,1,5; b,1,32; b,1,63; b,1,64; b,1,127; b,1,128;
     b,69,12;
     c,0,1; c,0,64; c,128,1; c,128,5; c,131,32; c,175,63; c,277,123] in
  List.iter chk_extract extract_testdata;
  List.iter chk_signed_extract extract_testdata;

  chk_bits I.zero;
  chk_bits p2;
  chk_bits (I.neg p2);
  chk_bits p30;
  chk_bits (I.neg p30);
  chk_bits p62;
  chk_bits (I.neg p62);
  chk_bits p300;
  chk_bits p120;
  chk_bits p121;
  chk_bits maxi;
  chk_bits mini;
  chk_bits maxi32;
  chk_bits mini32;
  chk_bits maxi64;
  chk_bits mini64;
  chk_bits maxni;
  chk_bits minni;

  List.iter chk_testbit [
    I.zero; I.one; I.of_int (-42);
    I.of_string "31415926535897932384626433832795028841971693993751058209749445923078164062862089986";
    I.neg (I.shift_left (I.of_int 123456) 64);
  ];

  List.iter chk_numbits_tz [
    I.zero; I.one; I.of_int (-42);
    I.shift_left (I.of_int 9999) 77;
    I.neg (I.shift_left (I.of_int 123456) 64);
  ];
  ()


(* testing Q *)

(* gcd extended to: gcd x 0 = gcd 0 x = 0 *)
let gcd2 a b =
  if Z.sign a = 0 then b
  else if Z.sign b = 0 then a
  else Z.gcd a b

(* check invariant *)
let check x =
  assert (Z.sign x.Q.den >= 0);
  assert (Z.compare (gcd2 x.Q.num x.Q.den) Z.one <= 0)


let t_list = [Q.zero;Q.one;Q.minus_one;Q.inf;Q.minus_inf;Q.undef]

let test1 msg op =
  List.iter
    (fun x ->
      let r = op x in
      check r;
      Printf.printf "%s %s = %s\n" msg (Q.to_string x)  (Q.to_string r)
    ) t_list

let test2 msg op =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          let r = op x y in
          check r;
          Printf.printf "%s %s %s = %s\n" (Q.to_string x) msg (Q.to_string y) (Q.to_string r)
        ) t_list
    ) t_list

let test_Q () =
  let _ = List.iter check t_list in
  let _ = test1 "-" Q.neg in
  let _ = test1 "1/" Q.inv in
  let _ = test1 "abs" Q.abs in
  let _ = test2 "+" Q.add in
  let _ = test2 "-" Q.sub in
  let _ = test2 "*" Q.mul in
  let _ = test2 "/" Q.div in
  let _ = test2 "* 1/" (fun a b -> Q.mul a (Q.inv b)) in
  let _ = test1 "mul_2exp (1) " (fun a -> Q.mul_2exp a 1) in
  let _ = test1 "mul_2exp (2) " (fun a -> Q.mul_2exp a 2) in
  let _ = test1 "div_2exp (1) " (fun a -> Q.div_2exp a 1) in
  let _ = test1 "div_2exp (2) " (fun a -> Q.div_2exp a 2) in
  (* check simple identitites *)
  List.iter
    (fun x ->
      assert (0 = Q.compare x (Q.div_2exp (Q.mul_2exp x 2) 2));
      assert (0 = Q.compare x (Q.mul_2exp (Q.div_2exp x 2) 2));
      List.iter
        (fun y ->
          Printf.printf "identity checking %s %s\n" (Q.to_string x) (Q.to_string y);
          assert (0 = Q.compare (Q.add x y) (Q.add y x));
          assert (0 = Q.compare (Q.sub x y) (Q.neg (Q.sub y x)));
          assert (0 = Q.compare (Q.sub x y) (Q.add x (Q.neg y)));
          assert (0 = Q.compare (Q.mul x y) (Q.mul y x));
          assert (0 = Q.compare (Q.div x y) (Q.mul x (Q.inv y)));
        ) t_list
    ) t_list;
  assert (Q.compare Q.undef Q.undef = 0);
  assert (not (Q.equal Q.undef Q.undef));
  assert (not (Q.lt  Q.undef Q.undef));
  assert (not (Q.leq Q.undef Q.undef));
  assert (not (Q.gt  Q.undef Q.undef));
  assert (not (Q.geq Q.undef Q.undef))


(* main *)

let _ = test_Z()
let _ = test_Q()
