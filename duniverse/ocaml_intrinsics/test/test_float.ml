[%%import "config.h"]

open! Base
open! Stdio
module I = Ocaml_intrinsics.Float

let same_bits x y =
  let open Int64 in
  bits_of_float x = bits_of_float y
;;

let args =
  [ Float.infinity
  ; -0x1p+63
  ; -0x1p+62
  ; -3.50
  ; -3.25
  ; -3.00
  ; -2.75
  ; -2.50
  ; -2.25
  ; -2.00
  ; -1.75
  ; -1.50
  ; -1.25
  ; -1.00
  ; -0.75
  ; -0.50
  ; -0.25
  ; -0.00
  ; 0.00
  ; 0.25
  ; 0.50
  ; 0.75
  ; 1.00
  ; 1.25
  ; 1.50
  ; 1.75
  ; 2.00
  ; 2.25
  ; 2.50
  ; 2.75
  ; 3.00
  ; 3.25
  ; 3.50
  ; 0x1.fffffffffffffp+61
  ; 0x1.fffffffffffffp+62
  ; Float.neg_infinity
  ; Float.nan
  ]
;;

let%expect_test "iround_half_to_even" =
  List.iter args ~f:(fun x ->
    printf "iround_half_to_even %.19g = %Ld\n" x (I.iround_half_to_even x));
  [%expect
    {|
    iround_half_to_even inf = -9223372036854775808
    iround_half_to_even -9223372036854775808 = -9223372036854775808
    iround_half_to_even -4611686018427387904 = -4611686018427387904
    iround_half_to_even -3.5 = -4
    iround_half_to_even -3.25 = -3
    iround_half_to_even -3 = -3
    iround_half_to_even -2.75 = -3
    iround_half_to_even -2.5 = -2
    iround_half_to_even -2.25 = -2
    iround_half_to_even -2 = -2
    iround_half_to_even -1.75 = -2
    iround_half_to_even -1.5 = -2
    iround_half_to_even -1.25 = -1
    iround_half_to_even -1 = -1
    iround_half_to_even -0.75 = -1
    iround_half_to_even -0.5 = 0
    iround_half_to_even -0.25 = 0
    iround_half_to_even -0 = 0
    iround_half_to_even 0 = 0
    iround_half_to_even 0.25 = 0
    iround_half_to_even 0.5 = 0
    iround_half_to_even 0.75 = 1
    iround_half_to_even 1 = 1
    iround_half_to_even 1.25 = 1
    iround_half_to_even 1.5 = 2
    iround_half_to_even 1.75 = 2
    iround_half_to_even 2 = 2
    iround_half_to_even 2.25 = 2
    iround_half_to_even 2.5 = 2
    iround_half_to_even 2.75 = 3
    iround_half_to_even 3 = 3
    iround_half_to_even 3.25 = 3
    iround_half_to_even 3.5 = 4
    iround_half_to_even 4611686018427387392 = 4611686018427387392
    iround_half_to_even 9223372036854774784 = 9223372036854774784
    iround_half_to_even -inf = -9223372036854775808
    iround_half_to_even nan = -9223372036854775808 |}]
;;

let%expect_test "min and max" =
  let args =
    [ 0., 1.
    ; 1., 0.
    ; Float.neg_infinity, Float.infinity
    ; Float.infinity, Float.neg_infinity
    ; -0., 0.
    ; 0., -0.
    ; Float.nan, 0.
    ; 0., Float.nan
    ]
  in
  List.iter args ~f:(fun (x, y) -> printf "min %.19g %.19g = %.19g\n" x y (I.min x y));
  [%expect
    {|
    min 0 1 = 0
    min 1 0 = 0
    min -inf inf = -inf
    min inf -inf = -inf
    min -0 0 = 0
    min 0 -0 = -0
    min nan 0 = 0
    min 0 nan = nan |}];
  List.iter args ~f:(fun (x, y) -> printf "max %.19g %.19g = %.19g\n" x y (I.max x y));
  [%expect
    {|
    max 0 1 = 1
    max 1 0 = 1
    max -inf inf = inf
    max inf -inf = inf
    max -0 0 = 0
    max 0 -0 = -0
    max nan 0 = 0
    max 0 nan = nan |}]
;;

let%expect_test "round" =
  List.iter args ~f:(fun x ->
    let base_res = Base.Float.round_nearest_half_to_even x in
    let res = I.round_half_to_even x in
    printf "round even  %.19g = %.19g" x res;
    if same_bits res base_res
    then printf "\n"
    else printf " != %.19g in base\n" base_res);
  [%expect
    {|
    round even  inf = inf
    round even  -9223372036854775808 = -9223372036854775808
    round even  -4611686018427387904 = -4611686018427387904
    round even  -3.5 = -4
    round even  -3.25 = -3
    round even  -3 = -3
    round even  -2.75 = -3
    round even  -2.5 = -2
    round even  -2.25 = -2
    round even  -2 = -2
    round even  -1.75 = -2
    round even  -1.5 = -2
    round even  -1.25 = -1
    round even  -1 = -1
    round even  -0.75 = -1
    round even  -0.5 = -0 != 0 in base
    round even  -0.25 = -0 != 0 in base
    round even  -0 = -0
    round even  0 = 0
    round even  0.25 = 0
    round even  0.5 = 0
    round even  0.75 = 1
    round even  1 = 1
    round even  1.25 = 1
    round even  1.5 = 2
    round even  1.75 = 2
    round even  2 = 2
    round even  2.25 = 2
    round even  2.5 = 2
    round even  2.75 = 3
    round even  3 = 3
    round even  3.25 = 3
    round even  3.5 = 4
    round even  4611686018427387392 = 4611686018427387392
    round even  9223372036854774784 = 9223372036854774784
    round even  -inf = -inf
    round even  nan = nan |}];
  List.iter args ~f:(fun x -> printf "round up %.19g = %.19g\n" x (I.round_up x));
  [%expect
    {|
 round up inf = inf
 round up -9223372036854775808 = -9223372036854775808
 round up -4611686018427387904 = -4611686018427387904
 round up -3.5 = -3
 round up -3.25 = -3
 round up -3 = -3
 round up -2.75 = -2
 round up -2.5 = -2
 round up -2.25 = -2
 round up -2 = -2
 round up -1.75 = -1
 round up -1.5 = -1
 round up -1.25 = -1
 round up -1 = -1
 round up -0.75 = -0
 round up -0.5 = -0
 round up -0.25 = -0
 round up -0 = -0
 round up 0 = 0
 round up 0.25 = 1
 round up 0.5 = 1
 round up 0.75 = 1
 round up 1 = 1
 round up 1.25 = 2
 round up 1.5 = 2
 round up 1.75 = 2
 round up 2 = 2
 round up 2.25 = 3
 round up 2.5 = 3
 round up 2.75 = 3
 round up 3 = 3
 round up 3.25 = 4
 round up 3.5 = 4
 round up 4611686018427387392 = 4611686018427387392
 round up 9223372036854774784 = 9223372036854774784
 round up -inf = -inf
 round up nan = nan |}];
  List.iter args ~f:(fun x -> printf "round down %.19g = %.19g\n" x (I.round_down x));
  [%expect
    {|
    round down inf = inf
    round down -9223372036854775808 = -9223372036854775808
    round down -4611686018427387904 = -4611686018427387904
    round down -3.5 = -4
    round down -3.25 = -4
    round down -3 = -3
    round down -2.75 = -3
    round down -2.5 = -3
    round down -2.25 = -3
    round down -2 = -2
    round down -1.75 = -2
    round down -1.5 = -2
    round down -1.25 = -2
    round down -1 = -1
    round down -0.75 = -1
    round down -0.5 = -1
    round down -0.25 = -1
    round down -0 = -0
    round down 0 = 0
    round down 0.25 = 0
    round down 0.5 = 0
    round down 0.75 = 0
    round down 1 = 1
    round down 1.25 = 1
    round down 1.5 = 1
    round down 1.75 = 1
    round down 2 = 2
    round down 2.25 = 2
    round down 2.5 = 2
    round down 2.75 = 2
    round down 3 = 3
    round down 3.25 = 3
    round down 3.5 = 3
    round down 4611686018427387392 = 4611686018427387392
    round down 9223372036854774784 = 9223372036854774784
    round down -inf = -inf
    round down nan = nan |}];
  List.iter args ~f:(fun x ->
    printf "round to zero %.19g = %.19g\n" x (I.round_towards_zero x));
  [%expect
    {|
    round to zero inf = inf
    round to zero -9223372036854775808 = -9223372036854775808
    round to zero -4611686018427387904 = -4611686018427387904
    round to zero -3.5 = -3
    round to zero -3.25 = -3
    round to zero -3 = -3
    round to zero -2.75 = -2
    round to zero -2.5 = -2
    round to zero -2.25 = -2
    round to zero -2 = -2
    round to zero -1.75 = -1
    round to zero -1.5 = -1
    round to zero -1.25 = -1
    round to zero -1 = -1
    round to zero -0.75 = -0
    round to zero -0.5 = -0
    round to zero -0.25 = -0
    round to zero -0 = -0
    round to zero 0 = 0
    round to zero 0.25 = 0
    round to zero 0.5 = 0
    round to zero 0.75 = 0
    round to zero 1 = 1
    round to zero 1.25 = 1
    round to zero 1.5 = 1
    round to zero 1.75 = 1
    round to zero 2 = 2
    round to zero 2.25 = 2
    round to zero 2.5 = 2
    round to zero 2.75 = 2
    round to zero 3 = 3
    round to zero 3.25 = 3
    round to zero 3.5 = 3
    round to zero 4611686018427387392 = 4611686018427387392
    round to zero 9223372036854774784 = 9223372036854774784
    round to zero -inf = -inf
    round to zero nan = nan |}]
;;

[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "bit_of_float" =
  List.iter args ~f:(fun x ->
    let y = Stdlib.Int64.bits_of_float x in
    let z = Stdlib.Int64.float_of_bits y in
    let check = if same_bits x z then "" else " FAIL " in
    printf "bits_of_float %.19g = %Ld -> %.19g%s\n" x y z check);
  [%expect
    {|
    bits_of_float inf = 9218868437227405312 -> inf
    bits_of_float -9223372036854775808 = -4332462841530417152 -> -9223372036854775808
    bits_of_float -4611686018427387904 = -4336966441157787648 -> -4611686018427387904
    bits_of_float -3.5 = -4608308318706860032 -> -3.5
    bits_of_float -3.25 = -4608871268660281344 -> -3.25
    bits_of_float -3 = -4609434218613702656 -> -3
    bits_of_float -2.75 = -4609997168567123968 -> -2.75
    bits_of_float -2.5 = -4610560118520545280 -> -2.5
    bits_of_float -2.25 = -4611123068473966592 -> -2.25
    bits_of_float -2 = -4611686018427387904 -> -2
    bits_of_float -1.75 = -4612811918334230528 -> -1.75
    bits_of_float -1.5 = -4613937818241073152 -> -1.5
    bits_of_float -1.25 = -4615063718147915776 -> -1.25
    bits_of_float -1 = -4616189618054758400 -> -1
    bits_of_float -0.75 = -4618441417868443648 -> -0.75
    bits_of_float -0.5 = -4620693217682128896 -> -0.5
    bits_of_float -0.25 = -4625196817309499392 -> -0.25
    bits_of_float -0 = -9223372036854775808 -> -0
    bits_of_float 0 = 0 -> 0
    bits_of_float 0.25 = 4598175219545276416 -> 0.25
    bits_of_float 0.5 = 4602678819172646912 -> 0.5
    bits_of_float 0.75 = 4604930618986332160 -> 0.75
    bits_of_float 1 = 4607182418800017408 -> 1
    bits_of_float 1.25 = 4608308318706860032 -> 1.25
    bits_of_float 1.5 = 4609434218613702656 -> 1.5
    bits_of_float 1.75 = 4610560118520545280 -> 1.75
    bits_of_float 2 = 4611686018427387904 -> 2
    bits_of_float 2.25 = 4612248968380809216 -> 2.25
    bits_of_float 2.5 = 4612811918334230528 -> 2.5
    bits_of_float 2.75 = 4613374868287651840 -> 2.75
    bits_of_float 3 = 4613937818241073152 -> 3
    bits_of_float 3.25 = 4614500768194494464 -> 3.25
    bits_of_float 3.5 = 4615063718147915776 -> 3.5
    bits_of_float 4611686018427387392 = 4886405595696988159 -> 4611686018427387392
    bits_of_float 9223372036854774784 = 4890909195324358655 -> 9223372036854774784
    bits_of_float -inf = -4503599627370496 -> -inf
    bits_of_float nan = 9218868437227405313 -> nan |}]
;;

[%%else]

let%expect_test "bit_of_float" =
  List.iter args ~f:(fun x ->
    if not (Float.is_nan x)
    then (
      let y = Stdlib.Int64.bits_of_float x in
      let z = Stdlib.Int64.float_of_bits y in
      let check = if same_bits x z then "" else " FAIL " in
      printf "bits_of_float %.19g = %Ld -> %.19g%s\n" x y z check));
  [%expect
    {|
    bits_of_float inf = 9218868437227405312 -> inf
    bits_of_float -9223372036854775808 = -4332462841530417152 -> -9223372036854775808
    bits_of_float -4611686018427387904 = -4336966441157787648 -> -4611686018427387904
    bits_of_float -3.5 = -4608308318706860032 -> -3.5
    bits_of_float -3.25 = -4608871268660281344 -> -3.25
    bits_of_float -3 = -4609434218613702656 -> -3
    bits_of_float -2.75 = -4609997168567123968 -> -2.75
    bits_of_float -2.5 = -4610560118520545280 -> -2.5
    bits_of_float -2.25 = -4611123068473966592 -> -2.25
    bits_of_float -2 = -4611686018427387904 -> -2
    bits_of_float -1.75 = -4612811918334230528 -> -1.75
    bits_of_float -1.5 = -4613937818241073152 -> -1.5
    bits_of_float -1.25 = -4615063718147915776 -> -1.25
    bits_of_float -1 = -4616189618054758400 -> -1
    bits_of_float -0.75 = -4618441417868443648 -> -0.75
    bits_of_float -0.5 = -4620693217682128896 -> -0.5
    bits_of_float -0.25 = -4625196817309499392 -> -0.25
    bits_of_float -0 = -9223372036854775808 -> -0
    bits_of_float 0 = 0 -> 0
    bits_of_float 0.25 = 4598175219545276416 -> 0.25
    bits_of_float 0.5 = 4602678819172646912 -> 0.5
    bits_of_float 0.75 = 4604930618986332160 -> 0.75
    bits_of_float 1 = 4607182418800017408 -> 1
    bits_of_float 1.25 = 4608308318706860032 -> 1.25
    bits_of_float 1.5 = 4609434218613702656 -> 1.5
    bits_of_float 1.75 = 4610560118520545280 -> 1.75
    bits_of_float 2 = 4611686018427387904 -> 2
    bits_of_float 2.25 = 4612248968380809216 -> 2.25
    bits_of_float 2.5 = 4612811918334230528 -> 2.5
    bits_of_float 2.75 = 4613374868287651840 -> 2.75
    bits_of_float 3 = 4613937818241073152 -> 3
    bits_of_float 3.25 = 4614500768194494464 -> 3.25
    bits_of_float 3.5 = 4615063718147915776 -> 3.5
    bits_of_float 4611686018427387392 = 4886405595696988159 -> 4611686018427387392
    bits_of_float 9223372036854774784 = 4890909195324358655 -> 9223372036854774784
    bits_of_float -inf = -4503599627370496 -> -inf |}]
;;

[%%endif]
