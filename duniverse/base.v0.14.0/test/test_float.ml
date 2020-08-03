open! Import
open! Float
open! Float.Private

let%expect_test ("hash coherence"[@tags "64-bits-only"]) =
  check_hash_coherence [%here] (module Float) [ min_value; 0.; 37.; max_value ];
  [%expect {| |}]
;;

let exponent_bits = 11
let mantissa_bits = 52
let exponent_mask64 = Int64.(shift_left one exponent_bits - one)
let exponent_mask = Int64.to_int_exn exponent_mask64
let mantissa_mask = Int63.(shift_left one mantissa_bits - one)
let _mantissa_mask64 = Int63.to_int64 mantissa_mask

let%test_unit "upper/lower_bound_for_int" =
  assert (
    [%compare.equal: (int * t * t) list]
      ([ 8; 16; 31; 32; 52; 53; 54; 62; 63; 64 ]
       |> List.map ~f:(fun x -> x, lower_bound_for_int x, upper_bound_for_int x))
      [ 8, -128.99999999999997, 127.99999999999999
      ; 16, -32768.999999999993, 32767.999999999996
      ; 31, -1073741824.9999998, 1073741823.9999999
      ; 32, -2147483648.9999995, 2147483647.9999998
      ; 52, -2251799813685248.5, 2251799813685247.8
      ; 53, -4503599627370496., 4503599627370495.5
      ; 54, -9007199254740992., 9007199254740991.
      ; 62, -2305843009213693952., 2305843009213693696.
      ; 63, -4611686018427387904., 4611686018427387392.
      ; 64, -9223372036854775808., 9223372036854774784.
      ])
;;

let%test_unit _ =
  (* on 64-bit platform ppx_hash hashes floats exactly the same as polymorphic hash *)
  match Word_size.word_size with
  | W32 -> ()
  | W64 ->
    List.iter
      ~f:(fun float ->
        let hash1 = Caml.Hashtbl.hash float in
        let hash2 = [%hash: float] float in
        let hash3 = specialized_hash float in
        if not Int.(hash1 = hash2 && hash1 = hash3)
        then
          raise_s
            [%message "bad" (hash1 : Int.Hex.t) (hash2 : Int.Hex.t) (hash3 : Int.Hex.t)])
      [ 0.926038888360971146; 34.1638588598232076 ]
;;

let test_both_ways (a : t) (b : int64) =
  Int64.( = ) (to_int64_preserve_order_exn a) b
  && Float.( = ) (of_int64_preserve_order b) a
;;

let%test _ = test_both_ways 0. 0L
let%test _ = test_both_ways (-0.) 0L
let%test _ = test_both_ways 1. Int64.(shift_left 1023L 52)
let%test _ = test_both_ways (-2.) Int64.(neg (shift_left 1024L 52))
let%test _ = test_both_ways infinity Int64.(shift_left 2047L 52)
let%test _ = test_both_ways neg_infinity Int64.(neg (shift_left 2047L 52))
let%test _ = one_ulp `Down infinity = max_finite_value
let%test _ = is_nan (one_ulp `Up infinity)
let%test _ = is_nan (one_ulp `Down neg_infinity)
let%test _ = one_ulp `Up neg_infinity = ~-.max_finite_value

(* Some tests to make sure that the compiler is generating code for handling subnormal
   numbers at runtime accurately. *)
let x () = min_positive_subnormal_value
let y () = min_positive_normal_value

let%test _ = test_both_ways (x ()) 1L
let%test _ = test_both_ways (y ()) Int64.(shift_left 1L 52)
let%test _ = x () > 0.
let%test_unit _ = [%test_result: float] (x () /. 2.) ~expect:0.
let%test _ = one_ulp `Up 0. = x ()
let%test _ = one_ulp `Down 0. = ~-.(x ())

let are_one_ulp_apart a b = one_ulp `Up a = b

let%test _ = are_one_ulp_apart (x ()) (2. *. x ())
let%test _ = are_one_ulp_apart (2. *. x ()) (3. *. x ())

let one_ulp_below_y () = y () -. x ()

let%test _ = one_ulp_below_y () < y ()
let%test _ = y () -. one_ulp_below_y () = x ()
let%test _ = are_one_ulp_apart (one_ulp_below_y ()) (y ())

let one_ulp_above_y () = y () +. x ()

let%test _ = y () < one_ulp_above_y ()
let%test _ = one_ulp_above_y () -. y () = x ()
let%test _ = are_one_ulp_apart (y ()) (one_ulp_above_y ())
let%test _ = not (are_one_ulp_apart (one_ulp_below_y ()) (one_ulp_above_y ()))

(* [2 * min_positive_normal_value] is where the ulp increases for the first time. *)
let z () = 2. *. y ()
let one_ulp_below_z () = z () -. x ()

let%test _ = one_ulp_below_z () < z ()
let%test _ = z () -. one_ulp_below_z () = x ()
let%test _ = are_one_ulp_apart (one_ulp_below_z ()) (z ())

let one_ulp_above_z () = z () +. (2. *. x ())

let%test _ = z () < one_ulp_above_z ()
let%test _ = one_ulp_above_z () -. z () = 2. *. x ()
let%test _ = are_one_ulp_apart (z ()) (one_ulp_above_z ())

let%test_module "clamp" =
  (module struct
    let%test _ = clamp_exn 1.0 ~min:2. ~max:3. = 2.
    let%test _ = clamp_exn 2.5 ~min:2. ~max:3. = 2.5
    let%test _ = clamp_exn 3.5 ~min:2. ~max:3. = 3.

    let%test_unit "clamp" =
      [%test_result: float Or_error.t] (clamp 3.5 ~min:2. ~max:3.) ~expect:(Ok 3.)
    ;;

    let%test_unit "clamp nan" =
      [%test_result: float Or_error.t] (clamp nan ~min:2. ~max:3.) ~expect:(Ok nan)
    ;;

    let%test "clamp bad" = Or_error.is_error (clamp 2.5 ~min:3. ~max:2.)
  end)
;;

let%test_unit _ =
  [%test_result: Int64.t]
    (Int64.bits_of_float 1.1235582092889474E+307)
    ~expect:0x7fb0000000000000L
;;

let%test_module "IEEE" =
  (module struct
    (* Note: IEEE 754 defines NaN values to be those where the exponent is all 1s and the
       mantissa is nonzero.  test_result<t> sees nan values as equal because it is based
       on [compare] rather than [=].  (If [x] and [x'] are nan, [compare x x'] returns 0,
       whereas [x = x'] returns [false].  This is the case regardless of whether or not
       [x] and [x'] are bit-identical values of nan.)  *)
    let f (t : t) (negative : bool) (exponent : int) (mantissa : Int63.t) : unit =
      let str = to_string t in
      let is_nan = is_nan t in
      (* the sign doesn't matter when nan *)
      if not is_nan
      then
        [%test_result: bool]
          ~message:("ieee_negative " ^ str)
          (ieee_negative t)
          ~expect:negative;
      [%test_result: int]
        ~message:("ieee_exponent " ^ str)
        (ieee_exponent t)
        ~expect:exponent;
      if is_nan
      then assert (Int63.(zero <> ieee_mantissa t))
      else
        [%test_result: Int63.t]
          ~message:("ieee_mantissa " ^ str)
          (ieee_mantissa t)
          ~expect:mantissa;
      [%test_result: t]
        ~message:
          (Printf.sprintf
             !"create_ieee ~negative:%B ~exponent:%d ~mantissa:%{Int63}"
             negative
             exponent
             mantissa)
        (create_ieee_exn ~negative ~exponent ~mantissa)
        ~expect:t
    ;;

    let%test_unit _ =
      let ( !! ) x = Int63.of_int x in
      f zero false 0 !!0;
      f min_positive_subnormal_value false 0 !!1;
      f min_positive_normal_value false 1 !!0;
      f epsilon_float false Int.(1023 - mantissa_bits) !!0;
      f one false 1023 !!0;
      f minus_one true 1023 !!0;
      f max_finite_value false Int.(exponent_mask - 1) mantissa_mask;
      f infinity false exponent_mask !!0;
      f neg_infinity true exponent_mask !!0;
      f nan false exponent_mask !!1
    ;;

    (* test the normalized case, that is, 1 <= exponent <= 2046 *)
    let%test_unit _ =
      let g ~negative ~exponent ~mantissa =
        assert (
          create_ieee_exn ~negative ~exponent ~mantissa:(Int63.of_int64_exn mantissa)
          = (if negative then -1. else 1.)
            * (2. **. (Float.of_int exponent - 1023.))
            * (1. + ((2. **. -52.) * Int64.to_float mantissa)))
      in
      g ~negative:false ~exponent:1 ~mantissa:147L;
      g ~negative:true ~exponent:137 ~mantissa:13L;
      g ~negative:false ~exponent:1015 ~mantissa:1370001L;
      g ~negative:true ~exponent:2046 ~mantissa:137000100945L
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let test f expect =
      let actual = to_padded_compact_string f in
      if String.(actual <> expect)
      then raise_s [%message "failure" (f : t) (expect : string) (actual : string)]
    ;;

    let both f expect =
      assert (f > 0.);
      test f expect;
      test ~-.f ("-" ^ expect)
    ;;

    let decr = one_ulp `Down
    let incr = one_ulp `Up

    let boundary f ~closer_to_zero ~at =
      assert (f > 0.);
      (* If [f] looks like an odd multiple of 0.05, it might be slightly under-represented
         as a float, e.g.

         1. -. 0.95 = 0.0500000000000000444

         In such case, sadly, the right way for [to_padded_compact_string], just as for
         [sprintf "%.1f"], is to round it down.  However, the next representable number
         should be rounded up:

         # let x = 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x;;
         - : string = "1 / 0.9 / 0.95 / 0.950 / 0.94999999999999995559"

         # let x = incr 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x ;;
         - : string = "1 / 1.0 / 0.95 / 0.950 / 0.95000000000000006661"

      *)
      let f =
        if f >= 1000.
        then f
        else (
          let x = Printf.sprintf "%.20f" f in
          let spot = String.index_exn x '.' in
          (* the following condition is only meant to work for small multiples of 0.05 *)
          let ( + ) = Int.( + ) in
          let ( = ) = Char.( = ) in
          if x.[spot + 2] = '4' && x.[spot + 3] = '9' && x.[spot + 4] = '9'
          then (* something like 0.94999999999999995559 *)
            incr f
          else f)
      in
      both (decr f) closer_to_zero;
      both f at
    ;;

    let%test_unit _ = test nan "nan  "
    let%test_unit _ = test 0.0 "0  "
    let%test_unit _ = both min_positive_subnormal_value "0  "
    let%test_unit _ = both infinity "inf  "
    let%test_unit _ = boundary 0.05 ~closer_to_zero:"0  " ~at:"0.1"
    let%test_unit _ = boundary 0.15 ~closer_to_zero:"0.1" ~at:"0.2"

    (* glibc printf resolves ties to even, cf.
       http://www.exploringbinary.com/inconsistent-rounding-of-printed-floating-point-numbers/
       Ties are resolved differently in JavaScript - mark some tests as no running with JavaScript.
    *)
    let%test_unit (_[@tags "no-js"]) =
      boundary (* tie *) 0.25 ~closer_to_zero:"0.2" ~at:"0.2"
    ;;

    let%test_unit (_[@tags "no-js"]) =
      boundary (incr 0.25) ~closer_to_zero:"0.2" ~at:"0.3"
    ;;

    let%test_unit _ = boundary 0.35 ~closer_to_zero:"0.3" ~at:"0.4"
    let%test_unit _ = boundary 0.45 ~closer_to_zero:"0.4" ~at:"0.5"
    let%test_unit _ = both 0.50 "0.5"
    let%test_unit _ = boundary 0.55 ~closer_to_zero:"0.5" ~at:"0.6"
    let%test_unit _ = boundary 0.65 ~closer_to_zero:"0.6" ~at:"0.7"
    (* this time tie-to-even means round away from 0 *)
    let%test_unit _ = boundary (* tie *) 0.75 ~closer_to_zero:"0.7" ~at:"0.8"
    let%test_unit _ = boundary 0.85 ~closer_to_zero:"0.8" ~at:"0.9"
    let%test_unit _ = boundary 0.95 ~closer_to_zero:"0.9" ~at:"1  "
    let%test_unit _ = boundary 1.05 ~closer_to_zero:"1  " ~at:"1.1"
    let%test_unit (_[@tags "no-js"]) = boundary 3.25 ~closer_to_zero:"3.2" ~at:"3.2"

    let%test_unit (_[@tags "no-js"]) =
      boundary (incr 3.25) ~closer_to_zero:"3.2" ~at:"3.3"
    ;;

    let%test_unit _ = boundary 3.75 ~closer_to_zero:"3.7" ~at:"3.8"
    let%test_unit _ = boundary 9.95 ~closer_to_zero:"9.9" ~at:"10  "
    let%test_unit _ = boundary 10.05 ~closer_to_zero:"10  " ~at:"10.1"
    let%test_unit _ = boundary 100.05 ~closer_to_zero:"100  " ~at:"100.1"

    let%test_unit (_[@tags "no-js"]) =
      boundary (* tie *) 999.25 ~closer_to_zero:"999.2" ~at:"999.2"
    ;;

    let%test_unit (_[@tags "no-js"]) =
      boundary (incr 999.25) ~closer_to_zero:"999.2" ~at:"999.3"
    ;;

    let%test_unit _ = boundary 999.75 ~closer_to_zero:"999.7" ~at:"999.8"
    let%test_unit _ = boundary 999.95 ~closer_to_zero:"999.9" ~at:"1k "
    let%test_unit _ = both 1000. "1k "
    (* some ties which we resolve manually in [iround_ratio_exn] *)
    let%test_unit _ = boundary 1050. ~closer_to_zero:"1k " ~at:"1k "
    let%test_unit _ = boundary (incr 1050.) ~closer_to_zero:"1k " ~at:"1k1"
    let%test_unit _ = boundary 1950. ~closer_to_zero:"1k9" ~at:"2k "
    let%test_unit _ = boundary 3250. ~closer_to_zero:"3k2" ~at:"3k2"
    let%test_unit _ = boundary (incr 3250.) ~closer_to_zero:"3k2" ~at:"3k3"
    let%test_unit _ = boundary 9950. ~closer_to_zero:"9k9" ~at:"10k "
    let%test_unit _ = boundary 33_250. ~closer_to_zero:"33k2" ~at:"33k2"
    let%test_unit _ = boundary (incr 33_250.) ~closer_to_zero:"33k2" ~at:"33k3"
    let%test_unit _ = boundary 33_350. ~closer_to_zero:"33k3" ~at:"33k4"
    let%test_unit _ = boundary 33_750. ~closer_to_zero:"33k7" ~at:"33k8"
    let%test_unit _ = boundary 333_250. ~closer_to_zero:"333k2" ~at:"333k2"
    let%test_unit _ = boundary (incr 333_250.) ~closer_to_zero:"333k2" ~at:"333k3"
    let%test_unit _ = boundary 333_750. ~closer_to_zero:"333k7" ~at:"333k8"
    let%test_unit _ = boundary 999_850. ~closer_to_zero:"999k8" ~at:"999k8"
    let%test_unit _ = boundary (incr 999_850.) ~closer_to_zero:"999k8" ~at:"999k9"
    let%test_unit _ = boundary 999_950. ~closer_to_zero:"999k9" ~at:"1m "
    let%test_unit _ = boundary 1_050_000. ~closer_to_zero:"1m " ~at:"1m "
    let%test_unit _ = boundary (incr 1_050_000.) ~closer_to_zero:"1m " ~at:"1m1"
    let%test_unit _ = boundary 999_950_000. ~closer_to_zero:"999m9" ~at:"1g "
    let%test_unit _ = boundary 999_950_000_000. ~closer_to_zero:"999g9" ~at:"1t "
    let%test_unit _ = boundary 999_950_000_000_000. ~closer_to_zero:"999t9" ~at:"1p "

    let%test_unit _ =
      boundary 999_950_000_000_000_000. ~closer_to_zero:"999p9" ~at:"1.0e+18"
    ;;

    (* Test the boundary between the subnormals and the normals. *)
    let%test_unit _ = boundary min_positive_normal_value ~closer_to_zero:"0  " ~at:"0  "
  end)
;;

let%test "int_pow" =
  let tol = 1e-15 in
  let test (x, n) =
    let reference_value = x **. of_int n in
    let relative_error = (int_pow x n -. reference_value) /. reference_value in
    abs relative_error < tol
  in
  List.for_all
    ~f:test
    [ 1.5, 17
    ; 1.5, 42
    ; 0.99, 64
    ; 2., -5
    ; 2., -1
    ; -1.3, 2
    ; -1.3, -1
    ; -1.3, -2
    ; 5., 0
    ; nan, 0
    ; 0., 0
    ; infinity, 0
    ]
;;

let%test "int_pow misc" =
  int_pow 0. (-1) = infinity
  && int_pow (-0.) (-1) = neg_infinity
  && int_pow (-0.) (-2) = infinity
  && int_pow 1.5 5000 = infinity
  && int_pow 1.5 (-5000) = 0.
  && int_pow (-1.) Int.max_value = -1.
  && int_pow (-1.) Int.min_value = 1.
;;

(* some ugly corner cases with extremely large exponents and some serious precision loss *)
let%test ("int_pow bad cases"[@tags "64-bits-only"]) =
  let a = one_ulp `Down 1. in
  let b = one_ulp `Up 1. in
  let large = 1 lsl 61 in
  let small = Int.neg large in
  (* this huge discrepancy comes from the fact that [1 / a = b] but this is a very poor
     approximation, and in particular [1 / b = one_ulp `Down a = a * a]. *)
  a **. of_int small = 1.5114276650041252e+111
  && int_pow a small = 2.2844048619719663e+222
  && int_pow b large = 2.2844048619719663e+222
  && b **. of_int large = 2.2844135865396268e+222
;;

let%test_unit "sign_exn" =
  List.iter
    ~f:(fun (input, expect) -> assert (Sign.equal (sign_exn input) expect))
    [ 1e-30, Sign.Pos; -0., Zero; 0., Zero; neg_infinity, Neg ]
;;

let%test _ =
  match sign_exn nan with
  | Neg | Zero | Pos -> false
  | exception _ -> true
;;

let%test_unit "sign_or_nan" =
  List.iter
    ~f:(fun (input, expect) -> assert (Sign_or_nan.equal (sign_or_nan input) expect))
    [ 1e-30, Sign_or_nan.Pos; -0., Zero; 0., Zero; neg_infinity, Neg; nan, Nan ]
;;

let%test_module _ =
  (module struct
    let check v expect =
      match Validate.result v, expect with
      | Ok (), `Ok | Error _, `Error -> ()
      | r, expect ->
        raise_s [%message "mismatch" (r : unit Or_error.t) (expect : [ `Ok | `Error ])]
    ;;

    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) nan) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) infinity) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) neg_infinity) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) (-1.)) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 0.) `Ok
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 1.) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) nan) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) infinity) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) neg_infinity) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) (-1.)) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 0.) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 1.) `Error

    (* Some of the following tests used to live in lib_test/core_float_test.ml. *)

    let () = Random.init 137

    (* round:
       ...  <-)[-><-)[-><-)[-><-)[-><-)[-><-)[->   ...
       ... -+-----+-----+-----+-----+-----+-----+- ...
       ... -3    -2    -1     0     1     2     3  ...
       so round x -. x should be in (-0.5,0.5]
    *)
    let round_test x =
      let y = round x in
      -0.5 < y -. x && y -. x <= 0.5
    ;;

    let iround_up_vs_down_test x =
      let expected_difference = if Parts.fractional (modf x) = 0. then 0 else 1 in
      match iround_up x, iround_down x with
      | Some x, Some y -> Int.(x - y = expected_difference)
      | _, _ -> true
    ;;

    let test_all_six
          x
          ~specialized_iround
          ~specialized_iround_exn
          ~float_rounding
          ~dir
          ~validate
      =
      let result1 = iround x ~dir in
      let result2 = Option.try_with (fun () -> iround_exn x ~dir) in
      let result3 = specialized_iround x in
      let result4 = Option.try_with (fun () -> specialized_iround_exn x) in
      let result5 = Option.try_with (fun () -> Int.of_float (float_rounding x)) in
      let result6 = Option.try_with (fun () -> Int.of_float (round ~dir x)) in
      let ( = ) = Caml.( = ) in
      if result1 = result2
      && result2 = result3
      && result3 = result4
      && result4 = result5
      && result5 = result6
      then validate result1
      else false
    ;;

    (* iround ~dir:`Nearest built so this should always be true *)
    let iround_nearest_test x =
      test_all_six
        x
        ~specialized_iround:iround_nearest
        ~specialized_iround_exn:iround_nearest_exn
        ~float_rounding:round_nearest
        ~dir:`Nearest
        ~validate:(function
          | None -> true
          | Some y ->
            let y = of_int y in
            -0.5 < y -. x && y -. x <= 0.5)
    ;;

    (* iround_down:
       ... )[<---)[<---)[<---)[<---)[<---)[<---)[  ...
       ... -+-----+-----+-----+-----+-----+-----+- ...
       ... -3    -2    -1     0     1     2     3  ...
       so x -. iround_down x should be in [0,1)
    *)
    let iround_down_test x =
      test_all_six
        x
        ~specialized_iround:iround_down
        ~specialized_iround_exn:iround_down_exn
        ~float_rounding:round_down
        ~dir:`Down
        ~validate:(function
          | None -> true
          | Some y ->
            let y = of_int y in
            0. <= x -. y && x -. y < 1.)
    ;;

    (* iround_up:
       ...  ](--->](--->](--->](--->](--->](--->]( ...
       ... -+-----+-----+-----+-----+-----+-----+- ...
       ... -3    -2    -1     0     1     2     3  ...
       so iround_up x -. x should be in [0,1)
    *)
    let iround_up_test x =
      test_all_six
        x
        ~specialized_iround:iround_up
        ~specialized_iround_exn:iround_up_exn
        ~float_rounding:round_up
        ~dir:`Up
        ~validate:(function
          | None -> true
          | Some y ->
            let y = of_int y in
            0. <= y -. x && y -. x < 1.)
    ;;

    (* iround_towards_zero:
       ...  ](--->](--->](---><--->)[<---)[<---)[  ...
       ... -+-----+-----+-----+-----+-----+-----+- ...
       ... -3    -2    -1     0     1     2     3  ...
       so abs x -. abs (iround_towards_zero x) should be in [0,1)
    *)
    let iround_towards_zero_test x =
      test_all_six
        x
        ~specialized_iround:iround_towards_zero
        ~specialized_iround_exn:iround_towards_zero_exn
        ~float_rounding:round_towards_zero
        ~dir:`Zero
        ~validate:(function
          | None -> true
          | Some y ->
            let x = abs x in
            let y = abs (of_int y) in
            0. <= x -. y && x -. y < 1. && (Sign.(sign_exn x = sign_exn y) || y = 0.0))
    ;;

    (* Easy cases that used to live inline with the code above. *)
    let%test_unit _ = [%test_result: int option] (iround_up (-3.4)) ~expect:(Some (-3))
    let%test_unit _ = [%test_result: int option] (iround_up 0.0) ~expect:(Some 0)
    let%test_unit _ = [%test_result: int option] (iround_up 3.4) ~expect:(Some 4)
    let%test_unit _ = [%test_result: int] (iround_up_exn (-3.4)) ~expect:(-3)
    let%test_unit _ = [%test_result: int] (iround_up_exn 0.0) ~expect:0
    let%test_unit _ = [%test_result: int] (iround_up_exn 3.4) ~expect:4
    let%test_unit _ = [%test_result: int option] (iround_down (-3.4)) ~expect:(Some (-4))
    let%test_unit _ = [%test_result: int option] (iround_down 0.0) ~expect:(Some 0)
    let%test_unit _ = [%test_result: int option] (iround_down 3.4) ~expect:(Some 3)
    let%test_unit _ = [%test_result: int] (iround_down_exn (-3.4)) ~expect:(-4)
    let%test_unit _ = [%test_result: int] (iround_down_exn 0.0) ~expect:0
    let%test_unit _ = [%test_result: int] (iround_down_exn 3.4) ~expect:3

    let%test_unit _ =
      [%test_result: int option] (iround_towards_zero (-3.4)) ~expect:(Some (-3))
    ;;

    let%test_unit _ =
      [%test_result: int option] (iround_towards_zero 0.0) ~expect:(Some 0)
    ;;

    let%test_unit _ =
      [%test_result: int option] (iround_towards_zero 3.4) ~expect:(Some 3)
    ;;

    let%test_unit _ = [%test_result: int] (iround_towards_zero_exn (-3.4)) ~expect:(-3)
    let%test_unit _ = [%test_result: int] (iround_towards_zero_exn 0.0) ~expect:0
    let%test_unit _ = [%test_result: int] (iround_towards_zero_exn 3.4) ~expect:3

    let%test_unit _ =
      [%test_result: int option] (iround_nearest (-3.6)) ~expect:(Some (-4))
    ;;

    let%test_unit _ =
      [%test_result: int option] (iround_nearest (-3.5)) ~expect:(Some (-3))
    ;;

    let%test_unit _ =
      [%test_result: int option] (iround_nearest (-3.4)) ~expect:(Some (-3))
    ;;

    let%test_unit _ = [%test_result: int option] (iround_nearest 0.0) ~expect:(Some 0)
    let%test_unit _ = [%test_result: int option] (iround_nearest 3.4) ~expect:(Some 3)
    let%test_unit _ = [%test_result: int option] (iround_nearest 3.5) ~expect:(Some 4)
    let%test_unit _ = [%test_result: int option] (iround_nearest 3.6) ~expect:(Some 4)
    let%test_unit _ = [%test_result: int] (iround_nearest_exn (-3.6)) ~expect:(-4)
    let%test_unit _ = [%test_result: int] (iround_nearest_exn (-3.5)) ~expect:(-3)
    let%test_unit _ = [%test_result: int] (iround_nearest_exn (-3.4)) ~expect:(-3)
    let%test_unit _ = [%test_result: int] (iround_nearest_exn 0.0) ~expect:0
    let%test_unit _ = [%test_result: int] (iround_nearest_exn 3.4) ~expect:3
    let%test_unit _ = [%test_result: int] (iround_nearest_exn 3.5) ~expect:4
    let%test_unit _ = [%test_result: int] (iround_nearest_exn 3.6) ~expect:4

    let special_values_test () =
      [%test_result: float] (round (-1.50001)) ~expect:(-2.);
      [%test_result: float] (round (-1.5)) ~expect:(-1.);
      [%test_result: float] (round (-0.50001)) ~expect:(-1.);
      [%test_result: float] (round (-0.5)) ~expect:0.;
      [%test_result: float] (round 0.49999) ~expect:0.;
      [%test_result: float] (round 0.5) ~expect:1.;
      [%test_result: float] (round 1.49999) ~expect:1.;
      [%test_result: float] (round 1.5) ~expect:2.;
      [%test_result: int] (iround_exn ~dir:`Up (-2.)) ~expect:(-2);
      [%test_result: int] (iround_exn ~dir:`Up (-1.9999)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Up (-1.)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Up (-0.9999)) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Up 0.) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Up 0.00001) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Up 1.) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Up 1.00001) ~expect:2;
      [%test_result: int] (iround_up_exn (-2.)) ~expect:(-2);
      [%test_result: int] (iround_up_exn (-1.9999)) ~expect:(-1);
      [%test_result: int] (iround_up_exn (-1.)) ~expect:(-1);
      [%test_result: int] (iround_up_exn (-0.9999)) ~expect:0;
      [%test_result: int] (iround_up_exn 0.) ~expect:0;
      [%test_result: int] (iround_up_exn 0.00001) ~expect:1;
      [%test_result: int] (iround_up_exn 1.) ~expect:1;
      [%test_result: int] (iround_up_exn 1.00001) ~expect:2;
      [%test_result: int] (iround_exn ~dir:`Down (-1.00001)) ~expect:(-2);
      [%test_result: int] (iround_exn ~dir:`Down (-1.)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Down (-0.00001)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Down 0.) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Down 0.99999) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Down 1.) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Down 1.99999) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Down 2.) ~expect:2;
      [%test_result: int] (iround_down_exn (-1.00001)) ~expect:(-2);
      [%test_result: int] (iround_down_exn (-1.)) ~expect:(-1);
      [%test_result: int] (iround_down_exn (-0.00001)) ~expect:(-1);
      [%test_result: int] (iround_down_exn 0.) ~expect:0;
      [%test_result: int] (iround_down_exn 0.99999) ~expect:0;
      [%test_result: int] (iround_down_exn 1.) ~expect:1;
      [%test_result: int] (iround_down_exn 1.99999) ~expect:1;
      [%test_result: int] (iround_down_exn 2.) ~expect:2;
      [%test_result: int] (iround_exn ~dir:`Zero (-2.)) ~expect:(-2);
      [%test_result: int] (iround_exn ~dir:`Zero (-1.99999)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Zero (-1.)) ~expect:(-1);
      [%test_result: int] (iround_exn ~dir:`Zero (-0.99999)) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Zero 0.99999) ~expect:0;
      [%test_result: int] (iround_exn ~dir:`Zero 1.) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Zero 1.99999) ~expect:1;
      [%test_result: int] (iround_exn ~dir:`Zero 2.) ~expect:2
    ;;

    let is_64_bit_platform = of_int Int.max_value >= 2. **. 60.

    (* Tests for values close to [iround_lbound] and [iround_ubound]. *)
    let extremities_test ~round =
      let ( + ) = Int.( + ) in
      let ( - ) = Int.( - ) in
      if is_64_bit_platform
      then (
        (* 64 bits *)
        [%test_result: int option]
          (round ((2.0 **. 62.) -. 512.))
          ~expect:(Some (Int.max_value - 511));
        [%test_result: int option]
          (round ((2.0 **. 62.) -. 1024.))
          ~expect:(Some (Int.max_value - 1023));
        [%test_result: int option] (round (-.(2.0 **. 62.))) ~expect:(Some Int.min_value);
        [%test_result: int option]
          (round (-.((2.0 **. 62.) -. 512.)))
          ~expect:(Some (Int.min_value + 512));
        [%test_result: int option] (round (2.0 **. 62.)) ~expect:None;
        [%test_result: int option] (round (-.((2.0 **. 62.) +. 1024.))) ~expect:None)
      else (
        let int_size_minus_one = of_int (Int.num_bits - 1) in
        (* 32 bits *)
        [%test_result: int option]
          (round ((2.0 **. int_size_minus_one) -. 1.))
          ~expect:(Some Int.max_value);
        [%test_result: int option]
          (round ((2.0 **. int_size_minus_one) -. 2.))
          ~expect:(Some (Int.max_value - 1));
        [%test_result: int option]
          (round (-.(2.0 **. int_size_minus_one)))
          ~expect:(Some Int.min_value);
        [%test_result: int option]
          (round (-.((2.0 **. int_size_minus_one) -. 1.)))
          ~expect:(Some (Int.min_value + 1));
        [%test_result: int option] (round (2.0 **. int_size_minus_one)) ~expect:None;
        [%test_result: int option]
          (round (-.((2.0 **. int_size_minus_one) +. 1.)))
          ~expect:None)
    ;;

    let%test_unit _ = extremities_test ~round:iround_down
    let%test_unit _ = extremities_test ~round:iround_up
    let%test_unit _ = extremities_test ~round:iround_nearest
    let%test_unit _ = extremities_test ~round:iround_towards_zero

    (* test values beyond the integers range *)
    let large_value_test x =
      [%test_result: int option] (iround_down x) ~expect:None;
      [%test_result: int option] (iround ~dir:`Down x) ~expect:None;
      [%test_result: int option] (iround_up x) ~expect:None;
      [%test_result: int option] (iround ~dir:`Up x) ~expect:None;
      [%test_result: int option] (iround_towards_zero x) ~expect:None;
      [%test_result: int option] (iround ~dir:`Zero x) ~expect:None;
      [%test_result: int option] (iround_nearest x) ~expect:None;
      [%test_result: int option] (iround ~dir:`Nearest x) ~expect:None;
      assert (Exn.does_raise (fun () -> iround_down_exn x));
      assert (Exn.does_raise (fun () -> iround_exn ~dir:`Down x));
      assert (Exn.does_raise (fun () -> iround_up_exn x));
      assert (Exn.does_raise (fun () -> iround_exn ~dir:`Up x));
      assert (Exn.does_raise (fun () -> iround_towards_zero_exn x));
      assert (Exn.does_raise (fun () -> iround_exn ~dir:`Zero x));
      assert (Exn.does_raise (fun () -> iround_nearest_exn x));
      assert (Exn.does_raise (fun () -> iround_exn ~dir:`Nearest x));
      [%test_result: float] (round_down x) ~expect:x;
      [%test_result: float] (round ~dir:`Down x) ~expect:x;
      [%test_result: float] (round_up x) ~expect:x;
      [%test_result: float] (round ~dir:`Up x) ~expect:x;
      [%test_result: float] (round_towards_zero x) ~expect:x;
      [%test_result: float] (round ~dir:`Zero x) ~expect:x;
      [%test_result: float] (round_nearest x) ~expect:x;
      [%test_result: float] (round ~dir:`Nearest x) ~expect:x
    ;;

    let large_numbers =
      let ( + ) = Int.( + ) in
      let ( - ) = Int.( - ) in
      List.concat
        (List.init (1024 - 64) ~f:(fun x ->
           let x = of_int (x + 64) in
           let y =
             [ 2. **. x
             ; (2. **. x) -. (2. **. (x -. 53.))
             ; (* one ulp down *)
               (2. **. x) +. (2. **. (x -. 52.))
             ]
             (* one ulp up *)
           in
           y @ List.map y ~f:neg))
      @ [ infinity; neg_infinity ]
    ;;

    let%test_unit _ = List.iter large_numbers ~f:large_value_test

    let numbers_near_powers_of_two =
      List.concat
        (List.init 64 ~f:(fun i ->
           let pow2 = 2. **. of_int i in
           let x =
             [ pow2
             ; one_ulp `Down (pow2 +. 0.5)
             ; pow2 +. 0.5
             ; one_ulp `Down (pow2 +. 1.0)
             ; pow2 +. 1.0
             ; one_ulp `Down (pow2 +. 1.5)
             ; pow2 +. 1.5
             ; one_ulp `Down (pow2 +. 2.0)
             ; pow2 +. 2.0
             ; one_ulp `Down ((pow2 *. 2.0) -. 1.0)
             ; one_ulp `Down pow2
             ; one_ulp `Up pow2
             ]
           in
           x @ List.map x ~f:neg))
    ;;

    let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_up_vs_down_test
    let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_nearest_test
    let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_down_test
    let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_up_test
    let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_towards_zero_test
    let%test _ = List.for_all numbers_near_powers_of_two ~f:round_test

    (* code for generating random floats on which to test functions *)
    let rec absirand () =
      let open Int.O in
      let rec aux acc cnt =
        if cnt = 0
        then acc
        else (
          let bit = if Random.bool () then 1 else 0 in
          aux ((2 * acc) + bit) (cnt - 1))
      in
      let result = aux 0 (if is_64_bit_platform then 62 else 30) in
      if result >= Int.max_value - 255
      then
        (* On a 64-bit box, [float x > Int.max_value] when [x >= Int.max_value - 255], so
           [iround (float x)] would be out of bounds.  So we try again.  This branch of code
           runs with probability 6e-17 :-)  As such, we have some fixed tests in
           [extremities_test] above, to ensure that we do always check some examples in
           that range. *)
        absirand ()
      else result
    ;;

    (* -Int.max_value <= frand () <= Int.max_value *)
    let frand () =
      let x = of_int (absirand ()) +. Random.float 1.0 in
      if Random.bool () then -1.0 *. x else x
    ;;

    let randoms = List.init ~f:(fun _ -> frand ()) 10_000

    let%test _ = List.for_all randoms ~f:iround_up_vs_down_test
    let%test _ = List.for_all randoms ~f:iround_nearest_test
    let%test _ = List.for_all randoms ~f:iround_down_test
    let%test _ = List.for_all randoms ~f:iround_up_test
    let%test _ = List.for_all randoms ~f:iround_towards_zero_test
    let%test _ = List.for_all randoms ~f:round_test
    let%test_unit _ = special_values_test ()
    let%test _ = iround_nearest_test (of_int Int.max_value)
    let%test _ = iround_nearest_test (of_int Int.min_value)
  end)
;;

module Test_bounds (I : sig
    type t

    val num_bits : int
    val of_float : float -> t
    val to_int64 : t -> Int64.t
    val max_value : t
    val min_value : t
  end) =
struct
  open I

  let float_lower_bound = lower_bound_for_int num_bits
  let float_upper_bound = upper_bound_for_int num_bits

  let%test_unit "lower bound is valid" = ignore (of_float float_lower_bound : t)
  let%test_unit "upper bound is valid" = ignore (of_float float_upper_bound : t)

  let%test "smaller than lower bound is not valid" =
    Exn.does_raise (fun () -> of_float (one_ulp `Down float_lower_bound))
  ;;

  let%test "bigger than upper bound is not valid" =
    Exn.does_raise (fun () -> of_float (one_ulp `Up float_upper_bound))
  ;;

  (* We use [Caml.Int64.of_float] in the next two tests because [Int64.of_float] rejects
     out-of-range inputs, whereas [Caml.Int.of_float] simply overflows (returns
     [Int64.min_int]). *)

  let%test "smaller than lower bound overflows" =
    let lower_bound = Int64.of_float float_lower_bound in
    let lower_bound_minus_epsilon =
      Caml.Int64.of_float (one_ulp `Down float_lower_bound)
    in
    let min_value = to_int64 min_value in
    if Int.( = ) num_bits 64
    (* We cannot detect overflow because on Intel overflow results in min_value. *)
    then true
    else (
      assert (Int64.( <= ) lower_bound_minus_epsilon lower_bound);
      (* a value smaller than min_value would overflow if converted to [t] *)
      Int64.( < ) lower_bound_minus_epsilon min_value)
  ;;

  let%test "bigger than upper bound overflows" =
    let upper_bound = Int64.of_float float_upper_bound in
    let upper_bound_plus_epsilon = Caml.Int64.of_float (one_ulp `Up float_upper_bound) in
    let max_value = to_int64 max_value in
    if Int.( = ) num_bits 64
    (* upper_bound_plus_epsilon is not representable as a Int64.t, it has overflowed *)
    then Int64.( < ) upper_bound_plus_epsilon upper_bound
    else (
      assert (Int64.( >= ) upper_bound_plus_epsilon upper_bound);
      (* a value greater than max_value would overflow if converted to [t] *)
      Int64.( > ) upper_bound_plus_epsilon max_value)
  ;;
end

let%test_module "Int" = (module Test_bounds (Int))
let%test_module "Int32" = (module Test_bounds (Int32))
let%test_module "Int63" = (module Test_bounds (Int63))
let%test_module "Int63_emul" = (module Test_bounds (Base.Int63.Private.Emul))
let%test_module "Int64" = (module Test_bounds (Int64))
let%test_module "Nativeint" = (module Test_bounds (Nativeint))
let%test_unit _ = [%test_result: string] (to_string 3.14) ~expect:"3.14"
let%test_unit _ = [%test_result: string] (to_string 3.1400000000000001) ~expect:"3.14"

let%test_unit _ =
  [%test_result: string] (to_string 3.1400000000000004) ~expect:"3.1400000000000006"
;;

let%test_unit _ =
  [%test_result: string] (to_string 8.000000000000002) ~expect:"8.0000000000000018"
;;

let%test_unit _ = [%test_result: string] (to_string 9.992) ~expect:"9.992"

let%test_unit _ =
  [%test_result: string]
    (to_string ((2. **. 63.) *. (1. +. (2. **. -52.))))
    ~expect:"9.2233720368547779e+18"
;;

let%test_unit _ = [%test_result: string] (to_string (-3.)) ~expect:"-3."
let%test_unit _ = [%test_result: string] (to_string nan) ~expect:"nan"
let%test_unit _ = [%test_result: string] (to_string infinity) ~expect:"inf"
let%test_unit _ = [%test_result: string] (to_string neg_infinity) ~expect:"-inf"
let%test_unit _ = [%test_result: string] (to_string 3e100) ~expect:"3e+100"

let%test_unit _ =
  [%test_result: string] (to_string max_finite_value) ~expect:"1.7976931348623157e+308"
;;

let%test_unit _ =
  [%test_result: string]
    (to_string min_positive_subnormal_value)
    ~expect:"4.94065645841247e-324"
;;

let%test _ = epsilon_float = one_ulp `Up 1. -. 1.
let%test _ = one_ulp_less_than_half = 0.49999999999999994
let%test _ = round_down 3.6 = 3. && round_down (-3.6) = -4.
let%test _ = round_up 3.6 = 4. && round_up (-3.6) = -3.
let%test _ = round_towards_zero 3.6 = 3. && round_towards_zero (-3.6) = -3.
let%test _ = round_nearest_half_to_even 0. = 0.
let%test _ = round_nearest_half_to_even 0.5 = 0.
let%test _ = round_nearest_half_to_even (-0.5) = 0.
let%test _ = round_nearest_half_to_even (one_ulp `Up 0.5) = 1.
let%test _ = round_nearest_half_to_even (one_ulp `Down 0.5) = 0.
let%test _ = round_nearest_half_to_even (one_ulp `Up (-0.5)) = 0.
let%test _ = round_nearest_half_to_even (one_ulp `Down (-0.5)) = -1.
let%test _ = round_nearest_half_to_even 3.5 = 4.
let%test _ = round_nearest_half_to_even 4.5 = 4.
let%test _ = round_nearest_half_to_even (one_ulp `Up (-5.5)) = -5.
let%test _ = round_nearest_half_to_even 5.5 = 6.
let%test _ = round_nearest_half_to_even 6.5 = 6.
let%test _ = round_nearest_half_to_even (one_ulp `Up (-.(2. **. 52.))) = -.(2. **. 52.)
let%test _ = round_nearest (one_ulp `Up (-.(2. **. 52.))) = 1. -. (2. **. 52.)

let%test_module _ =
  (module struct
    (* check we raise on invalid input *)
    let must_fail f x = Exn.does_raise (fun () -> f x)

    let must_succeed f x =
      ignore (f x : _);
      true
    ;;

    let%test _ = must_fail int63_round_nearest_portable_alloc_exn nan
    let%test _ = must_fail int63_round_nearest_portable_alloc_exn max_value
    let%test _ = must_fail int63_round_nearest_portable_alloc_exn min_value
    let%test _ = must_fail int63_round_nearest_portable_alloc_exn (2. **. 63.)
    let%test _ = must_fail int63_round_nearest_portable_alloc_exn ~-.(2. **. 63.)

    let%test _ =
      must_succeed int63_round_nearest_portable_alloc_exn ((2. **. 62.) -. 512.)
    ;;

    let%test _ = must_fail int63_round_nearest_portable_alloc_exn (2. **. 62.)

    let%test _ =
      must_fail int63_round_nearest_portable_alloc_exn (~-.(2. **. 62.) -. 1024.)
    ;;

    let%test _ = must_succeed int63_round_nearest_portable_alloc_exn ~-.(2. **. 62.)
  end)
;;

let%test _ = round_nearest 3.6 = 4. && round_nearest (-3.6) = -4.


(* The redefinition of [sexp_of_t] in float.ml assumes sexp conversion uses E rather than
   e. *)
let%test_unit "e vs E" =
  [%test_result: Sexp.t] [%sexp (1.4e100 : t)] ~expect:(Atom "1.4E+100")
;;

let%test_module _ =
  (module struct
    let test ?delimiter ~decimals f s s_strip_zero =
      let s' = to_string_hum ?delimiter ~decimals ~strip_zero:false f in
      if String.(s' <> s)
      then
        raise_s
          [%message
            "to_string_hum ~strip_zero:false"
              ~input:(f : float)
              (decimals : int)
              ~got:(s' : string)
              ~expected:(s : string)];
      let s_strip_zero' = to_string_hum ?delimiter ~decimals ~strip_zero:true f in
      if String.(s_strip_zero' <> s_strip_zero)
      then
        raise_s
          [%message
            "to_string_hum ~strip_zero:true"
              ~input:(f : float)
              (decimals : int)
              ~got:(s_strip_zero : string)
              ~expected:(s_strip_zero' : string)]
    ;;

    let%test_unit _ = test ~decimals:3 0.99999 "1.000" "1"
    let%test_unit _ = test ~decimals:3 0.00001 "0.000" "0"
    let%test_unit _ = test ~decimals:3 ~-.12345.1 "-12_345.100" "-12_345.1"

    let%test_unit _ =
      test ~delimiter:',' ~decimals:3 ~-.12345.1 "-12,345.100" "-12,345.1"
    ;;

    let%test_unit _ = test ~decimals:0 0.99999 "1" "1"
    let%test_unit _ = test ~decimals:0 0.00001 "0" "0"
    let%test_unit _ = test ~decimals:0 ~-.12345.1 "-12_345" "-12_345"
    let%test_unit _ = test ~decimals:0 (5.0 /. 0.0) "inf" "inf"
    let%test_unit _ = test ~decimals:0 (-5.0 /. 0.0) "-inf" "-inf"
    let%test_unit _ = test ~decimals:0 (0.0 /. 0.0) "nan" "nan"
    let%test_unit _ = test ~decimals:2 (5.0 /. 0.0) "inf" "inf"
    let%test_unit _ = test ~decimals:2 (-5.0 /. 0.0) "-inf" "-inf"
    let%test_unit _ = test ~decimals:2 (0.0 /. 0.0) "nan" "nan"
    let%test_unit _ = test ~decimals:5 (10_000.0 /. 3.0) "3_333.33333" "3_333.33333"
    let%test_unit _ = test ~decimals:2 ~-.0.00001 "-0.00" "-0"

    let rand_test n =
      let go () =
        let f = Random.float 1_000_000.0 -. 500_000.0 in
        let repeatable to_str =
          let s = to_str f in
          if String.( <> )
               (String.split s ~on:',' |> String.concat |> of_string |> to_str)
               s
          then raise_s [%message "failed" (f : t)]
        in
        repeatable (to_string_hum ~decimals:3 ~strip_zero:false)
      in
      try
        for _ = 0 to Int.( - ) n 1 do
          go ()
        done;
        true
      with
      | e ->
        eprintf "%s\n%!" (Exn.to_string e);
        false
    ;;

    let%test _ = rand_test 10_000
  end)
;;

let%test_module "Hexadecimal syntax" =
  (module struct
    let should_fail str = Exn.does_raise (fun () -> Caml.float_of_string str)
    let test_equal str g = Caml.float_of_string str = g

    let%test _ = should_fail "0x"
    let%test _ = should_fail "0x.p0"
    let%test _ = test_equal "0x0" 0.
    let%test _ = test_equal "0x1.b7p-1" 0.857421875
    let%test _ = test_equal "0x1.999999999999ap-4" 0.1
  end)
;;

let%expect_test "square" =
  printf "%f\n" (square 1.5);
  printf "%f\n" (square (-2.5));
  [%expect {|
    2.250000
    6.250000 |}]
;;

let%expect_test "mathematical constants" =
  (* Compare to the from-string conversion of numbers from Wolfram Alpha *)
  let eq x s = assert (x = of_string s) in
  eq pi "3.141592653589793238462643383279502884197169399375105820974";
  eq sqrt_pi "1.772453850905516027298167483341145182797549456122387128213";
  eq sqrt_2pi "2.506628274631000502415765284811045253006986740609938316629";
  eq euler "0.577215664901532860606512090082402431042159335939923598805";
  (* Check size of diff from  ordinary computation. *)
  printf "sqrt pi diff  : %.20f\n" (sqrt_pi - sqrt pi);
  printf "sqrt 2pi diff : %.20f\n" (sqrt_2pi - sqrt (2. * pi));
  [%expect
    {|
    sqrt pi diff  : 0.00000000000000022204
    sqrt 2pi diff : 0.00000000000000044409 |}]
;;

let%test _ = not (is_negative Float.nan)
let%test _ = not (is_non_positive Float.nan)
let%test _ = is_non_negative (-0.)

let%expect_test "iround_nearest_exn noalloc" =
  let t = Sys.opaque_identity 205.414 in
  Expect_test_helpers_core.require_no_allocation [%here] (fun () -> iround_nearest_exn t)
  |> printf "%d\n";
  [%expect {| 205 |}]
;;

let%test_unit "int to float conversion consistency" =
  let test_int63 x =
    [%test_result: float] (Float.of_int63 x) ~expect:(Float.of_int64 (Int63.to_int64 x))
  in
  let test_int x =
    [%test_result: float] (Float.of_int x) ~expect:(Float.of_int63 (Int63.of_int x));
    test_int63 (Int63.of_int x)
  in
  test_int 0;
  test_int 35;
  test_int (-1);
  test_int Int.max_value;
  test_int Int.min_value;
  test_int63 Int63.zero;
  test_int63 Int63.min_value;
  test_int63 Int63.max_value;
  let rand =
    Random.State.make [| Hashtbl.hash "int to float conversion consistency" |]
  in
  for _i = 0 to 100 do
    let x = Random.State.int rand Int.max_value in
    test_int x
  done;
  ()
;;

let%expect_test "min and max" =
  let nan = Float.nan in
  let inf = Float.infinity in
  let ninf = Float.neg_infinity in
  List.iter
    [ 0.1, 0.3; 71., -7.; nan, 0.3; nan, ninf; nan, inf; nan, nan; ninf, inf; 0., -0. ]
    ~f:(fun (a, b) ->
      printf
        "%5g%5g%5g%5g%5g%5g\n"
        a
        b
        (Float.min a b)
        (Float.min b a)
        (Float.max a b)
        (Float.max b a));
  [%expect
    {|
     0.1  0.3  0.1  0.1  0.3  0.3
      71   -7   -7   -7   71   71
     nan  0.3  nan  nan  nan  nan
     nan -inf  nan  nan  nan  nan
     nan  inf  nan  nan  nan  nan
     nan  nan  nan  nan  nan  nan
    -inf  inf -inf -inf  inf  inf
       0   -0   -0    0   -0    0
    |}]
;;
