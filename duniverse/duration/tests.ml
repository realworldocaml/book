open Duration

let test_f f s factor upper upperv () =
  List.iter (fun v ->
      Alcotest.(check int64 (s ^ " works" ^ string_of_int v)
                  Int64.(mul (of_int v) factor)
                  (f v)))
    [ 0 ; 1 ; 100 ] ;
  Alcotest.(check_raises (s ^ " raises") (Invalid_argument "negative")
              (fun () -> ignore (f (-1)))) ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upperv (f upper)) ;
  Alcotest.(check_raises ("upper bound + 1 " ^ s ^ " raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (f (succ upper))))

let test_g g s factor upper upperv () =
  List.iter (fun (e, v) ->
      Alcotest.(check int (s ^ " works " ^ Int64.to_string v)
                  e (g v)))
    [ 0, 0L ; 1, factor ] ;
  Alcotest.(check int ("upper bound " ^ s ^ " good")
              upper (g upperv))

let test_inv f g s upper () =
  Alcotest.(check int ("inverse 0 " ^ s) 0 (g (f 0))) ;
  Alcotest.(check int ("inverse 1 " ^ s) 1 (g (f 1))) ;
  Alcotest.(check int ("inverse 10 " ^ s) 10 (g (f 10))) ;
  Alcotest.(check int "inverse upper" upper (g (f upper)))

let test_one f g s fa m mv = [
  "of" ^ s ^ " is good", `Quick, test_f f ("of" ^ s) fa m mv ;
  "to" ^ s ^ " is good", `Quick, test_g g ("to" ^ s) fa m mv ;
  "inverse of/to" ^ s, `Quick, test_inv f g s m
]

let test_f_64 f s factor upper upperv () =
  List.iter (fun v ->
      Alcotest.(check int64 (s ^ " works" ^ Int64.to_string v)
                  Int64.(mul v factor)
                  (f v)))
    [ 0L ; 1L ; 100L ] ;
  Alcotest.(check_raises (s ^ " raises") (Invalid_argument "negative")
              (fun () -> ignore (f (-1L)))) ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upperv (f upper)) ;
  Alcotest.(check_raises ("upper bound + 1 " ^ s ^ " raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (f (Int64.succ upper))))

let test_g_64 g s factor upper upperv () =
  List.iter (fun (e, v) ->
      Alcotest.(check int64 (s ^ " works " ^ Int64.to_string v)
                  e (g v)))
    [ 0L, 0L ; 1L, factor ] ;
  Alcotest.(check int64 ("upper bound " ^ s ^ " good")
              upper (g upperv))

let test_inv_64 f g s upper () =
  Alcotest.(check int64 ("inverse 0 " ^ s) 0L (g (f 0L))) ;
  Alcotest.(check int64 ("inverse 1 " ^ s) 1L (g (f 1L))) ;
  Alcotest.(check int64 ("inverse 10 " ^ s) 10L (g (f 10L))) ;
  Alcotest.(check int64 "inverse upper" upper (g (f upper)))

let test_one_64 f g s fa m mv = [
  "of" ^ s ^ " is good", `Quick, test_f_64 f ("of" ^ s) fa m mv ;
  "to" ^ s ^ " is good", `Quick, test_g_64 g ("to" ^ s) fa m mv ;
  "inverse of/to" ^ s, `Quick, test_inv_64 f g s m
]

let test_of_us =
  let a, b = 18446744073709549L, 0xFFFFFFFFFFFFF5C8L in
  test_one_64 of_us_64 to_us_64 "_us" 1_000L a b @
  if Sys.word_size = 64 then
    test_one of_us to_us "_us" 1_000L (Int64.to_int a) b
  else
    []

let test_of_ms =
  let a, b = 18446744073709L, 0xFFFFFFFFFFF79540L in
  test_one_64 of_ms_64 to_ms_64 "_ms" 1_000_000L a b @
  if Sys.word_size = 64 then
    test_one of_ms to_ms "_ms" 1_000_000L (Int64.to_int a) b
  else
    []

let test_of_sec =
  let a, b = 18446744073L, 0xFFFFFFFFD5B51A00L in
  test_one_64 of_sec_64 to_sec_64 "_sec" 1_000_000_000L a b @
  if Sys.word_size = 64 then
    test_one of_sec to_sec "_sec" 1_000_000_000L (Int64.to_int a) b
  else
    []

let test_of_min =
  test_one of_min to_min "_min" 60_000_000_000L 307445734 0xFFFFFFF826C11000L

let test_of_hour =
  test_one of_hour to_hour "_hour" 3600_000_000_000L 5124095 0xFFFFFE1D2D476000L

let test_of_day =
  let d_in_ns = Int64.mul 24L 3600_000_000_000L in
  test_one of_day to_day "_day" d_in_ns 213503 0xFFFFB2CECCB10000L

let test_of_year =
  let y_in_ns = Int64.mul 8766L 3600_000_000_000L in
  test_one of_year to_year "_year" y_in_ns 584 0xFFC33A7AFAE60000L


let test_of_f () =
  List.iter (fun v ->
      Alcotest.(check int64 ("of_f works " ^ string_of_float v)
                  (Int64.of_float (v *. 1_000_000_000.))
                  (of_f v)))
    [ 0. ; 1. ; 2. ; 0.000000001 ] ;
  Alcotest.(check_raises ("of_f raises") (Invalid_argument "negative")
              (fun () -> ignore (of_f (-1.)))) ;
  Alcotest.(check int64 ("upper bound of_f good")
              0xFFFFFFFFFFFFF596L (of_f 18446744073.709549)) ;
  Alcotest.(check_raises ("upper bound + 1 of_f raises")
              (Invalid_argument "out of range")
              (fun () -> ignore (of_f 18446744073.709551)))

let test_to_f () =
  List.iter (fun (e, v) ->
      Alcotest.(check (float 0.) ("to_f works " ^ Int64.to_string v)
                  e (to_f v)))
    [ 0., 0L ; 1., 1000000000L ; 2., 2000000000L ; 0.000000001, 1L ] ;
  Alcotest.(check (float 0.) ("upper bound to_f good")
              18446744073.709549 (to_f 0xFFFFFFFFFFFFF596L))

let test_inv_f () =
  Alcotest.(check (float 0.) "inverse 0 to/of_f" 0. (to_f (of_f 0.))) ;
  Alcotest.(check (float 0.) "inverse 1 to/of_f" 1. (to_f (of_f 1.))) ;
  Alcotest.(check (float 0.) "inverse 10 to/of_f" 10. (to_f (of_f 10.))) ;
  Alcotest.(check (float 0.) "inverse 3.5 to/of_f" 3.5 (to_f (of_f 3.5))) ;
  Alcotest.(check (float 0.) "inverse upper" 18446744073.709549 (to_f (of_f 18446744073.709549)))

let test_float = [
  "of_f is good", `Quick, test_of_f ;
  "to_f is good", `Quick, test_to_f ;
  "inverse of/to_f", `Quick, test_inv_f
]

let dur_tests =
  List.flatten [
    test_of_us ;
    test_of_ms ;
    test_of_sec ;
    test_of_min ;
    test_of_hour ;
    test_of_day ;
    test_of_year ;
    test_float
  ]


let tests = [
  "Duration", dur_tests
]

let () = Alcotest.run "Duration tests" tests
