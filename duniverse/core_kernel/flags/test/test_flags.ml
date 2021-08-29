open! Core_kernel
open! Expect_test_helpers_core
open! Flags

let a = Int63.of_int 0x1
let b = Int63.of_int 0x2
let c = Int63.of_int 0xC
let d = create ~bit:62

let%expect_test _ =
  List.iter [ -1; 63 ] ~f:(fun bit ->
    show_raise ~hide_positions:true (fun () -> ignore (create ~bit : Int63.t)));
  [%expect
    {|
    (raised (
      "Flags.create got invalid ~bit (must be between 0 and 62)"
      -1
      lib/flags/src/flags.ml:LINE:COL))
    (raised (
      "Flags.create got invalid ~bit (must be between 0 and 62)"
      63
      lib/flags/src/flags.ml:LINE:COL)) |}]
;;

let%expect_test _ =
  let print_hex bit =
    print_s [%sexp (create ~bit |> Int63.Hex.to_string_hum : string)]
  in
  print_hex 0;
  [%expect {|
    0x1 |}];
  print_hex 1;
  [%expect {|
    0x2 |}];
  print_hex 62;
  [%expect {|
    -0x4000_0000_0000_0000 |}]
;;

module M = Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let known = [ a, "a"; b, "b"; c, "c" ]
    let remove_zero_flags = false
  end)

include M

let%expect_test _ =
  print_and_check_comparable_sexps [%here] (module M) [ a; b; c ];
  [%expect
    {|
    (Set (
      (a)
      (b)
      (c)))
    (Map (
      ((a) 0)
      ((b) 1)
      ((c) 2))) |}]
;;

(* [sexp_of_t] *)
let print_sexp_of t = print_s [%sexp (t : t)]

let%expect_test _ =
  print_sexp_of empty;
  [%expect {|
    () |}]
;;

let%expect_test _ =
  print_sexp_of a;
  [%expect {|
    (a) |}]
;;

let%expect_test _ =
  print_sexp_of c;
  [%expect {|
    (c) |}]
;;

let%expect_test _ =
  print_sexp_of (a + b);
  [%expect {|
    (a b) |}]
;;

let%expect_test _ =
  print_sexp_of (Int63.of_int 0x10);
  [%expect {|
    (() (unrecognized_bits 0x10)) |}]
;;

(* [t_of_sexp] *)
let check_t_of_sexp here expect string =
  require here (equal expect (t_of_sexp (Sexp.of_string string)))
;;

let%expect_test _ = check_t_of_sexp [%here] empty "()"
let%expect_test _ = check_t_of_sexp [%here] a "(a)"
let%expect_test _ = check_t_of_sexp [%here] c "(c)"
let%expect_test _ = check_t_of_sexp [%here] (b + c) "(b c)"
let%expect_test _ = check_t_of_sexp [%here] (b + c) "(c b)"

let%expect_test _ =
  List.iter [ "a"; "(())"; "(a ())"; "(d)" ] ~f:(fun string ->
    show_raise (fun () ->
      print_s [%message "" ~input:string];
      ignore (t_of_sexp (Sexp.of_string string) : t)));
  [%expect
    {|
    (input a)
    (raised (Of_sexp_error "list_of_sexp: list needed" (invalid_sexp a)))
    (input "(())")
    (raised (Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp ())))
    (input "(a ())")
    (raised (Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp ())))
    (input "(d)")
    (raised (
      Of_sexp_error "Flags.t_of_sexp got unknown name: d" (invalid_sexp (d)))) |}]
;;

(* +, - *)
let%expect_test _ = require [%here] (equal (a + a) a)
let%expect_test _ = require [%here] (equal (a + b) (b + a))
let%expect_test _ = require [%here] (equal (a - a) empty)
let%expect_test _ = require [%here] (equal (a + b - a) b)
(* [intersect] *)
let%expect_test _ = require [%here] (equal (intersect a a) a)
let%expect_test _ = require [%here] (equal (intersect a b) empty)
let%expect_test _ = require [%here] (equal (intersect (a + b) a) a)
(* [complement] *)
let%expect_test _ = require [%here] (equal (intersect (complement a) b) b)
(* [do_intersect] *)
let%expect_test _ = require [%here] (do_intersect a a)
let%expect_test _ = require [%here] (not (do_intersect a b))
let%expect_test _ = require [%here] (do_intersect (a + b) a)
let%expect_test _ = require [%here] (do_intersect (a + b) b)
let%expect_test _ = require [%here] (not (do_intersect (a + b) c))
(* [are_disjoint] *)
let%expect_test _ = require [%here] (are_disjoint a empty)
let%expect_test _ = require [%here] (not (are_disjoint a a))
let%expect_test _ = require [%here] (are_disjoint a b)
let%expect_test _ = require [%here] (are_disjoint b a)
let%expect_test _ = require [%here] (not (are_disjoint (a + b) a))
let%expect_test _ = require [%here] (are_disjoint (a + b) c)
(* compare *)
let%expect_test _ = require [%here] (Int.( = ) (Int.compare 0 1) (-1))

let print_compare t1 t2 = print_s [%sexp (compare t1 t2 : int)]

let%expect_test _ =
  print_compare a empty;
  [%expect {|
    1 |}];
  print_compare c empty;
  [%expect {|
    1 |}];
  print_compare a a;
  [%expect {|
    0 |}];
  print_compare c c;
  [%expect {|
    0 |}];
  print_compare empty empty;
  [%expect {|
    0 |}];
  print_compare empty a;
  [%expect {|
    -1 |}];
  print_compare empty c;
  [%expect {|
    -1 |}];
  print_compare (a + c) a;
  [%expect {|
    1 |}];
  print_compare (a + c) c;
  [%expect {|
    1 |}];
  print_compare (b + b) b;
  [%expect {|
    0 |}];
  print_compare b (b + c);
  [%expect {|
    -1 |}]
;;

let%expect_test "[compare] is a total order consistent with [is_subset]" =
  print_compare a b;
  [%expect {|
    -1 |}];
  print_compare b (b + d);
  [%expect {|
    -1 |}];
  print_compare a (b + d);
  [%expect {|
    -1 |}];
  let ordered =
    [| empty
     ; a
     ; b
     ; a + b
     ; c
     ; a + c
     ; b + c
     ; a + b + c
     ; complement d
     ; d
     ; a + d
     ; b + d
     ; complement (a + b)
     ; complement b
     ; complement a
     ; complement empty
    |]
  in
  Array.iteri ordered ~f:(fun i ai ->
    Array.iteri ordered ~f:(fun j aj ->
      require [%here] (Bool.equal (equal ai aj) (Int.equal (compare ai aj) 0));
      if is_subset ai ~of_:aj then require [%here] (Int.( <= ) (compare ai aj) 0);
      require [%here] (Int.equal (compare ai aj) (Int.compare i j))))
;;

(* Check that conflicting flags leads to an error. *)
let%test _ =
  Result.is_error
    (Result.try_with (fun () ->
       let module M =
         Make (struct
           let allow_intersecting = false
           let should_print_error = false
           let known = [ Int63.of_int 0x1, ""; Int63.of_int 0x1, "" ]
           let remove_zero_flags = false
         end)
       in
       ()))
;;
