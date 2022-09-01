open! Core
open! Import

let mark_prefix s ~len =
  let before = String.sub s ~pos:0 ~len in
  let after = String.sub s ~pos:len ~len:(String.length s - len) in
  [%string "%{before}│%{after}"]
;;

let show mode s ~len =
  let state, stack = Automaton.of_substring mode Sexp_with_positions s ~pos:0 ~len in
  let sexp_prefix = Sexp_prefix.create state stack in
  print_s [%sexp (sexp_prefix : Sexp_prefix.t option)]
;;

let test cr mode s =
  for len = 0 to String.length s do
    print_endline (mark_prefix s ~len);
    require_does_not_raise [%here] ~cr ~hide_positions:true (fun () -> show mode s ~len);
    print_endline ""
  done
;;

let%expect_test "[Many]" =
  test CR Many {|a #;() ("b\r\n\x61 c") #| ; |# d|};
  [%expect
    {|
    │a #;() ("b\r\n\x61 c") #| ; |# d
    ((() (Hole ())))

    a│ #;() ("b\r\n\x61 c") #| ; |# d
    ((
      ()
      (Hole ((
        (signified (Complete (prefix a)))
        (signifier_begin_offset 0)
        (signifier_end_offset   1))))))

    a │#;() ("b\r\n\x61 c") #| ; |# d
    (((a) (Hole ())))

    a #│;() ("b\r\n\x61 c") #| ; |# d
    ()

    a #;│() ("b\r\n\x61 c") #| ; |# d
    ()

    a #;(│) ("b\r\n\x61 c") #| ; |# d
    ()

    a #;()│ ("b\r\n\x61 c") #| ; |# d
    (((a) (Hole ())))

    a #;() │("b\r\n\x61 c") #| ; |# d
    (((a) (Hole ())))

    a #;() (│"b\r\n\x61 c") #| ; |# d
    (((a) (In_list (() (Hole ())))))

    a #;() ("│b\r\n\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "")))
          (signifier_begin_offset 8)
          (signifier_end_offset   9))))))))

    a #;() ("b│\r\n\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix b)))
          (signifier_begin_offset 8)
          (signifier_end_offset   10))))))))

    a #;() ("b\│r\n\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Incomplete (prefix_of_prefix b)))
          (signifier_begin_offset 8)
          (signifier_end_offset   11))))))))

    a #;() ("b\r│\n\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "b\r")))
          (signifier_begin_offset 8)
          (signifier_end_offset   12))))))))

    a #;() ("b\r\│n\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Incomplete (prefix_of_prefix "b\r")))
          (signifier_begin_offset 8)
          (signifier_end_offset   13))))))))

    a #;() ("b\r\n│\x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "b\r\n")))
          (signifier_begin_offset 8)
          (signifier_end_offset   14))))))))

    a #;() ("b\r\n\│x61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Incomplete (prefix_of_prefix "b\r\n")))
          (signifier_begin_offset 8)
          (signifier_end_offset   15))))))))

    a #;() ("b\r\n\x│61 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Incomplete (prefix_of_prefix "b\r\n")))
          (signifier_begin_offset 8)
          (signifier_end_offset   16))))))))

    a #;() ("b\r\n\x6│1 c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Incomplete (prefix_of_prefix "b\r\n")))
          (signifier_begin_offset 8)
          (signifier_end_offset   17))))))))

    a #;() ("b\r\n\x61│ c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "b\r\na")))
          (signifier_begin_offset 8)
          (signifier_end_offset   18))))))))

    a #;() ("b\r\n\x61 │c") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "b\r\na ")))
          (signifier_begin_offset 8)
          (signifier_end_offset   19))))))))

    a #;() ("b\r\n\x61 c│") #| ; |# d
    ((
      (a)
      (In_list (
        ()
        (Hole ((
          (signified (Complete (prefix "b\r\na c")))
          (signifier_begin_offset 8)
          (signifier_end_offset   20))))))))

    a #;() ("b\r\n\x61 c"│) #| ; |# d
    (((a) (In_list (("b\r\na c") (Hole ())))))

    a #;() ("b\r\n\x61 c")│ #| ; |# d
    (((a ("b\r\na c")) (Hole ())))

    a #;() ("b\r\n\x61 c") │#| ; |# d
    (((a ("b\r\na c")) (Hole ())))

    a #;() ("b\r\n\x61 c") #│| ; |# d
    ()

    a #;() ("b\r\n\x61 c") #|│ ; |# d
    ()

    a #;() ("b\r\n\x61 c") #| │; |# d
    ()

    a #;() ("b\r\n\x61 c") #| ;│ |# d
    ()

    a #;() ("b\r\n\x61 c") #| ; │|# d
    ()

    a #;() ("b\r\n\x61 c") #| ; |│# d
    ()

    a #;() ("b\r\n\x61 c") #| ; |#│ d
    (((a ("b\r\na c")) (Hole ())))

    a #;() ("b\r\n\x61 c") #| ; |# │d
    (((a ("b\r\na c")) (Hole ())))

    a #;() ("b\r\n\x61 c") #| ; |# d│
    ((
      (a ("b\r\na c"))
      (Hole ((
        (signified (Complete (prefix d)))
        (signifier_begin_offset 31)
        (signifier_end_offset   32)))))) |}]
;;

let%expect_test "deep nesting" =
  test CR Many "((((a))))";
  [%expect
    {|
    │((((a))))
    ((() (Hole ())))

    (│(((a))))
    ((() (In_list (() (Hole ())))))

    ((│((a))))
    ((() (In_list (() (In_list (() (Hole ())))))))

    (((│(a))))
    ((() (In_list (() (In_list (() (In_list (() (Hole ())))))))))

    ((((│a))))
    ((() (In_list (() (In_list (() (In_list (() (In_list (() (Hole ())))))))))))

    ((((a│))))
    ((
      ()
      (In_list (
        ()
        (In_list (
          ()
          (In_list (
            ()
            (In_list (
              ()
              (Hole ((
                (signified (Complete (prefix a)))
                (signifier_begin_offset 4)
                (signifier_end_offset   5))))))))))))))

    ((((a)│)))
    ((() (In_list (() (In_list (() (In_list (((a)) (Hole ())))))))))

    ((((a))│))
    ((() (In_list (() (In_list ((((a))) (Hole ())))))))

    ((((a)))│)
    ((() (In_list (((((a)))) (Hole ())))))

    ((((a))))│
    (((((((a))))) (Hole ()))) |}]
;;

let%expect_test "only [Many] mode" =
  test Comment Single "";
  [%expect
    {|
    │
    (* require-failed: lib/parsexp/prefix/test/test_sexp_prefix_examples.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure "The automaton must be in [Many] mode.")) |}];
  test Comment (Eager { got_sexp = (fun _ stack -> stack); no_sexp_is_error = false }) "";
  [%expect
    {|
    │
    (* require-failed: lib/parsexp/prefix/test/test_sexp_prefix_examples.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure "The automaton must be in [Many] mode.")) |}];
  ignore ()
;;
