open! Core
open Expect_test_helpers_base
open Patdiff_kernel

let patdiff ?location_style ?context ?keep_ws string1 string2 =
  Patdiff_core.Without_unix.patdiff (* turns off ANSI codes for color *)
    ?location_style
    ~output:
      Ascii (* without color, cannot produce the "!|" lines that mix add/keep/remove *)
    ~produce_unified_lines:
      false (* line splitting produces confusing output in ASCII format *)
    ~split_long_lines:false
    ?context
    ?keep_ws
    ~prev:{ name = "a"; text = string1 }
    ~next:{ name = "b"; text = string2 }
    ()
;;

let patdiff_s ?location_style ?context ?keep_ws sexp1 sexp2 =
  patdiff ?location_style ?context ?keep_ws (sexp_to_string sexp1) (sexp_to_string sexp2)
;;

let print_endline_if_non_empty = function
  | "" -> ()
  | string -> print_endline string
;;

let print_patdiff ?location_style ?context ?keep_ws string1 string2 =
  print_endline_if_non_empty (patdiff ?location_style ?context ?keep_ws string1 string2)
;;

let print_patdiff_s ?location_style ?context ?keep_ws sexp1 sexp2 =
  print_endline_if_non_empty (patdiff_s ?location_style ?context ?keep_ws sexp1 sexp2)
;;

let diff_printer ?location_style ?context ?keep_ws initial =
  let print =
    let previous = ref None in
    fun current ->
      (match !previous with
       | None -> print_endline current
       | Some previous -> print_patdiff ?location_style ?context ?keep_ws previous current);
      previous := Some current
  in
  let () = Option.iter initial ~f:print in
  stage print
;;

let diff_printer_s ?location_style ?context ?keep_ws initial =
  let diff_printer =
    unstage
      (diff_printer
         ?location_style
         ?context
         ?keep_ws
         (Option.map ~f:sexp_to_string initial))
  in
  stage (fun sexp -> diff_printer (sexp_to_string sexp))
;;
