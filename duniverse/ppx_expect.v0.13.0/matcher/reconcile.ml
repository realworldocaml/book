open Expect_test_common.Std
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

module Result = struct
  (* Either match with an explicit success, or (lazily) produce a correction. *)
  type 'a t =
    | Match
    | Correction of 'a
  [@@deriving sexp_of, compare]

  let map t ~f =
    match t with
    | Match -> Match
    | Correction x -> Correction (f x)
  ;;

  let value t ~success =
    match t with
    | Match -> success
    | Correction f -> f
  ;;
end

let matches_regexp ~(pat : Re.t) s =
  Re.execp (Re.compile (Re.whole_string pat)) s
;;

let glob = Re.Glob.glob ~anchored:true ~pathname:false ~expand_braces:true

let line_matches ~(expect : Fmt.t) ~actual =
  match expect with
  | Literal expect -> expect = actual
  | Glob expect -> matches_regexp ~pat:(glob expect) actual
  | Regexp expect -> matches_regexp ~pat:(Re.Emacs.re expect) actual
;;

let%test _ = line_matches ~expect:(Literal "foo") ~actual:"foo"
let%test _ = line_matches ~expect:(Literal "f.*o (regexp)") ~actual:"f.*o (regexp)"
let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"
let%test _ = not (line_matches ~expect:(Regexp "f.*o") ~actual:"foo (regexp)")
let%test _ = not (line_matches ~expect:(Regexp "[a]") ~actual:"[a]")
let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"
(* Regexp provides the possibility to match trailing *)
let%test _ = line_matches ~expect:(Regexp "f.*o[ ]") ~actual:"foo "

let literal_line ~allow_output_patterns actual : Fmt.t Cst.Line.t =
  match actual with
  | "" -> Blank ""
  | _ ->
    let line_matches_itself =
      (not allow_output_patterns)
      || line_matches
           ~expect:(Lexer.parse_pretty_line actual ~allow_output_patterns)
           ~actual
    in
    Not_blank
      { data = Literal actual
      ; orig = (if line_matches_itself then actual else actual ^ " (literal)")
      ; trailing_blanks = ""
      }
;;

let reconcile_line ~(expect : Fmt.t) ~actual ~allow_output_patterns
  : Fmt.t Cst.Line.t Result.t
  =
  assert (not (String.contains actual '\n'));
  if line_matches ~expect ~actual
  then Match
  else Correction (literal_line actual ~allow_output_patterns)
;;

let%test_module _ =
  (module struct
    let allow_output_patterns = true

    let expect_match ~expect ~actual =
      let expect = Lexer.parse_pretty_line expect ~allow_output_patterns in
      [%test_result: Fmt.t Cst.Line.t Result.t]
        (reconcile_line ~expect ~actual ~allow_output_patterns)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~corrected =
      let expect = Lexer.parse_pretty_line expect ~allow_output_patterns in
      let corrected : Fmt.t Cst.Line.t =
        Not_blank
          { orig = corrected
          ; data = Lexer.parse_pretty_line corrected ~allow_output_patterns
          ; trailing_blanks = ""
          }
      in
      [%test_result: Fmt.t Cst.Line.t Result.t]
        (reconcile_line ~expect ~actual ~allow_output_patterns)
        ~expect:(Correction corrected)
    ;;

    let%test_unit _ = expect_match ~expect:"foo" ~actual:"foo"
    let%test_unit _ = expect_match ~expect:"[a] (regexp)" ~actual:"a"
    let%test_unit _ = expect_correction ~expect:"[a] (regexp)" ~actual:"b" ~corrected:"b"
  end)
;;

let rec lines_match
          ~(expect_lines : Fmt.t Cst.Line.t list)
          ~(actual_lines : string list)
          ~allow_output_patterns
  : bool
  =
  match expect_lines, actual_lines with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | expect :: expect_lines, actual :: actual_lines ->
    let format = Cst.Line.data expect ~blank:(Literal "") in
    let line = reconcile_line ~expect:format ~actual ~allow_output_patterns in
    (match line with
     | Match -> lines_match ~expect_lines ~actual_lines ~allow_output_patterns
     | _ -> false)
;;

let rec corrected_rev
          acc
          ~(expect_lines : Fmt.t Cst.Line.t list)
          ~(actual_lines : string list)
          ~allow_output_patterns
  : Fmt.t Cst.Line.t list
  =
  match expect_lines, actual_lines with
  | [], [] -> acc
  | [], actual_lines ->
    ListLabels.fold_left actual_lines ~init:acc ~f:(fun acc x ->
      literal_line x ~allow_output_patterns :: acc)
  | _, [] -> acc
  | expect :: expect_lines, actual :: actual_lines ->
    let format = Cst.Line.data expect ~blank:(Literal "") in
    let line =
      reconcile_line ~expect:format ~actual ~allow_output_patterns
      |> Result.value ~success:expect
    in
    corrected_rev ~expect_lines ~actual_lines (line :: acc) ~allow_output_patterns
;;

let reconcile_lines ~expect_lines ~actual_lines ~allow_output_patterns
  : Fmt.t Cst.Line.t list Result.t
  =
  if lines_match ~expect_lines ~actual_lines ~allow_output_patterns
  then Match
  else
    Correction
      (List.rev (corrected_rev [] ~expect_lines ~actual_lines ~allow_output_patterns))
;;

let expectation_body_internal
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  : Fmt.t Cst.t Expectation.Body.t Result.t
  =
  match expect with
  | Exact expect -> if expect = actual then Match else Correction (Exact actual)
  | Output -> Match
  | Pretty expect ->
    let actual_lines =
      Lexer.strip_surrounding_whitespaces actual |> Cst.stripped_original_lines
    in
    let expect_lines = Cst.to_lines expect in
    (match reconcile_lines ~expect_lines ~actual_lines ~allow_output_patterns with
     | Match -> Match
     | Correction reconciled_lines ->
       let reconciled =
         Cst.reconcile
           expect
           ~lines:reconciled_lines
           ~default_indentation:default_indent
           ~pad_single_line
       in
       Correction (Pretty reconciled))
  | Unreachable ->
    let actual_lines =
      Lexer.strip_surrounding_whitespaces actual |> Cst.stripped_original_lines
    in
    (match reconcile_lines ~expect_lines:[] ~actual_lines ~allow_output_patterns with
     | Match -> Correction (Pretty (Empty ""))
     | Correction reconciled_lines ->
       let reconciled =
         Cst.reconcile
           (Empty "")
           ~lines:reconciled_lines
           ~default_indentation:default_indent
           ~pad_single_line
       in
       Correction (Pretty reconciled))
;;

let expectation_body
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  : Fmt.t Cst.t Expectation.Body.t Result.t
  =
  let res =
    expectation_body_internal
      ~expect
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  in
  match res with
  | Match -> Match
  | Correction c ->
    (match
       expectation_body_internal
         ~expect:c
         ~actual
         ~default_indent
         ~pad_single_line
         ~allow_output_patterns
     with
     | Match -> res
     | Correction _ -> assert false)
;;

let%test_module _ =
  (module struct
    let allow_output_patterns = true
    let strip s = Lexer.strip_surrounding_whitespaces s
    let nb orig trailing_blanks = Cst.Line.Not_blank { orig; trailing_blanks; data = () }

    let%test_unit _ = [%test_result: unit Cst.t] (strip "\n  ") ~expect:(Empty "\n  ")

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip "   \n   foo   \n     bar     \n     plop  \n  \n    blah \n \n   ")
        ~expect:
          (Multi_lines
             { leading_spaces = "   \n"
             ; trailing_spaces = "\n \n   "
             ; indentation = "   "
             ; lines =
                 [ nb "foo" "   "
                 ; nb "  bar" "     "
                 ; nb "  plop" "  "
                 ; Blank "  "
                 ; nb " blah" " "
                 ]
             })
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip "abc \ndef ")
        ~expect:
          (Multi_lines
             { leading_spaces = ""
             ; trailing_spaces = " "
             ; indentation = ""
             ; lines = [ nb "abc" " "; nb "def" "" ]
             })
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip " [a] (regexp) ")
        ~expect:
          (Single_line
             { leading_blanks = " "
             ; trailing_spaces = " "
             ; orig = "[a] (regexp)"
             ; data = ()
             })
    ;;

    let expect_match ~expect ~actual =
      let expect = Lexer.parse_body (Pretty expect) ~allow_output_patterns in
      [%test_result: Fmt.t Cst.t Expectation.Body.t Result.t]
        (expectation_body
           ~expect
           ~actual
           ~default_indent:0
           ~pad_single_line:true
           ~allow_output_patterns)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~default_indent ~corrected =
      let expect = Lexer.parse_body (Pretty expect) ~allow_output_patterns in
      [%test_result: Fmt.t Cst.t Expectation.Body.t Result.t]
        (expectation_body
           ~expect
           ~actual
           ~default_indent
           ~pad_single_line:true
           ~allow_output_patterns)
        ~expect:(Correction corrected)
    ;;

    let%test_unit _ = expect_match ~expect:" foo " ~actual:"foo"
    let%test_unit _ = expect_match ~expect:"foo\n[a] (regexp)" ~actual:"foo\na"

    let%test_unit _ =
      expect_correction
        ~expect:"foo\n[a] (regexp)"
        ~actual:"foo\nb"
        ~default_indent:0
        ~corrected:(Lexer.parse_body ~allow_output_patterns:true (Pretty "foo\nb"))
    ;;

    (* check regexp are preserved in corrections *)
    let%test_unit _ =
      expect_correction
        ~expect:"foo\n[ab]* (regexp)"
        ~actual:"not-foo\nbaba"
        ~default_indent:0
        ~corrected:
          (Lexer.parse_body ~allow_output_patterns (Pretty "not-foo\n[ab]* (regexp)"))
    ;;
  end)
;;
