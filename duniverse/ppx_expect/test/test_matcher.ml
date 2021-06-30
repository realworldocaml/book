open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv
open Expect_test_common
open Expect_test_matcher

(* [matcher/lexer.mll] checks for escaped newlines. *)
let%test_unit _ =
  [%test_result: string] (Scanf.unescaped "xx\\n\032yy") ~expect:"xx\n yy"
;;

let%test_module "Choose_tag" =
  (module struct
    open Choose_tag

    let test body = [%test_result: string] (choose ~default:"" body)

    let%test_unit _ = test "nice text" ~expect:""
    let%test_unit _ = test "with embedded |} somewhere" ~expect:"xxx"
    let%test_unit _ = test "with embedded |a} somewhere" ~expect:""
    let%test_unit _ = test "with embedded |xxx} somewhere" ~expect:""
    let%test_unit _ = test "double - |} and |xxx} - embedding" ~expect:"xxxx"

    let testD body = [%test_result: string] (choose ~default:"default" body)

    let%test_unit _ = testD "nice text" ~expect:"default"
    let%test_unit _ = testD "with embedded |} somewhere" ~expect:"default"
    let%test_unit _ = testD "with embedded |default} somewhere" ~expect:"default_xxx"
    let%test_unit _ = testD "double - |default} and |default_xxx}" ~expect:"default_xxxx"
  end)
;;

let%test_module "Reconcile" =
  (module struct
    open Reconcile
    open Private

    let%test _ = line_matches ~expect:(Literal "foo") ~actual:"foo"
    let%test _ = line_matches ~expect:(Literal "f.*o (regexp)") ~actual:"f.*o (regexp)"
    let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"
    let%test _ = not (line_matches ~expect:(Regexp "f.*o") ~actual:"foo (regexp)")
    let%test _ = not (line_matches ~expect:(Regexp "[a]") ~actual:"[a]")
    let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"
    (* Regexp provides the possibility to match trailing *)
    let%test _ = line_matches ~expect:(Regexp "f.*o[ ]") ~actual:"foo "

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

        let%test_unit _ =
          expect_correction ~expect:"[a] (regexp)" ~actual:"b" ~corrected:"b"
        ;;
      end)
    ;;

    let%test_module _ =
      (module struct
        let allow_output_patterns = true
        let strip s = Lexer.strip_surrounding_whitespaces s

        let nb orig trailing_blanks =
          Cst.Line.Not_blank { orig; trailing_blanks; data = () }
        ;;

        let%test_unit _ =
          [%test_result: unit Cst.t] (strip "\n  ") ~expect:(Empty "\n  ")
        ;;

        let%test_unit _ =
          [%test_result: unit Cst.t]
            (strip
               "   \n   foo   \n     bar     \n     plop  \n  \n    blah \n \n   ")
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
  end)
;;
