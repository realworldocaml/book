open Import

let parse_and_print input =
  let x = Parsexp.Many_cst.parse_string_exn input in
  print_s [%sexp (x : Parsexp.Cst.t_or_comment list)]
;;

let%expect_test "escape sequence in block comment" =
  parse_and_print {|#| "\255" |#|};
  [%expect{|
    ((
      Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    12)
            (offset 12)))))
        (comment "#| \"\\255\" |#"))))
  |}]
;;

let%expect_test "quoted atom" =
  parse_and_print {| "foo bar" |};
  [%expect{|
    ((
      Sexp (
        Atom
        (loc (
          (start_pos (
            (line   1)
            (col    1)
            (offset 1)))
          (end_pos (
            (line   1)
            (col    10)
            (offset 10)))))
        (atom "foo bar")
        (unescaped ("\"foo bar\"")))))
  |}]
;;
