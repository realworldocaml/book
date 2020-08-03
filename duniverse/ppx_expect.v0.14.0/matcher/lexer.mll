{
  open Import
  open Expect_test_common
  open Ppx_sexp_conv_lib.Conv

  let escaped s =
    let unescaped = Scanf.unescaped s in
    (* [test/test_matcher.ml] tests the behavior of [Scanf.unescaped] on newlines. *)
    if String.contains unescaped '\n'
    then
      failwith "(escaped) strings can't contain escaped newlines";
    Fmt.Literal unescaped
}

let space = [' ' '\t']
let line_contents = [^' ' '\t' '\n']+ (space* [^' ' '\t' '\n']+)*
let lowercase = ['a'-'z' '_']

rule pretty_line = parse
  | space* line_contents as s space* "(escaped)" eof { escaped s  }
  | space* line_contents as s space* "(literal)" eof { Literal s  }
  | space* line_contents as s space* "(regexp)"  eof { Regexp  s  }
  | space* line_contents as s space* "(glob)"    eof { Glob    s  }
  | space* line_contents as s                    eof { Literal s  }
  | space*                                       eof { Literal "" }
  | _* as s { Printf.ksprintf invalid_arg "Lexer.pretty_line %S" s }

and pretty_line_no_output_patterns = parse
  | space* line_contents as s eof { Fmt.Literal s  }
  | space*                    eof { Literal "" }
  | _* as s { Printf.ksprintf invalid_arg
                "Lexer.pretty_line_no_output_patterns %S" s }

and leading_spaces = parse
  | (space* '\n')* as s { s }

and lines_with_identation acc = parse
  | space* as sp '\n'
    { let line = Cst.Line.Blank sp in
      lines_with_identation (line :: acc) lexbuf
    }
  | (space | '\n')* as tr eof
    { (List.rev acc,
       (* Add the newline that was consumed by the previous line. Since
          [lines_with_identation] is never called on blank strings, we know there is such
          a line. *)
       "\n" ^ tr) }
  | space* line_contents as s (space* as tr) eof
    { let line =
        Cst.Line.Not_blank
          { orig = s
          ; data = ()
          ; trailing_blanks = ""
          }
      in
      (List.rev (line :: acc), tr)
    }
  | space* line_contents as s (space* as tr) '\n'
    { let line =
        Cst.Line.Not_blank
          { orig = s
          ; data = ()
          ; trailing_blanks = tr
          }
      in
      lines_with_identation (line :: acc) lexbuf
    }

and strip_surrounding_whitespaces = parse
  | (space | '\n')* eof as s
    { Cst.Empty s }
  | (space* as leading) (line_contents as s) ((space | '\n')* as trailing) eof
    { Cst.Single_line
        { leading_blanks  = leading
        ; trailing_spaces = trailing
        ; orig            = s
        ; data            = ()
        }
    }
  | ""
    { let leading_spaces = leading_spaces lexbuf in
      let lines, trailing_spaces = lines_with_identation [] lexbuf in
      let indentation, lines = Cst.extract_indentation lines in
      Cst.Multi_lines
        { trailing_spaces
        ; leading_spaces
        ; indentation
        ; lines
        }
    }

and quoted_string_terminators acc = parse
  | "|" (lowercase* as s) "}" { quoted_string_terminators (s :: acc) lexbuf }
  | _                         { quoted_string_terminators       acc  lexbuf }
  | eof                       { acc }

{
  let strip_surrounding_whitespaces s =
    let lexbuf = Lexing.from_string s in
    let contents = strip_surrounding_whitespaces lexbuf in
    Cst.invariant ignore contents;
    contents

  let parse_pretty_line ~allow_output_patterns s =
    let lexbuf = Lexing.from_string s in
    if allow_output_patterns then
      pretty_line lexbuf
    else
      pretty_line_no_output_patterns lexbuf

  let parse_pretty ~allow_output_patterns s =
    let res =
      Cst.map (strip_surrounding_whitespaces s)
        ~f:(fun s () ->
          parse_pretty_line ~allow_output_patterns s)
    in
    (match Ppx_inline_test_lib.Runtime.testing with
    | `Testing `Am_test_runner ->
      let cst = Cst.to_string res in
      if not (String.equal cst s)
      then
        failwith (Printf.sprintf "ppx_expect internal error: expected: %S, got: %S" s cst)
    | `Testing `Am_child_of_test_runner | `Not_testing -> ());
    res

  let parse_body ~allow_output_patterns body =
    Expectation.Body.map_pretty body ~f:(parse_pretty ~allow_output_patterns)

  let extract_quoted_string_terminators s =
    quoted_string_terminators [] (Lexing.from_string s)
}
