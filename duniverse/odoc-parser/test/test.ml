open Odoc_parser

type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

module Location_to_sexp = struct
  let point : Loc.point -> sexp =
   fun { line; column } ->
    List [ Atom (string_of_int line); Atom (string_of_int column) ]

  let span : Loc.span -> sexp =
   fun { file; start; end_ } -> List [ Atom file; point start; point end_ ]

  let at : ('a -> sexp) -> 'a Loc.with_location -> sexp =
   fun f { location; value } -> List [ span location; f value ]
end

module Ast_to_sexp = struct
  (* let at = Location_to_sexp.at *)
  type at = { at : 'a. ('a -> sexp) -> 'a Loc.with_location -> sexp }

  let loc_at = { at = Location_to_sexp.at }

  let str s = Atom s

  let opt f s = match s with Some s -> List [ f s ] | None -> List []

  let style : Ast.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let reference_kind : Ast.reference_kind -> sexp = function
    | `Simple -> Atom "simple"
    | `With_text -> Atom "with_text"

  let rec inline_element at : Ast.inline_element -> sexp = function
    | `Space _ -> Atom "space"
    | `Word w -> List [ Atom "word"; Atom w ]
    | `Code_span c -> List [ Atom "code_span"; Atom c ]
    | `Raw_markup (target, s) ->
        List [ Atom "raw_markup"; opt str target; Atom s ]
    | `Styled (s, es) ->
        List [ style s; List (List.map (at.at (inline_element at)) es) ]
    | `Reference (kind, r, es) ->
        List
          [
            reference_kind kind;
            at.at str r;
            List (List.map (at.at (inline_element at)) es);
          ]
    | `Link (u, es) ->
        List [ str u; List (List.map (at.at (inline_element at)) es) ]

  let rec nestable_block_element at : Ast.nestable_block_element -> sexp =
    function
    | `Paragraph es ->
        List
          [ Atom "paragraph"; List (List.map (at.at (inline_element at)) es) ]
    | `Code_block (None, c) -> List [ Atom "code_block"; at.at str c ]
    | `Code_block (Some m, c) ->
        List [ Atom "code_block"; at.at str m; at.at str c ]
    | `Verbatim t -> List [ Atom "verbatim"; Atom t ]
    | `Modules ps -> List [ Atom "modules"; List (List.map (at.at str) ps) ]
    | `List (kind, weight, items) ->
        let kind =
          match kind with `Unordered -> "unordered" | `Ordered -> "ordered"
        in
        let weight =
          match weight with `Light -> "light" | `Heavy -> "heavy"
        in
        let items =
          items
          |> List.map (fun item ->
                 List (List.map (at.at (nestable_block_element at)) item))
          |> fun items -> List items
        in
        List [ Atom kind; Atom weight; items ]

  let tag at : Ast.tag -> sexp = function
    | `Author s -> List [ Atom "@author"; Atom s ]
    | `Deprecated es ->
        List
          (Atom "@deprecated" :: List.map (at.at (nestable_block_element at)) es)
    | `Param (s, es) ->
        List
          ([ Atom "@param"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Raise (s, es) ->
        List
          ([ Atom "@raise"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Return es ->
        List (Atom "@return" :: List.map (at.at (nestable_block_element at)) es)
    | `See (kind, s, es) ->
        let kind =
          match kind with
          | `Url -> "url"
          | `File -> "file"
          | `Document -> "document"
        in
        List
          ([ Atom "@see"; Atom kind; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Since s -> List [ Atom "@since"; Atom s ]
    | `Before (s, es) ->
        List
          ([ Atom "@before"; Atom s ]
          @ List.map (at.at (nestable_block_element at)) es)
    | `Version s -> List [ Atom "@version"; Atom s ]
    | `Canonical p -> List [ Atom "@canonical"; at.at str p ]
    | `Inline -> Atom "@inline"
    | `Open -> Atom "@open"
    | `Closed -> Atom "@closed"

  let block_element at : Ast.block_element -> sexp = function
    | #Ast.nestable_block_element as e -> nestable_block_element at e
    | `Heading (level, label, es) ->
        let label = List [ Atom "label"; opt str label ] in
        let level = string_of_int level in
        List
          [ Atom level; label; List (List.map (at.at (inline_element at)) es) ]
    | `Tag t -> tag at t

  let docs at : Ast.t -> sexp =
   fun f -> List (List.map (at.at (block_element at)) f)
end

let error err = Atom (Odoc_parser.Warning.to_string err)

let parser_output formatter v =
  let ast, warnings = Odoc_parser.(ast v, warnings v) in
  let value = Ast_to_sexp.(docs loc_at ast) in
  let warnings = List (List.map error warnings) in
  let output =
    List [ List [ Atom "output"; value ]; List [ Atom "warnings"; warnings ] ]
  in
  Sexplib0.Sexp.pp_hum formatter output;
  Format.pp_print_flush formatter ()

let test ?(location = { Loc.line = 1; column = 0 }) str =
  let dummy_filename = "f.ml" in
  let location =
    {
      Lexing.pos_fname = dummy_filename;
      pos_lnum = location.line;
      pos_bol = 0;
      pos_cnum = location.column;
    }
  in
  let ast = Odoc_parser.parse_comment ~location ~text:str in
  Format.printf "%a" parser_output ast

[@@@ocaml.warning "-32"]

let%expect_test _ =
  let module Trivial = struct
    let empty =
      test "";
      [%expect "((output ()) (warnings ()))"]

    let space =
      test " ";
      [%expect "((output ()) (warnings ()))"]

    let two_spaces =
      test "  ";
      [%expect "((output ()) (warnings ()))"]

    let tab =
      test "\t";
      [%expect "((output ()) (warnings ()))"]

    let mixed_space =
      test " \t \t";
      [%expect "((output ()) (warnings ()))"]

    let newline =
      test "\n";
      [%expect "((output ()) (warnings ()))"]

    let blank_line =
      test "\n\n";
      [%expect "((output ()) (warnings ()))"]

    let cf_lf =
      test "\r\n";
      [%expect "((output ()) (warnings ()))"]
  end in
  ()

let%expect_test _ =
  let module One_paragraph = struct
    let word =
      test "foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))))
         (warnings ())) |}]

    let two_words =
      test "foo bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 7)) (word bar)))))))
         (warnings ())) |}]

    let two_words =
      test "foo bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 7)) (word bar)))))))
         (warnings ())) |}]

    let two_spaces =
      test "foo  bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 5)) space)
              ((f.ml (1 5) (1 8)) (word bar)))))))
         (warnings ())) |}]

    let mixed_space =
      test "foo \t \t bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 8)) space)
              ((f.ml (1 8) (1 11)) (word bar)))))))
         (warnings ())) |}]

    let two_lines =
      test "foo\n";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))))
         (warnings ())) |}]

    let two_lines_cr_lf =
      test "foo\r\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (2 0)) space)
              ((f.ml (2 0) (2 3)) (word bar)))))))
         (warnings ())) |}]

    let leading_space =
      test " foo";
      [%expect
        {|
        ((output
          (((f.ml (1 1) (1 4)) (paragraph (((f.ml (1 1) (1 4)) (word foo)))))))
         (warnings ())) |}]

    let trailing_space =
      test "foo ";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))))
         (warnings ())) |}]

    let leading_space_on_line =
      test "foo\n bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (2 1)) space)
              ((f.ml (2 1) (2 4)) (word bar)))))))
         (warnings ())) |}]

    let trailing_space_on_line =
      test "foo \nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (2 0)) space)
              ((f.ml (2 0) (2 3)) (word bar)))))))
         (warnings ())) |}]

    let leading_tab_on_line =
      test "foo\n\tbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (2 1)) space)
              ((f.ml (2 1) (2 4)) (word bar)))))))
         (warnings ())) |}]

    let trailing_tab_on_line =
      test "foo\t\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (2 0)) space)
              ((f.ml (2 0) (2 3)) (word bar)))))))
         (warnings ())) |}]

    let email =
      test "foo@bar.com";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph (((f.ml (1 0) (1 11)) (word foo@bar.com)))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Two_paragraphs = struct
    let basic =
      test "foo\n\nbar";
      [%expect
        {|
      ((output
        (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
         ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))
       (warnings ())) |}]

    let leading_space =
      test "foo \n\nbar";
      [%expect
        {|
      ((output
        (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
         ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))
       (warnings ())) |}]

    let trailing_space =
      test "foo\n\n bar";
      [%expect
        {|
      ((output
        (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
         ((f.ml (3 1) (3 4)) (paragraph (((f.ml (3 1) (3 4)) (word bar)))))))
       (warnings ())) |}]

    let cr_lf =
      test "foo\r\n\r\nbar";
      [%expect
        {|
      ((output
        (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
         ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))
       (warnings ())) |}]

    let mixed_cr_lf =
      test "foo\n\r\nbar";
      [%expect
        {|
      ((output
        (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
         ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))
       (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Plus_minus_words = struct
    let minus_in_word =
      test "foo-bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 0) (1 7)) (word foo-bar)))))))
         (warnings ())) |}]

    let minus_as_word =
      test "foo -";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 5)) (word -)))))))
         (warnings ())) |}]

    let plus_in_word =
      test "foo+bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 0) (1 7)) (word foo+bar)))))))
         (warnings ())) |}]

    let plus_as_word =
      test "foo +";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 5)) (word +)))))))
         (warnings ())) |}]

    let negative_number =
      test "-3.14 -1337";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 5)) (word -3.14)) ((f.ml (1 5) (1 6)) space)
              ((f.ml (1 6) (1 11)) (word -1337)))))))
         (warnings ())) |}]

    let n_em_dash =
      test "-- ---";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 2)) (word --)) ((f.ml (1 2) (1 3)) space)
              ((f.ml (1 3) (1 6)) (word ---)))))))
         (warnings ())) |}]

    let minus_at =
      test "-@";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word -@)))))))
         (warnings ())) |}]

    let at_minus =
      test "-@-";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word -@-)))))))
         (warnings ())) |}]

    let option =
      test "--option";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word --option)))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Escape_sequence = struct
    let left_brace =
      test "\\{";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word {)))))))
         (warnings ())) |}]

    let left_brace_in_word =
      test "foo\\{bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word foo{bar)))))))
         (warnings ())) |}]

    let right_brace =
      test "\\}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word })))))))
         (warnings ())) |}]

    let right_brace_in_word =
      test "foo\\{bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word foo{bar)))))))
         (warnings ())) |}]

    let left_bracket =
      test "\\[";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word [)))))))
         (warnings ())) |}]

    let left_bracket_in_word =
      test "foo\\[bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word foo[bar)))))))
         (warnings ())) |}]

    let right_bracket =
      test "\\]";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word ])))))))
         (warnings ())) |}]

    let right_bracket_in_word =
      test "foo\\]bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word foo]bar)))))))
         (warnings ())) |}]

    let at =
      test "@";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word @)))))))
         (warnings ( "File \"f.ml\", line 1, characters 0-1:\
                    \nStray '@'."))) |}]

    let not_a_tag =
      test "\\@author";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word @author)))))))
         (warnings ())) |}]

    let at_in_word =
      test "foo\\@bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word foo@bar)))))))
         (warnings ())) |}]

    let trailing_backslash =
      test "foo\\";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (word "foo\\")))))))
         (warnings ())) |}]

    let none_escape =
      test "foo\\bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 0) (1 7)) (word "foo\\bar")))))))
         (warnings ())) |}]

    let backslash_not_escaped =
      test "foo\\\\{bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (paragraph (((f.ml (1 0) (1 9)) (word "foo\\{bar")))))))
         (warnings ())) |}]

    let single_backslash =
      test "\\";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word "\\")))))))
         (warnings ())) |}]

    let escape_minus =
      test "\\{- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 3)) (word {-)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 7)) (word foo)))))))
         (warnings ())) |}]

    let escape_plus =
      test "\\{+ foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 3)) (word {+)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 7)) (word foo)))))))
         (warnings ())) |}]

    let minus_escape =
      test "-\\{";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word -{)))))))
         (warnings ())) |}]

    let plus_escape =
      test "+\\{";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word +{)))))))
         (warnings ())) |}]

    let escape_at =
      test "\\{@author";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (paragraph (((f.ml (1 0) (1 9)) (word {@author)))))))
         (warnings ())) |}]

    let two =
      test "\\{\\}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (word {})))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Code_span = struct
    let basic =
      test "[foo]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 5)) (paragraph (((f.ml (1 0) (1 5)) (code_span foo)))))))
     (warnings ())) |}]

    let empty =
      test "[]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (code_span "")))))))
     (warnings ())) |}]

    let list =
      test "[[]]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (code_span [])))))))
     (warnings ())) |}]
    (* TODO The next two error messages are particularly unintuitive. *)

    let unbalanced_list =
      test "[[]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (code_span [])))))))
     (warnings
      ( "File \"f.ml\", line 1, characters 3-3:\
       \nEnd of text is not allowed in '[...]' (code)."))) |}]

    let no_markup =
      test "[{b";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (code_span {b)))))))
     (warnings
      ( "File \"f.ml\", line 1, characters 3-3:\
       \nEnd of text is not allowed in '[...]' (code)."))) |}]

    let few_escapes =
      test "[\\{]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (code_span "\\{")))))))
     (warnings ())) |}]

    let escaped_right_bracket =
      test "[\\]]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (code_span ])))))))
     (warnings ())) |}]

    let escaped_left_bracket =
      test "[\\[]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (code_span [)))))))
     (warnings ())) |}]

    let whitespace_preserved =
      test "[ foo bar ]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 11))
        (paragraph (((f.ml (1 0) (1 11)) (code_span " foo bar ")))))))
     (warnings ())) |}]

    let no_new_lines =
      test "[foo\nbar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (2 4))
        (paragraph (((f.ml (1 0) (2 4)) (code_span  "foo\
                                                   \nbar")))))))
     (warnings ())) |}]

    let cr_lf_preserved =
      test "[foo\r\nbar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (2 4))
        (paragraph (((f.ml (1 0) (2 4)) (code_span  "foo\r\
                                                   \nbar")))))))
     (warnings ())) |}]

    let no_double_new_line =
      test "[foo\r\n\r\nbar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (3 4))
        (paragraph (((f.ml (1 0) (3 4)) (code_span  "foo\
                                                   \nbar")))))))
     (warnings
      ( "File \"f.ml\", line 1, character 4 to line 3, character 0:\
       \nBlank line is not allowed in '[...]' (code)."))) |}]

    let no_double_crlf =
      test "[foo\r\n\r\nbar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (3 4))
        (paragraph (((f.ml (1 0) (3 4)) (code_span  "foo\
                                                   \nbar")))))))
     (warnings
      ( "File \"f.ml\", line 1, character 4 to line 3, character 0:\
       \nBlank line is not allowed in '[...]' (code)."))) |}]

    let not_merged =
      test "[foo][bar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 10))
        (paragraph
         (((f.ml (1 0) (1 5)) (code_span foo))
          ((f.ml (1 5) (1 10)) (code_span bar)))))))
     (warnings ())) |}]

    let explicit_space =
      test "[foo] [bar]";
      [%expect
        {|
    ((output
      (((f.ml (1 0) (1 11))
        (paragraph
         (((f.ml (1 0) (1 5)) (code_span foo)) ((f.ml (1 5) (1 6)) space)
          ((f.ml (1 6) (1 11)) (code_span bar)))))))
     (warnings ())) |}]

    let untermindated =
      test "[foo";
      [%expect
        {|
  ((output
    (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (code_span foo)))))))
   (warnings
    ( "File \"f.ml\", line 1, characters 4-4:\
     \nEnd of text is not allowed in '[...]' (code)."))) |}]
  end in
  ()

let%expect_test _ =
  let module Bold = struct
    let basic =
      test "{b foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (bold (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let extra_leading_whitespace =
      test "{b  \t foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (bold (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline =
      test "{b\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (bold (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_cr_lf =
      test "{b\r\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (bold (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline_and_whitespace =
      test "{b\n foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (paragraph
             (((f.ml (1 0) (2 5)) (bold (((f.ml (2 1) (2 4)) (word foo))))))))))
         (warnings ())) |}]

    let no_leading_whitespace =
      test "{bfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (bold (((f.ml (1 2) (1 5)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-2:\
           \n'{b' should be followed by space, a tab, or a new line."))) |}]

    let trailing_whitespace =
      test "{b foo }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (bold (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let trailing_newline =
      test "{b foo\n}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 1))
            (paragraph
             (((f.ml (1 0) (2 1)) (bold (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let trailing_cr_lf =
      test "{b foo\r\n}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 1))
            (paragraph
             (((f.ml (1 0) (2 1)) (bold (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let two_words =
      test "{b foo bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (1 6) (1 7)) space)
                 ((f.ml (1 7) (1 10)) (word bar))))))))))
         (warnings ())) |}]

    let not_merged =
      test "{b foo}{b bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (paragraph
             (((f.ml (1 0) (1 7)) (bold (((f.ml (1 3) (1 6)) (word foo)))))
              ((f.ml (1 7) (1 14)) (bold (((f.ml (1 10) (1 13)) (word bar))))))))))
         (warnings ())) |}]

    let nested =
      test "{b foo{b bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (paragraph
             (((f.ml (1 0) (1 14))
               (bold
                (((f.ml (1 3) (1 6)) (word foo))
                 ((f.ml (1 6) (1 13)) (bold (((f.ml (1 9) (1 12)) (word bar)))))))))))))
         (warnings ())) |}]

    let newline =
      test "{b foo\nbar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (1 6) (2 0)) space)
                 ((f.ml (2 0) (2 3)) (word bar))))))))))
         (warnings ())) |}]

    let cr_lf =
      test "{b foo\r\nbar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (1 6) (2 0)) space)
                 ((f.ml (2 0) (2 3)) (word bar))))))))))
         (warnings ())) |}]

    let minus =
      test "{b -}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph (((f.ml (1 0) (1 5)) (bold (((f.ml (1 3) (1 4)) (word -))))))))))
         (warnings ())) |}]

    let minus_list_item =
      test "{b foo\n - bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7))
            (paragraph
             (((f.ml (1 0) (2 7))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (1 6) (2 1)) space)
                 ((f.ml (2 1) (2 2)) (word -)) ((f.ml (2 2) (2 3)) space)
                 ((f.ml (2 3) (2 6)) (word bar))))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 1-2:\
           \n'-' (bulleted list item) is not allowed in '{b ...}' (boldface text).\
           \nSuggestion: move '-' so it isn't the first thing on the line."))) |}]

    let plus_list_item =
      test "{b foo\n + bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7))
            (paragraph
             (((f.ml (1 0) (2 7))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (1 6) (2 1)) space)
                 ((f.ml (2 1) (2 2)) (word +)) ((f.ml (2 2) (2 3)) space)
                 ((f.ml (2 3) (2 6)) (word bar))))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 1-2:\
           \n'+' (numbered list item) is not allowed in '{b ...}' (boldface text).\
           \nSuggestion: move '+' so it isn't the first thing on the line."))) |}]

    let immediate_minus_list_item =
      test "{b\n- foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6))
            (paragraph
             (((f.ml (1 0) (2 6))
               (bold
                (((f.ml (2 0) (2 1)) (word -)) ((f.ml (2 1) (2 2)) space)
                 ((f.ml (2 2) (2 5)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-1:\
           \n'-' (bulleted list item) is not allowed in '{b ...}' (boldface text).\
           \nSuggestion: move '-' so it isn't the first thing on the line."))) |}]

    let immediate_plus_list_item =
      test "{b\n+ foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6))
            (paragraph
             (((f.ml (1 0) (2 6))
               (bold
                (((f.ml (2 0) (2 1)) (word +)) ((f.ml (2 1) (2 2)) space)
                 ((f.ml (2 2) (2 5)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-1:\
           \n'+' (numbered list item) is not allowed in '{b ...}' (boldface text).\
           \nSuggestion: move '+' so it isn't the first thing on the line."))) |}]

    let blank_line =
      test "{b foo\n\nbar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 4))
            (paragraph
             (((f.ml (1 0) (3 4))
               (bold
                (((f.ml (1 3) (1 6)) (word foo)) ((f.ml (2 0) (2 0)) space)
                 ((f.ml (3 0) (3 3)) (word bar))))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-0:\
           \nBlank line is not allowed in '{b ...}' (boldface text)."))) |}]

    let immediate_blank_line =
      test "{b";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (bold ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-2:\
           \n'{b' should be followed by space, a tab, or a new line."
            "File \"f.ml\", line 1, characters 2-2:\
           \nEnd of text is not allowed in '{b ...}' (boldface text)."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{b ...}' (boldface text) should not be empty."))) |}]

    let end_of_comment =
      test "{b foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (bold (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{b ...}' (boldface text)."))) |}]

    let nested_code_block =
      test "{b {[foo]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (bold ())))))
           ((f.ml (1 3) (1 10)) (code_block ((f.ml (1 5) (1 8)) foo)))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-10:\
           \n'{[...]}' (code block) is not allowed in '{b ...}' (boldface text)."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{b ...}' (boldface text) should not be empty."
            "File \"f.ml\", line 1, characters 3-10:\
           \n'{[...]}' (code block) should begin on its own line."))) |}]

    let degenerate =
      test "{b}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (bold ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-3:\
           \n'{b ...}' (boldface text) should not be empty."))) |}]

    let empty =
      test "{b }";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (bold ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'{b ...}' (boldface text) should not be empty."))) |}]
  end in
  ()

let%expect_test _ =
  let module Italic = struct
    let basic =
      test "{i foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (italic (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let extra_leading_whitespace =
      test "{i  \t foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (italic (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline =
      test "{i\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (italic (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline_and_whitespace =
      test "{i\n foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (paragraph
             (((f.ml (1 0) (2 5)) (italic (((f.ml (2 1) (2 4)) (word foo))))))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Emphasis = struct
    let basic =
      test "{e foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (emphasis (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let extra_leading_whitespace =
      test "{e  \t foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (emphasis (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline =
      test "{e\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (emphasis (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline_and_whitespace =
      test "{e\n foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (paragraph
             (((f.ml (1 0) (2 5)) (emphasis (((f.ml (2 1) (2 4)) (word foo))))))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Superscript = struct
    let basic =
      test "{^ foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (superscript (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let extra_leading_whitespace =
      test "{^  \t foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (superscript (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline =
      test "{^\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (superscript (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_cr_lf =
      test "{^\r\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (superscript (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline_and_whitespace =
      test "{^\n foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (paragraph
             (((f.ml (1 0) (2 5)) (superscript (((f.ml (2 1) (2 4)) (word foo))))))))))
         (warnings ())) |}]

    let no_whitespace =
      test "{^foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (superscript (((f.ml (1 2) (1 5)) (word foo))))))))))
         (warnings ())) |}]

    let degenerate =
      test "{^}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (superscript ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-3:\
           \n'{^...}' (superscript) should not be empty."))) |}]

    let empty =
      test "{^ }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (superscript ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'{^...}' (superscript) should not be empty."))) |}]
  end in
  ()

let%expect_test _ =
  let module Subscript = struct
    let basic =
      test "{_ foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (subscript (((f.ml (1 3) (1 6)) (word foo))))))))))
         (warnings ())) |}]

    let extra_leading_whitespace =
      test "{_  \t foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (subscript (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline =
      test "{_\nfoo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (paragraph
             (((f.ml (1 0) (2 4)) (subscript (((f.ml (2 0) (2 3)) (word foo))))))))))
         (warnings ())) |}]

    let leading_newline_and_whitespace =
      test "{_\n foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (paragraph
             (((f.ml (1 0) (2 5)) (subscript (((f.ml (2 1) (2 4)) (word foo))))))))))
         (warnings ())) |}]

    let no_whitespace =
      test "{_foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (subscript (((f.ml (1 2) (1 5)) (word foo))))))))))
         (warnings ())) |}]

    let v_verbose =
      test "{_uv}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 5)) (subscript (((f.ml (1 2) (1 4)) (word uv))))))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Simple_reference = struct
    let basic =
      test "{!foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph (((f.ml (1 0) (1 6)) (simple ((f.ml (1 2) (1 6)) foo) ())))))))
         (warnings ())) |}]

    let leading_whitespace =
      test "{! foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (simple ((f.ml (1 2) (1 7)) " foo") ())))))))
         (warnings ())) |}]

    let trailing_whitespace =
      test "{!foo }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (simple ((f.ml (1 2) (1 7)) "foo ") ())))))))
         (warnings ())) |}]

    let adjacent_word_leading =
      test "bar{!foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 3)) (word bar))
              ((f.ml (1 3) (1 9)) (simple ((f.ml (1 5) (1 9)) foo) ())))))))
         (warnings ())) |}]

    let explicit_leading_space =
      test "bar {!foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 3)) (word bar)) ((f.ml (1 3) (1 4)) space)
              ((f.ml (1 4) (1 10)) (simple ((f.ml (1 6) (1 10)) foo) ())))))))
         (warnings ())) |}]

    let adjacent_word_trailing =
      test "{!foo}bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 6)) (simple ((f.ml (1 2) (1 6)) foo) ()))
              ((f.ml (1 6) (1 9)) (word bar)))))))
         (warnings ())) |}]

    let explicit_trailing_space =
      test "{!foo} bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 6)) (simple ((f.ml (1 2) (1 6)) foo) ()))
              ((f.ml (1 6) (1 7)) space) ((f.ml (1 7) (1 10)) (word bar)))))))
         (warnings ())) |}]

    let kind =
      test "{!val:foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) (simple ((f.ml (1 2) (1 10)) val:foo) ())))))))
         (warnings ())) |}]

    let empty =
      test "{!}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3))
            (paragraph (((f.ml (1 0) (1 3)) (simple ((f.ml (1 2) (1 3)) "") ())))))))
         (warnings ())) |}]

    let whitespace_only =
      test "{! }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 4)) (simple ((f.ml (1 2) (1 4)) " ") ())))))))
         (warnings ())) |}]

    let internal_whitespace =
      test "{!( * )}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) "( * )") ())))))))
         (warnings ())) |}]

    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    let unterminated =
      test "{!foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph (((f.ml (1 0) (1 5)) (simple ((f.ml (1 2) (1 5)) foo) ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-5:\
           \nEnd of text is not allowed in '{!...}' (cross-reference)."))) |}]

    let empty_kind =
      test "{!:foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph (((f.ml (1 0) (1 7)) (simple ((f.ml (1 2) (1 7)) :foo) ())))))))
         (warnings ())) |}]

    let whitespace_kind =
      test "{! :foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) " :foo") ())))))))
         (warnings ())) |}]

    let with_kind_but_empty =
      test "{!val:}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph (((f.ml (1 0) (1 7)) (simple ((f.ml (1 2) (1 7)) val:) ())))))))
         (warnings ())) |}]

    let with_kind_but_whitespace =
      test "{!val: }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) "val: ") ())))))))
         (warnings ())) |}]

    let leading_whitespace_in_kind =
      test "{! val:foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11)) (simple ((f.ml (1 2) (1 11)) " val:foo") ())))))))
         (warnings ())) |}]

    let internal_whitespace_in_kind =
      test "{!va l:foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11)) (simple ((f.ml (1 2) (1 11)) "va l:foo") ())))))))
         (warnings ())) |}]

    let internal_whitespace_in_referent =
      test "{!val:( * )}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 12))
            (paragraph
             (((f.ml (1 0) (1 12)) (simple ((f.ml (1 2) (1 12)) "val:( * )") ())))))))
         (warnings ())) |}]

    let two_colons =
      test "{!val:foo:bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (paragraph
             (((f.ml (1 0) (1 14)) (simple ((f.ml (1 2) (1 14)) val:foo:bar) ())))))))
         (warnings ())) |}]

    let space_before_colon =
      test "{!val :foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11)) (simple ((f.ml (1 2) (1 11)) "val :foo") ())))))))
         (warnings ())) |}]

    let space_after_colon =
      test "{!val: foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11)) (simple ((f.ml (1 2) (1 11)) "val: foo") ())))))))
         (warnings ())) |}]

    let unterminated_after_kind =
      test "{!val:foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 9)) (simple ((f.ml (1 2) (1 9)) val:foo) ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 9-9:\
           \nEnd of text is not allowed in '{!...}' (cross-reference)."))) |}]

    let operator =
      test "{!(>>=)}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) "(>>=)") ())))))))
         (warnings ())) |}]

    let operator_with_dash =
      test "{!(@->)}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) "(@->)") ())))))))
         (warnings ())) |}]

    let operator_with_dot =
      test "{!(*.)}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph
             (((f.ml (1 0) (1 7)) (simple ((f.ml (1 2) (1 7)) "(*.)") ())))))))
         (warnings ())) |}]

    let operator_with_colon =
      test "{!(>::)}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (simple ((f.ml (1 2) (1 8)) "(>::)") ())))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Reference_with_text = struct
    let basic =
      test "{{!foo} bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 12))
            (paragraph
             (((f.ml (1 0) (1 12))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 11)) (word bar))))))))))
         (warnings ())) |}]

    let degenerate =
      test "{{!foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 8)) (with_text ((f.ml (1 3) (1 7)) foo) ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-8:\
           \n'{{!...} ...}' (cross-reference) should not be empty."))) |}]

    let empty =
      test "{{!foo} }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 9)) (with_text ((f.ml (1 3) (1 7)) foo) ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-9:\
           \n'{{!...} ...}' (cross-reference) should not be empty."))) |}]

    let nested_markup =
      test "{{!foo} {b bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 15)) (bold (((f.ml (1 11) (1 14)) (word bar)))))))))))))
         (warnings ())) |}]

    let in_markup =
      test "{e {{!foo} bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16))
               (emphasis
                (((f.ml (1 3) (1 15))
                  (with_text ((f.ml (1 6) (1 10)) foo)
                   (((f.ml (1 11) (1 14)) (word bar)))))))))))))
         (warnings ())) |}]

    let no_separating_space =
      test "{{!foo}bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 7) (1 10)) (word bar))))))))))
         (warnings ())) |}]

    let kind =
      test "{{!val:foo} bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16))
               (with_text ((f.ml (1 3) (1 11)) val:foo)
                (((f.ml (1 12) (1 15)) (word bar))))))))))
         (warnings ())) |}]

    let nested_reference =
      test "{{!foo} {!bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 15))
            (paragraph
             (((f.ml (1 0) (1 15))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 14)) (simple ((f.ml (1 10) (1 14)) bar) ()))))))))))
         (warnings ())) |}]

    let nested_empty =
      test "{{!foo} {{!bar}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (paragraph
             (((f.ml (1 0) (1 17))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 16)) (with_text ((f.ml (1 11) (1 15)) bar) ()))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-16:\
           \n'{{!...} ...}' (cross-reference) should not be empty."))) |}]

    let nested_through_emphasis =
      test "{{!foo} {e {{!bar} baz}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 25))
            (paragraph
             (((f.ml (1 0) (1 25))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 24))
                  (emphasis
                   (((f.ml (1 11) (1 23))
                     (with_text ((f.ml (1 14) (1 18)) bar)
                      (((f.ml (1 19) (1 22)) (word baz))))))))))))))))
         (warnings ())) |}]

    let simple_through_emphasis =
      test "{{!foo} {e {!bar}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 19))
            (paragraph
             (((f.ml (1 0) (1 19))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 18))
                  (emphasis
                   (((f.ml (1 11) (1 17)) (simple ((f.ml (1 13) (1 17)) bar) ())))))))))))))
         (warnings ())) |}]

    let empty_target =
      test "{{!} foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 9))
               (with_text ((f.ml (1 3) (1 4)) "") (((f.ml (1 5) (1 8)) (word foo))))))))))
         (warnings ())) |}]

    let whitespace_only_in_target =
      test "{{! } foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10))
               (with_text ((f.ml (1 3) (1 5)) " ") (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings ())) |}]

    let internal_whitespace =
      test "{{!( * )} baz}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (paragraph
             (((f.ml (1 0) (1 14))
               (with_text ((f.ml (1 3) (1 9)) "( * )")
                (((f.ml (1 10) (1 13)) (word baz))))))))))
         (warnings ())) |}]

    let unterminated =
      test "{{!foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (with_text ((f.ml (1 3) (1 6)) foo) ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{{!...} ...}' (cross-reference)."
            "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{{!...} ...}' (cross-reference)."
            "File \"f.ml\", line 1, characters 0-6:\
           \n'{{!...} ...}' (cross-reference) should not be empty."))) |}]

    let unterminated_content =
      test "{{!foo} bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 11)) (word bar))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 11-11:\
           \nEnd of text is not allowed in '{{!...} ...}' (cross-reference)."))) |}]
  end in
  ()

let%expect_test _ =
  let module Link = struct
    let basic =
      test "{{:foo} bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 12))
            (paragraph
             (((f.ml (1 0) (1 12)) (foo (((f.ml (1 8) (1 11)) (word bar))))))))))
         (warnings ())) |}]

    let nested_markup =
      test "{{:foo} {b bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16))
               (foo
                (((f.ml (1 8) (1 15)) (bold (((f.ml (1 11) (1 14)) (word bar)))))))))))))
         (warnings ())) |}]

    let in_markup =
      test "{e {{:foo} bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16))
               (emphasis
                (((f.ml (1 3) (1 15)) (foo (((f.ml (1 11) (1 14)) (word bar)))))))))))))
         (warnings ())) |}]

    let no_separating_space =
      test "{{:foo}bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph
             (((f.ml (1 0) (1 11)) (foo (((f.ml (1 7) (1 10)) (word bar))))))))))
         (warnings ())) |}]

    let nested_link =
      test "{{:foo} {{:bar} baz}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 21))
            (paragraph
             (((f.ml (1 0) (1 21))
               (foo
                (((f.ml (1 8) (1 20)) (bar (((f.ml (1 16) (1 19)) (word baz)))))))))))))
         (warnings ())) |}]

    let nested_through_emphasis =
      test "{{:foo} {e {{:bar} baz}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 25))
            (paragraph
             (((f.ml (1 0) (1 25))
               (foo
                (((f.ml (1 8) (1 24))
                  (emphasis
                   (((f.ml (1 11) (1 23)) (bar (((f.ml (1 19) (1 22)) (word baz))))))))))))))))
         (warnings ())) |}]

    let reference_through_emphasis =
      test "{{:foo} {e {!bar}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 19))
            (paragraph
             (((f.ml (1 0) (1 19))
               (foo
                (((f.ml (1 8) (1 18))
                  (emphasis
                   (((f.ml (1 11) (1 17)) (simple ((f.ml (1 13) (1 17)) bar) ())))))))))))))
         (warnings ())) |}]

    let nested_in_reference =
      test "{{!foo} {e {{:bar} baz}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 25))
            (paragraph
             (((f.ml (1 0) (1 25))
               (with_text ((f.ml (1 3) (1 7)) foo)
                (((f.ml (1 8) (1 24))
                  (emphasis
                   (((f.ml (1 11) (1 23)) (bar (((f.ml (1 19) (1 22)) (word baz))))))))))))))))
         (warnings ())) |}]

    let empty_target =
      test "{{:} foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph (((f.ml (1 0) (1 9)) ("" (((f.ml (1 5) (1 8)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'{{:...} ...}' (external link) should not be empty."))) |}]

    let whitespace_only_in_target =
      test "{{: } foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph
             (((f.ml (1 0) (1 10)) ("" (((f.ml (1 6) (1 9)) (word foo))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-5:\
           \n'{{:...} ...}' (external link) should not be empty."))) |}]

    let empty =
      test "{{:foo}}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (foo ())))))))
         (warnings ())) |}]

    let internal_whitespace =
      test "{{:foo bar} baz}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (paragraph
             (((f.ml (1 0) (1 16)) ("foo bar" (((f.ml (1 12) (1 15)) (word baz))))))))))
         (warnings ())) |}]

    let unterminated =
      test "{{:foo";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (paragraph (((f.ml (1 0) (1 6)) (foo ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{{:...} ...}' (external link)."
            "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{{:...} ...}' (external link)."))) |}]

    let single_braces =
      test "{:foo}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (paragraph (((f.ml (1 0) (1 6)) (foo ())))))))
         (warnings ())) |}]

    let unterminated_single_braces =
      test "{:foo";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 5)) (paragraph (((f.ml (1 0) (1 5)) (foo ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-5:\
           \nEnd of text is not allowed in '{:...} (external link)'."))) |}]

    let empty_single_braces =
      test "{:}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) ("" ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-3:\
           \n'{:...} (external link)' should not be empty."))) |}]

    let single_braces_whitespace_only =
      test "{: }";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) ("" ())))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'{:...} (external link)' should not be empty."))) |}]
  end in
  ()

let%expect_test _ =
  let module Module_list = struct
    let basic =
      test "{!modules:Foo}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 14)) (modules (((f.ml (1 0) (1 14)) Foo))))))
         (warnings ())) |}]

    let two =
      test "{!modules:Foo Bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 18))
            (modules (((f.ml (1 0) (1 18)) Foo) ((f.ml (1 0) (1 18)) Bar))))))
         (warnings ())) |}]

    let extra_whitespace =
      test "{!modules: Foo  Bar }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 21))
            (modules (((f.ml (1 0) (1 21)) Foo) ((f.ml (1 0) (1 21)) Bar))))))
         (warnings ())) |}]

    let newline =
      test "{!modules:Foo\nBar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (modules (((f.ml (1 0) (2 4)) Foo) ((f.ml (1 0) (2 4)) Bar))))))
         (warnings ())) |}]

    let cr_lf =
      test "{!modules:Foo\r\nBar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 4))
            (modules (((f.ml (1 0) (2 4)) Foo) ((f.ml (1 0) (2 4)) Bar))))))
         (warnings ())) |}]

    let empty =
      test "{!modules:}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 11)) (modules ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-11:\
           \n'{!modules ...}' should not be empty."))) |}]

    let whitespace_only =
      test "{!modules: }";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 12)) (modules ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-12:\
           \n'{!modules ...}' should not be empty."))) |}]

    let unterminated =
      test "{!modules:";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 10)) (modules ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-10:\
           \nEnd of text is not allowed in '{!modules ...}'."
            "File \"f.ml\", line 1, characters 0-10:\
           \n'{!modules ...}' should not be empty."))) |}]

    let in_paragraph =
      test "foo {!modules:Foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space))))
           ((f.ml (1 4) (1 18)) (modules (((f.ml (1 4) (1 18)) Foo))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-18:\
           \n'{!modules ...}' should begin on its own line."))) |}]

    let followed_by_word =
      test "{!modules:Foo} foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14)) (modules (((f.ml (1 0) (1 14)) Foo))))
           ((f.ml (1 15) (1 18)) (paragraph (((f.ml (1 15) (1 18)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 15-18:\
           \nParagraph should begin on its own line."))) |}]

    let in_list =
      test "- {!modules:Foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (unordered light
             ((((f.ml (1 2) (1 16)) (modules (((f.ml (1 2) (1 16)) Foo))))))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Code_block = struct
    let basic =
      test "{[foo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))))
           (warnings ())) |}]

    let empty =
      test "{[]}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (code_block ((f.ml (1 2) (1 2)) "")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'{[...]}' (code block) should not be empty."))) |}]

    let whitespace_only =
      test "{[ ]}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 5)) (code_block ((f.ml (1 2) (1 3)) "")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-5:\
           \n'{[...]}' (code block) should not be empty."))) |}]

    let blank_line_only =
      test "{[\n  \n]}";
      [%expect
        {|
        ((output (((f.ml (1 0) (3 2)) (code_block ((f.ml (1 2) (3 0)) "")))))
         (warnings
          ( "File \"f.ml\", line 1, character 0 to line 3, character 2:\
           \n'{[...]}' (code block) should not be empty."))) |}]

    let whitespace =
      test "{[foo bar]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 11)) (code_block ((f.ml (1 2) (1 9)) "foo bar")))))
           (warnings ())) |}]

    let newline =
      test "{[foo\nbar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5)) (code_block ((f.ml (1 2) (2 3))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let cr_lf =
      test "{[foo\r\nbar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5)) (code_block ((f.ml (1 2) (2 3))  "foo\r\
                                                               \nbar")))))
         (warnings ())) |}]

    let blank_line =
      test "{[foo\n\nbar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 5)) (code_block ((f.ml (1 2) (3 3))  "foo\
                                                               \n\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace =
      test "{[ foo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) foo)))))
           (warnings ())) |}]

    let leading_whitespace_two =
      test "{[ foo\n bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6)) (code_block ((f.ml (1 2) (2 4))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_two_cr_lf =
      test "{[ foo\r\n bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6)) (code_block ((f.ml (1 2) (2 4))  "foo\r\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_two_different_indent =
      test "{[ foo\n   bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 8)) (code_block ((f.ml (1 2) (2 6))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_two_different_indent_rev =
      test "{[   foo\n bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6)) (code_block ((f.ml (1 2) (2 4))  "  foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_two_different_indent_reloc =
      test "{[ foo\n      bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 11)) (code_block ((f.ml (1 2) (2 9))  "foo\
                                                                \n   bar")))))
         (warnings ())) |}]

    let leading_whitespace_with_empty_line =
      test "{[ foo\n\n bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 6)) (code_block ((f.ml (1 2) (3 4))  "foo\
                                                               \n\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_with_whitespace_line_short =
      test "{[  foo\n \n  bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 7)) (code_block ((f.ml (1 2) (3 5))  "foo\
                                                               \n \
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_with_whitespace_line_long =
      test "{[ foo\n   \n bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 6)) (code_block ((f.ml (1 2) (3 4))  "foo\
                                                               \n  \
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_whitespace_leading_newline =
      test "{[\n  foo\n  bar\n]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (4 2)) (code_block ((f.ml (1 2) (4 0))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_tab =
      test "{[\tfoo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) foo)))))
           (warnings ())) |}]

    let leading_tab_two =
      test "{[\tfoo\n\tbar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 6)) (code_block ((f.ml (1 2) (2 4))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_tab_two_different_indent =
      test "{[\tfoo\n\t\tbar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7)) (code_block ((f.ml (1 2) (2 5))  "foo\
                                                               \nbar")))))
         (warnings ())) |}]

    let leading_newline =
      test "{[\nfoo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 5)) (code_block ((f.ml (1 2) (2 3)) foo)))))
           (warnings ())) |}]

    let leading_cr_lf =
      test "{[\r\nfoo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 5)) (code_block ((f.ml (1 2) (2 3)) foo)))))
           (warnings ())) |}]

    let leading_newlines =
      test "{[\n\nfoo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (3 5)) (code_block ((f.ml (1 2) (3 3)) foo)))))
           (warnings ())) |}]

    let leading_newline_with_space =
      test "{[\n foo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 6)) (code_block ((f.ml (1 2) (2 4)) foo)))))
           (warnings ())) |}]

    let leading_newline_with_trash =
      test "{[ \nfoo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 5)) (code_block ((f.ml (1 2) (2 3)) foo)))))
           (warnings ())) |}]

    let nested_opener =
      test "{[{[]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 6)) (code_block ((f.ml (1 2) (1 4)) {[)))))
           (warnings ())) |}]

    let nested_closer =
      test "{[foo]}]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 7) (1 8)) (paragraph (((f.ml (1 7) (1 8)) (word ])))))
           ((f.ml (1 8) (1 9)) (paragraph (((f.ml (1 8) (1 9)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 7-8:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."
            "File \"f.ml\", line 1, characters 7-8:\
           \nParagraph should begin on its own line."
            "File \"f.ml\", line 1, characters 8-9:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let nested_bracket =
      test "{[]]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 5)) (code_block ((f.ml (1 2) (1 3)) ])))))
           (warnings ())) |}]

    let two_nested_brackets =
      test "{[]]]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 6)) (code_block ((f.ml (1 2) (1 4)) ]])))))
           (warnings ())) |}]

    let nested_brackets_in_text =
      test "{[foo]]bar]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 12)) (code_block ((f.ml (1 2) (1 10)) foo]]bar)))))
           (warnings ())) |}]

    let trailing_whitespace =
      test "{[foo ]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) "foo ")))))
           (warnings ())) |}]

    let trailing_tab =
      test "{[foo\t]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) "foo\t")))))
           (warnings ())) |}]

    let trailing_newline =
      test "{[foo\n]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 2)) (code_block ((f.ml (1 2) (2 0)) foo)))))
           (warnings ())) |}]

    let trailing_cr_lf =
      test "{[foo\r\n]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (2 2)) (code_block ((f.ml (1 2) (2 0)) foo)))))
           (warnings ())) |}]

    let trailing_newlines =
      test "{[foo\n\n]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (3 2)) (code_block ((f.ml (1 2) (3 0)) foo)))))
           (warnings ())) |}]

    let preceded_by_whitespace =
      test "{[foo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))))
           (warnings ())) |}]

    let followed_by_whitespace =
      test "{[foo]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))))
           (warnings ())) |}]

    let two_on_one_line =
      test "{[foo]} {[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 15)) (code_block ((f.ml (1 10) (1 13)) bar)))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-15:\
           \n'{[...]}' (code block) should begin on its own line."))) |}]

    let two =
      test "{[foo]}\n{[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (2 0) (2 7)) (code_block ((f.ml (2 2) (2 5)) bar)))))
         (warnings ())) |}]

    let two_with_blank_line =
      test "{[foo]}\n\n{[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (3 0) (3 7)) (code_block ((f.ml (3 2) (3 5)) bar)))))
         (warnings ())) |}]

    let followed_by_words =
      test "{[foo]} bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word bar)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-11:\
           \nParagraph should begin on its own line."))) |}]

    let preceded_by_words =
      test "foo {[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space))))
           ((f.ml (1 4) (1 11)) (code_block ((f.ml (1 6) (1 9)) bar)))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-11:\
           \n'{[...]}' (code block) should begin on its own line."))) |}]

    let preceded_by_paragraph =
      test "foo\n{[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (2 0) (2 7)) (code_block ((f.ml (2 2) (2 5)) bar)))))
         (warnings ())) |}]

    let followed_by_paragraph =
      test "{[foo]}\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word bar)))))))
         (warnings ())) |}]

    let unterminated =
      test "{[foo";
      [%expect
        {|
        ((output (((f.ml (1 2) (1 5)) (code_block ((f.ml (1 2) (1 3)) foo)))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-5:\
           \nEnd of text is not allowed in '{[...]}' (code block)."))) |}]

    let unterminated_bracket =
      test "{[foo]";
      [%expect
        {|
        ((output (((f.ml (1 2) (1 6)) (code_block ((f.ml (1 2) (1 4)) foo])))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{[...]}' (code block)."))) |}]

    let trailing_cr =
      test "{[foo\r]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) "foo\r")))))
           (warnings ())) |}]

    let comment =
      test "{[(* foo *)\nlet bar = ()]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 14))
            (code_block ((f.ml (1 2) (2 12))  "(* foo *)\
                                             \nlet bar = ()")))))
         (warnings ())) |}]

    let docstring =
      test "{[(** foo *)\nlet bar = ()]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 14))
            (code_block ((f.ml (1 2) (2 12))  "(** foo *)\
                                             \nlet bar = ()")))))
         (warnings ())) |}]

    let docstring_with_code_block =
      test "{[(** {[foo]} *)\nlet bar = ()]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 13)) (code_block ((f.ml (1 2) (1 11)) "(** {[foo")))
           ((f.ml (1 14) (2 13))
            (paragraph
             (((f.ml (1 14) (1 16)) (word "*)")) ((f.ml (1 16) (2 0)) space)
              ((f.ml (2 0) (2 3)) (word let)) ((f.ml (2 3) (2 4)) space)
              ((f.ml (2 4) (2 7)) (word bar)) ((f.ml (2 7) (2 8)) space)
              ((f.ml (2 8) (2 9)) (word =)) ((f.ml (2 9) (2 10)) space)
              ((f.ml (2 10) (2 12)) (word "()")) ((f.ml (2 12) (2 13)) (word ])))))
           ((f.ml (2 13) (2 14)) (paragraph (((f.ml (2 13) (2 14)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 14-16:\
           \nParagraph should begin on its own line."
            "File \"f.ml\", line 2, characters 12-13:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."
            "File \"f.ml\", line 2, characters 13-14:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let code_block_with_meta =
      test "{@ocaml env=f1 version>=4.06 [code goes here]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 46))
            (code_block ((f.ml (1 2) (1 29)) "ocaml env=f1 version>=4.06 ")
             ((f.ml (1 30) (1 44)) "code goes here")))))
         (warnings ())) |}]

    let code_block_empty_meta =
      test "{@[code goes here]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 19))
            (code_block ((f.ml (1 2) (1 2)) "")
             ((f.ml (1 3) (1 17)) "code goes here")))))
         (warnings ())) |}]

    let unterminated_code_block_with_meta =
      test "{@meta[foo";
      [%expect
        {|
        ((output
          (((f.ml (1 7) (1 10))
            (code_block ((f.ml (1 2) (1 6)) meta) ((f.ml (1 7) (1 8)) foo)))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-10:\
           \nEnd of text is not allowed in '{[...]}' (code block)."))) |}]

    let unterminated_code_block_with_meta =
      test "{@met";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 1)) (word {)) ((f.ml (1 1) (1 5)) (word @met)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."
            "File \"f.ml\", line 1, characters 1-5:\
           \nUnknown tag '@met'."))) |}]
  end in
  ()

let%expect_test _ =
  let module Verbatim = struct
    let basic =
      test "{v foo v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim foo)))) (warnings ())) |}]

    let empty =
      test "{v v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 5)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-5:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let degenerate =
      test "{vv}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (verbatim v}))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-4:\
           \n'v}' should be preceded by whitespace."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{v' should be followed by whitespace."))) |}]

    let whitespace_only =
      test "{v  v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let blank_line_only =
      test "{v\n  \nv}";
      [%expect
        {|
        ((output (((f.ml (1 0) (3 2)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, character 0 to line 3, character 2:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let no_leading_whitespace =
      test "{vfoo v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (verbatim foo))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-2:\
           \n'{v' should be followed by whitespace."))) |}]

    let no_trailing_whitespace =
      test "{v foov}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (verbatim foov}))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-8:\
           \n'v}' should be preceded by whitespace."))) |}]

    let multiple_leading_whitespace =
      test "{v  foo v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (verbatim " foo")))) (warnings ())) |}]

    let multiple_trailing_whitespace =
      test "{v foo  v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (verbatim "foo ")))) (warnings ())) |}]

    let leading_tab =
      test "{v\tfoo v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim "\tfoo")))) (warnings ())) |}]

    let leading_newline =
      test "{v\nfoo v}";
      [%expect
        {| ((output (((f.ml (1 0) (2 6)) (verbatim foo)))) (warnings ())) |}]

    let leading_cr_lf =
      test "{v\r\nfoo v}";
      [%expect
        {| ((output (((f.ml (1 0) (2 6)) (verbatim foo)))) (warnings ())) |}]

    let trailing_tab =
      test "{v foo\tv}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim "foo\t")))) (warnings ())) |}]

    let trailing_newline =
      test "{v foo\nv}";
      [%expect
        {| ((output (((f.ml (1 0) (2 2)) (verbatim foo)))) (warnings ())) |}]

    let trailing_cr_lf =
      test "{v foo\r\nv}";
      [%expect
        {| ((output (((f.ml (1 0) (2 2)) (verbatim foo)))) (warnings ())) |}]

    let internal_whitespace =
      test "{v foo bar v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 13)) (verbatim "foo bar")))) (warnings ())) |}]

    let newline =
      test "{v foo\nbar v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (2 6)) (verbatim  "foo\
                                                \nbar")))) (warnings ())) |}]

    let cr_lf =
      test "{v foo\r\nbar v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (2 6)) (verbatim  "foo\r\
                                                \nbar")))) (warnings ())) |}]

    let blank_line =
      test "{v foo\n\nbar v}";
      [%expect
        {|
        ((output (((f.ml (1 0) (3 6)) (verbatim  "foo\
                                                \n\
                                                \nbar")))) (warnings ())) |}]

    let leading_newlines =
      test "{v\n\nfoo v}";
      [%expect
        {| ((output (((f.ml (1 0) (3 6)) (verbatim foo)))) (warnings ())) |}]

    let leading_newline_with_space =
      test "{v\n foo v}";
      [%expect
        {| ((output (((f.ml (1 0) (2 7)) (verbatim " foo")))) (warnings ())) |}]

    let leading_newline_with_trash =
      test "{v \nfoo v}";
      [%expect
        {| ((output (((f.ml (1 0) (2 6)) (verbatim foo)))) (warnings ())) |}]

    let nested_opener =
      test "{v {v v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 8)) (verbatim {v)))) (warnings ())) |}]

    let nested_closer =
      test "{v foo v} v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo))
           ((f.ml (1 10) (1 11)) (paragraph (((f.ml (1 10) (1 11)) (word v)))))
           ((f.ml (1 11) (1 12)) (paragraph (((f.ml (1 11) (1 12)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-11:\
           \nParagraph should begin on its own line."
            "File \"f.ml\", line 1, characters 11-12:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let nested_closer_with_word =
      test "{v {dev} v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (verbatim {dev})))) (warnings ())) |}]

    let nested_v =
      test "{v v v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 7)) (verbatim v)))) (warnings ())) |}]

    let two_nested_vs =
      test "{v vv v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 8)) (verbatim vv)))) (warnings ())) |}]

    let nested_v_at_end =
      test "{v vv}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (verbatim vv}))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-6:\
           \n'v}' should be preceded by whitespace."))) |}]

    let two_nested_vs_at_end =
      test "{v vvv}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 7)) (verbatim vvv}))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-7:\
           \n'v}' should be preceded by whitespace."))) |}]

    let nested_vs_in_text =
      test "{v foovvbar v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 14)) (verbatim foovvbar)))) (warnings ())) |}]

    let trailing_newlines =
      test "{v foo\n\nv}";
      [%expect
        {| ((output (((f.ml (1 0) (3 2)) (verbatim foo)))) (warnings ())) |}]

    let preceded_by_whitespace =
      test "{v foo v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim foo)))) (warnings ())) |}]

    let followed_by_whitespace =
      test "{v foo v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim foo)))) (warnings ())) |}]

    let two_on_one_line =
      test "{v foo v} {v bar v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo)) ((f.ml (1 10) (1 19)) (verbatim bar))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-19:\
           \n'{v ... v}' (verbatim text) should begin on its own line."))) |}]

    let two =
      test "{v foo v}\n{v bar v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo)) ((f.ml (2 0) (2 9)) (verbatim bar))))
         (warnings ())) |}]

    let two_with_blank_line =
      test "{v foo v}\n\n{v bar v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo)) ((f.ml (3 0) (3 9)) (verbatim bar))))
         (warnings ())) |}]

    let followed_by_words =
      test "{v foo v} bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo))
           ((f.ml (1 10) (1 13)) (paragraph (((f.ml (1 10) (1 13)) (word bar)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-13:\
           \nParagraph should begin on its own line."))) |}]

    let preceded_by_words =
      test "foo {v bar v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) space))))
           ((f.ml (1 4) (1 13)) (verbatim bar))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-13:\
           \n'{v ... v}' (verbatim text) should begin on its own line."))) |}]

    let preceded_by_paragraph =
      test "foo\n{v bar v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (2 0) (2 9)) (verbatim bar))))
         (warnings ())) |}]

    let followed_by_paragraph =
      test "{v foo v}\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (verbatim foo))
           ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word bar)))))))
         (warnings ())) |}]

    let unterminated =
      test "{v foo";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (verbatim foo))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-6:\
           \nEnd of text is not allowed in '{v ... v}' (verbatim text)."))) |}]

    let unterminated_v =
      test "{v foo v";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (verbatim "foo v"))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-8:\
           \nEnd of text is not allowed in '{v ... v}' (verbatim text)."))) |}]

    let unterminated_empty =
      test "{v";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-2:\
           \nEnd of text is not allowed in '{v ... v}' (verbatim text)."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let unterminated_whitespace =
      test "{v";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-2:\
           \nEnd of text is not allowed in '{v ... v}' (verbatim text)."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let unterminated_whitespace_2 =
      test "{v";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 2)) (verbatim ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-2:\
           \nEnd of text is not allowed in '{v ... v}' (verbatim text)."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{v ... v}' (verbatim text) should not be empty."))) |}]

    let trailing_cr =
      test "{v foo\rv}";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (verbatim "foo\r")))) (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Shorthand_list = struct
    let basic =
      test "- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))))
         (warnings ())) |}]

    let multiple_items =
      test "- foo\n- bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo))))))
              (((f.ml (2 2) (2 5)) (paragraph (((f.ml (2 2) (2 5)) (word bar)))))))))))
         (warnings ())) |}]

    let two_lists =
      test "- foo\n\n- bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))
           ((f.ml (3 0) (3 5))
            (unordered light
             ((((f.ml (3 2) (3 5)) (paragraph (((f.ml (3 2) (3 5)) (word bar)))))))))))
         (warnings ())) |}]

    let ordered =
      test "+ foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (ordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))))
         (warnings ())) |}]

    let leading_whitespace =
      test "- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))))
         (warnings ())) |}]

    let trailing_whitespace =
      test "- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))))
         (warnings ())) |}]

    let bullet_in_line =
      test "- foo - bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (unordered light
             ((((f.ml (1 2) (1 11))
                (paragraph
                 (((f.ml (1 2) (1 5)) (word foo)) ((f.ml (1 5) (1 6)) space)
                  ((f.ml (1 6) (1 7)) (word -)) ((f.ml (1 7) (1 8)) space)
                  ((f.ml (1 8) (1 11)) (word bar)))))))))))
         (warnings ())) |}]

    let bullet_in_line_immediately =
      test "- - foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (unordered light
             (()
              (((f.ml (1 4) (1 7)) (paragraph (((f.ml (1 4) (1 7)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-3:\
           \n'-' (bulleted list item) should begin on its own line."
            "File \"f.ml\", line 1, characters 0-1:\
           \n'-' (bulleted list item) should not be empty."))) |}]

    let code_block =
      test "- {[foo]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (unordered light
             ((((f.ml (1 2) (1 9)) (code_block ((f.ml (1 4) (1 7)) foo)))))))))
         (warnings ())) |}]

    let verbatim =
      test "- {v foo v}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (unordered light ((((f.ml (1 2) (1 11)) (verbatim foo))))))))
         (warnings ())) |}]

    let multiple_blocks =
      test "- foo\n{[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))
               ((f.ml (2 0) (2 7)) (code_block ((f.ml (2 2) (2 5)) bar)))))))))
         (warnings ())) |}]

    let followed_by_code_block =
      test "- foo\n\n{[bar]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))
           ((f.ml (3 0) (3 7)) (code_block ((f.ml (3 2) (3 5)) bar)))))
         (warnings ())) |}]

    let different_kinds =
      test "- foo\n+ bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))
           ((f.ml (2 0) (2 5))
            (ordered light
             ((((f.ml (2 2) (2 5)) (paragraph (((f.ml (2 2) (2 5)) (word bar)))))))))))
         (warnings ())) |}]

    let no_content =
      test "-";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (unordered light (())))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'-' (bulleted list item) should not be empty."))) |}]

    let immediate_newline =
      test "-\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (unordered light
             ((((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))))
         (warnings ())) |}]

    let immediate_blank_line =
      test "-\n\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 1)) (unordered light (())))
           ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'-' (bulleted list item) should not be empty."))) |}]

    let immediate_markup =
      test "-{b foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (unordered light
             ((((f.ml (1 1) (1 8))
                (paragraph
                 (((f.ml (1 1) (1 8)) (bold (((f.ml (1 4) (1 7)) (word foo))))))))))))))
         (warnings ())) |}]

    let after_code_block =
      test "{[foo]} - bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 13))
            (unordered light
             ((((f.ml (1 10) (1 13)) (paragraph (((f.ml (1 10) (1 13)) (word bar)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-9:\
           \n'-' (bulleted list item) should begin on its own line."))) |}]
  end in
  ()

let%expect_test _ =
  let module Explicit_list = struct
    let basic =
      test "{ul {li foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 13))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))))))
         (warnings ())) |}]

    let ordered =
      test "{ol {li foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 13))
            (ordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))))))
         (warnings ())) |}]

    let two_items =
      test "{ul {li foo} {li bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 22))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo))))))
              (((f.ml (1 17) (1 20)) (paragraph (((f.ml (1 17) (1 20)) (word bar)))))))))))
         (warnings ())) |}]

    let items_on_separate_lines =
      test "{ul {li foo}\n{li bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 9))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo))))))
              (((f.ml (2 4) (2 7)) (paragraph (((f.ml (2 4) (2 7)) (word bar)))))))))))
         (warnings ())) |}]

    let blank_line =
      test "{ul {li foo}\n\n{li bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 9))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo))))))
              (((f.ml (3 4) (3 7)) (paragraph (((f.ml (3 4) (3 7)) (word bar)))))))))))
         (warnings ())) |}]

    let blank_line_in_item =
      test "{ul {li foo\n\nbar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 5))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))
               ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))))))
         (warnings ())) |}]

    let junk =
      test "{ul foo}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (unordered heavy ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-7:\
           \n'foo' is not allowed in '{ul ...}' (bulleted list).\
           \nSuggestion: move 'foo' into a list item, '{li ...}' or '{- ...}'."
            "File \"f.ml\", line 1, characters 0-3:\
           \n'{ul ...}' (bulleted list) should not be empty."))) |}]

    let junk_with_no_whitespace =
      test "{ulfoo}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 7)) (unordered heavy ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-6:\
           \n'foo' is not allowed in '{ul ...}' (bulleted list).\
           \nSuggestion: move 'foo' into a list item, '{li ...}' or '{- ...}'."
            "File \"f.ml\", line 1, characters 0-3:\
           \n'{ul ...}' (bulleted list) should not be empty."))) |}]

    let empty =
      test "{ul}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 4)) (unordered heavy ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-3:\
           \n'{ul ...}' (bulleted list) should not be empty."))) |}]

    let unterminated_list =
      test "{ul";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 3)) (unordered heavy ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-3:\
           \nEnd of text is not allowed in '{ul ...}' (bulleted list)."
            "File \"f.ml\", line 1, characters 0-3:\
           \n'{ul ...}' (bulleted list) should not be empty."))) |}]

    let no_whitespace =
      test "{ul{li foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 12))
            (unordered heavy
             ((((f.ml (1 7) (1 10)) (paragraph (((f.ml (1 7) (1 10)) (word foo)))))))))))
         (warnings ())) |}]

    let whitespace_at_end_of_item =
      test "{ul {li foo\n\n\n}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (4 2))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))))))
         (warnings ())) |}]

    let unterminated_li_syntax =
      test "{ul {li foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 11-11:\
           \nEnd of text is not allowed in '{li ...}' (list item)."
            "File \"f.ml\", line 1, characters 11-11:\
           \nEnd of text is not allowed in '{ul ...}' (bulleted list)."))) |}]

    let unterminated_left_curly_brace =
      test "{ul {- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (unordered heavy
             ((((f.ml (1 7) (1 10)) (paragraph (((f.ml (1 7) (1 10)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 10-10:\
           \nEnd of text is not allowed in '{- ...}' (list item)."
            "File \"f.ml\", line 1, characters 10-10:\
           \nEnd of text is not allowed in '{ul ...}' (bulleted list)."))) |}]

    let empty_li_styntax =
      test "{ul {li }}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 10)) (unordered heavy (())))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-7:\
           \n'{li ...}' (list item) should not be empty."))) |}]

    let empty_left_curly_brace =
      test "{ul {- }}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 9)) (unordered heavy (())))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-6:\
           \n'{- ...}' (list item) should not be empty."))) |}]

    let li_syntax_without_whitespace =
      test "{ul {lifoo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 12))
            (unordered heavy
             ((((f.ml (1 7) (1 10)) (paragraph (((f.ml (1 7) (1 10)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-7:\
           \n'{li ...}' should be followed by space, a tab, or a new line."))) |}]

    let li_syntax_followed_by_newline =
      test "{ul {li\nfoo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (unordered heavy
             ((((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))))
         (warnings ())) |}]

    let li_syntax_followed_by_cr_lf =
      test "{ul {li\r\nfoo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (unordered heavy
             ((((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))))
         (warnings ())) |}]

    let li_syntax_followed_by_blank_line =
      test "{ul {li\n\nfoo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 5))
            (unordered heavy
             ((((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word foo)))))))))))
         (warnings ())) |}]

    let left_curly_brace_without_whitespace =
      test "{ul {-foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (unordered heavy
             ((((f.ml (1 6) (1 9)) (paragraph (((f.ml (1 6) (1 9)) (word foo)))))))))))
         (warnings ())) |}]

    let mixed_list_items =
      test "{ul {li foo} {- bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 21))
            (unordered heavy
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo))))))
              (((f.ml (1 16) (1 19)) (paragraph (((f.ml (1 16) (1 19)) (word bar)))))))))))
         (warnings ())) |}]

    let nested =
      test "{ul {li {ul {li foo}}}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 23))
            (unordered heavy
             ((((f.ml (1 8) (1 21))
                (unordered heavy
                 ((((f.ml (1 16) (1 19))
                    (paragraph (((f.ml (1 16) (1 19)) (word foo)))))))))))))))
         (warnings ())) |}]

    let shorthand_in_explicit =
      test "{ul {li - foo\n- bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7))
            (unordered heavy
             ((((f.ml (1 8) (2 5))
                (unordered light
                 ((((f.ml (1 10) (1 13))
                    (paragraph (((f.ml (1 10) (1 13)) (word foo))))))
                  (((f.ml (2 2) (2 5)) (paragraph (((f.ml (2 2) (2 5)) (word bar)))))))))))))))
         (warnings ())) |}]

    let explicit_in_shorthand =
      test "- {ul {li foo}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 15))
            (unordered light
             ((((f.ml (1 2) (1 15))
                (unordered heavy
                 ((((f.ml (1 10) (1 13))
                    (paragraph (((f.ml (1 10) (1 13)) (word foo)))))))))))))))
         (warnings ())) |}]

    let bare_li_syntax =
      test "{li foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 4) (1 7)) (paragraph (((f.ml (1 4) (1 7)) (word foo)))))
           ((f.ml (1 7) (1 8)) (paragraph (((f.ml (1 7) (1 8)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-3:\
           \n'{li ...}' (list item) is not allowed in top-level text.\
           \nSuggestion: move '{li ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item)."
            "File \"f.ml\", line 1, characters 7-8:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let bare_left_curly_brace =
      test "{- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 3) (1 6)) (paragraph (((f.ml (1 3) (1 6)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-2:\
           \n'{- ...}' (list item) is not allowed in top-level text.\
           \nSuggestion: move '{- ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item)."))) |}]

    let after_code_block =
      test "{[foo]} {ul {li bar}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 21))
            (unordered heavy
             ((((f.ml (1 16) (1 19)) (paragraph (((f.ml (1 16) (1 19)) (word bar)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-11:\
           \n'{ul ...}' (bulleted list) should begin on its own line."))) |}]
  end in
  ()

let%expect_test _ =
  let module Deprecated = struct
    let basic =
      test "@deprecated";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@deprecated)))) (warnings ())) |}]

    let words =
      test "@deprecated foo bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 19))
            (@deprecated
             ((f.ml (1 12) (1 19))
              (paragraph
               (((f.ml (1 12) (1 15)) (word foo)) ((f.ml (1 15) (1 16)) space)
                ((f.ml (1 16) (1 19)) (word bar)))))))))
         (warnings ())) |}]

    let multiline =
      test "@deprecated foo\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (@deprecated
             ((f.ml (1 12) (2 3))
              (paragraph
               (((f.ml (1 12) (1 15)) (word foo)) ((f.ml (1 15) (2 0)) space)
                ((f.ml (2 0) (2 3)) (word bar)))))))))
         (warnings ())) |}]

    let paragraphs =
      test "@deprecated foo\n\nbar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 3))
            (@deprecated
             ((f.ml (1 12) (1 15)) (paragraph (((f.ml (1 12) (1 15)) (word foo)))))
             ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word bar)))))))))
         (warnings ())) |}]

    let whitespace_only =
      test "@deprecated";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@deprecated)))) (warnings ())) |}]

    let immediate_newline =
      test "@deprecated\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (@deprecated
             ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))
         (warnings ())) |}]

    let immediate_cr_lf =
      test "@deprecated\r\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (@deprecated
             ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))
         (warnings ())) |}]

    let immediate_blank_line =
      test "@deprecated\n\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 3))
            (@deprecated
             ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word foo)))))))))
         (warnings ())) |}]

    let extra_whitespace =
      test "@deprecated  foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 16))
            (@deprecated
             ((f.ml (1 13) (1 16)) (paragraph (((f.ml (1 13) (1 16)) (word foo)))))))))
         (warnings ())) |}]

    let followed_by_deprecated =
      test "@deprecated foo\n@deprecated bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 15))
            (@deprecated
             ((f.ml (1 12) (1 15)) (paragraph (((f.ml (1 12) (1 15)) (word foo)))))))
           ((f.ml (2 0) (2 15))
            (@deprecated
             ((f.ml (2 12) (2 15)) (paragraph (((f.ml (2 12) (2 15)) (word bar)))))))))
         (warnings ())) |}]

    let followed_by_deprecated_cr_lf =
      test "@deprecated foo\r\n@deprecated bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 15))
            (@deprecated
             ((f.ml (1 12) (1 15)) (paragraph (((f.ml (1 12) (1 15)) (word foo)))))))
           ((f.ml (2 0) (2 15))
            (@deprecated
             ((f.ml (2 12) (2 15)) (paragraph (((f.ml (2 12) (2 15)) (word bar)))))))))
         (warnings ())) |}]

    let nested_in_self =
      test "@deprecated foo @deprecated bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 31))
            (@deprecated
             ((f.ml (1 12) (1 16))
              (paragraph
               (((f.ml (1 12) (1 15)) (word foo)) ((f.ml (1 15) (1 16)) space))))
             ((f.ml (1 16) (1 27))
              (paragraph (((f.ml (1 16) (1 27)) (word @deprecated)))))
             ((f.ml (1 28) (1 31)) (paragraph (((f.ml (1 28) (1 31)) (word bar)))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 16-27:\
           \n'@deprecated' is not allowed in '@deprecated'.\
           \nSuggestion: move '@deprecated' outside of any other markup."))) |}]

    let nested_in_self_at_start =
      test "@deprecated @deprecated foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 27))
            (@deprecated
             ((f.ml (1 12) (1 23))
              (paragraph (((f.ml (1 12) (1 23)) (word @deprecated)))))
             ((f.ml (1 24) (1 27)) (paragraph (((f.ml (1 24) (1 27)) (word foo)))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 12-23:\
           \n'@deprecated' is not allowed in '@deprecated'.\
           \nSuggestion: move '@deprecated' outside of any other markup."))) |}]

    let preceded_by_paragraph =
      test "foo\n@deprecated";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (2 0) (2 11)) (@deprecated))))
         (warnings ())) |}]

    let preceded_by_shorthand_list =
      test "- foo\n@deprecated";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))
           ((f.ml (2 0) (2 11)) (@deprecated))))
         (warnings ())) |}]

    let with_shorthand_list =
      test "@deprecated - foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 17))
            (@deprecated
             ((f.ml (1 12) (1 17))
              (unordered light
               ((((f.ml (1 14) (1 17))
                  (paragraph (((f.ml (1 14) (1 17)) (word foo)))))))))))))
         (warnings ())) |}]

    let with_shorthand_list_after_newline =
      test "@deprecated\n- foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 5))
            (@deprecated
             ((f.ml (2 0) (2 5))
              (unordered light
               ((((f.ml (2 2) (2 5)) (paragraph (((f.ml (2 2) (2 5)) (word foo)))))))))))))
         (warnings ())) |}]

    let prefix =
      test "@deprecatedfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (paragraph (((f.ml (1 0) (1 14)) (word @deprecatedfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-14:\
           \nUnknown tag '@deprecatedfoo'."))) |}]

    let after_code_block =
      test "{[foo]} @deprecated";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 19)) (@deprecated))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-19:\
           \n'@deprecated' should begin on its own line."))) |}]

    let followed_by_section =
      test "@deprecated foo\n{2 Bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 7))
            (@deprecated
             ((f.ml (1 12) (1 15)) (paragraph (((f.ml (1 12) (1 15)) (word foo)))))
             ((f.ml (2 0) (2 7)) (paragraph (((f.ml (2 3) (2 6)) (word Bar)))))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-2:\
           \n'{2 ...}' (section heading) is not allowed in '@deprecated'.\
           \nSuggestion: move '{2' outside of any other markup."))) |}]
  end in
  ()

let%expect_test _ =
  let module Param = struct
    let basic =
      test "@param foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@param foo)))) (warnings ())) |}]

    let bare =
      test "@param";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (@param ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@param' expects parameter name on the same line."))) |}]

    let bare_with_whitespace =
      test "@param";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (@param ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@param' expects parameter name on the same line."))) |}]

    let immediate_newline =
      test "@param\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (@param ""
             ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@param' expects parameter name on the same line."))) |}]

    let followed_by_whitespace =
      test "@param foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@param foo)))) (warnings ())) |}]

    let extra_whitespace =
      test "@param  foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@param foo)))) (warnings ())) |}]

    let words =
      test "@param foo bar baz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 18))
            (@param foo
             ((f.ml (1 11) (1 18))
              (paragraph
               (((f.ml (1 11) (1 14)) (word bar)) ((f.ml (1 14) (1 15)) space)
                ((f.ml (1 15) (1 18)) (word baz)))))))))
         (warnings ())) |}]

    let multiline =
      test "@param foo\nbar\nbaz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 3))
            (@param foo
             ((f.ml (2 0) (3 3))
              (paragraph
               (((f.ml (2 0) (2 3)) (word bar)) ((f.ml (2 3) (3 0)) space)
                ((f.ml (3 0) (3 3)) (word baz)))))))))
         (warnings ())) |}]

    let paragraphs =
      test "@param foo bar\n\nbaz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (3 3))
            (@param foo
             ((f.ml (1 11) (1 14)) (paragraph (((f.ml (1 11) (1 14)) (word bar)))))
             ((f.ml (3 0) (3 3)) (paragraph (((f.ml (3 0) (3 3)) (word baz)))))))))
         (warnings ())) |}]

    let two =
      test "@param foo\n@param bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10)) (@param foo)) ((f.ml (2 0) (2 10)) (@param bar))))
         (warnings ())) |}]

    let nested =
      test "@param foo @param bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 21))
            (@param foo
             ((f.ml (1 11) (1 21))
              (paragraph
               (((f.ml (1 11) (1 21)) (word @param)) ((f.ml (1 11) (1 21)) space)
                ((f.ml (1 11) (1 21)) (word bar)))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 11-21:\
           \n'@param' is not allowed in '@param'.\
           \nSuggestion: move '@param' outside of any other markup."))) |}]

    let preceded_by_paragraph =
      test "foo\n@param bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (2 0) (2 10)) (@param bar))))
         (warnings ())) |}]

    let prefix =
      test "@paramfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (paragraph (((f.ml (1 0) (1 9)) (word @paramfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-9:\
           \nUnknown tag '@paramfoo'."))) |}]

    let after_code_block =
      test "{[foo]} @param foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 18)) (@param foo))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-18:\
           \n'@param' should begin on its own line."))) |}]
  end in
  ()

let%expect_test _ =
  let module Raise = struct
    let basic =
      test "@raise Foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@raise Foo)))) (warnings ())) |}]

    let bare =
      test "@raise";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (@raise ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@raise' expects exception constructor on the same line."))) |}]

    let words =
      test "@raise foo bar baz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 18))
            (@raise foo
             ((f.ml (1 11) (1 18))
              (paragraph
               (((f.ml (1 11) (1 14)) (word bar)) ((f.ml (1 14) (1 15)) space)
                ((f.ml (1 15) (1 18)) (word baz)))))))))
         (warnings ())) |}]

    let prefix =
      test "@raisefoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (paragraph (((f.ml (1 0) (1 9)) (word @raisefoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-9:\
           \nUnknown tag '@raisefoo'."))) |}]
  end in
  ()

let%expect_test _ =
  let module Return = struct
    let basic =
      test "@return";
      [%expect {| ((output (((f.ml (1 0) (1 7)) (@return)))) (warnings ())) |}]

    let words =
      test "@return foo bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 15))
            (@return
             ((f.ml (1 8) (1 15))
              (paragraph
               (((f.ml (1 8) (1 11)) (word foo)) ((f.ml (1 11) (1 12)) space)
                ((f.ml (1 12) (1 15)) (word bar)))))))))
         (warnings ())) |}]

    let prefix =
      test "@returnfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph (((f.ml (1 0) (1 10)) (word @returnfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \nUnknown tag '@returnfoo'."))) |}]
  end in
  ()

let%expect_test _ =
  let module See = struct
    let url =
      test "@see <foo>";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@see url foo)))) (warnings ())) |}]

    let file =
      test "@see 'foo'";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@see file foo)))) (warnings ())) |}]

    let document =
      test "@see \"foo\"";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@see document foo)))) (warnings ())) |}]

    let bare =
      test "@see";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (word @see)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'@see' should be followed by <url>, 'file', or \"document title\"."))) |}]

    let unterminated_url =
      test "@see <foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 4)) (word @see)) ((f.ml (1 4) (1 5)) space)
              ((f.ml (1 5) (1 9)) (word <foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'@see' should be followed by <url>, 'file', or \"document title\"."))) |}]

    let unterminated_file =
      test "@see 'foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9))
            (paragraph
             (((f.ml (1 0) (1 4)) (word @see)) ((f.ml (1 4) (1 5)) space)
              ((f.ml (1 5) (1 9)) (word 'foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'@see' should be followed by <url>, 'file', or \"document title\"."))) |}]

    let unterminated_document =
      test "@see foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8))
            (paragraph
             (((f.ml (1 0) (1 4)) (word @see)) ((f.ml (1 4) (1 5)) space)
              ((f.ml (1 5) (1 8)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-4:\
           \n'@see' should be followed by <url>, 'file', or \"document title\"."))) |}]

    let no_space =
      test "@see<foo>";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (@see url foo)))) (warnings ())) |}]

    let words =
      test "@see <foo> bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (@see url foo
             ((f.ml (1 11) (1 14)) (paragraph (((f.ml (1 11) (1 14)) (word bar)))))))))
         (warnings ())) |}]

    let prefix =
      test "@seefoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 0) (1 7)) (word @seefoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \nUnknown tag '@seefoo'."))) |}]

    let after_code_block =
      test "{[foo]} @see <foo>";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (code_block ((f.ml (1 2) (1 5)) foo)))
           ((f.ml (1 8) (1 18)) (@see url foo))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-18:\
           \n'@see' should begin on its own line."))) |}]

    let url_attempted_nested_closer =
      test "@see <foo>bar>";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (@see url foo
             ((f.ml (1 10) (1 14)) (paragraph (((f.ml (1 10) (1 14)) (word bar>)))))))))
         (warnings ())) |}]

    let file_attempted_nested_closer =
      test "@see 'foo'bar'";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (@see file foo
             ((f.ml (1 10) (1 14)) (paragraph (((f.ml (1 10) (1 14)) (word bar')))))))))
         (warnings ())) |}]

    let document_attempted_nested_closer =
      test "@see \"foo\"bar\"";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 14))
            (@see document foo
             ((f.ml (1 10) (1 14))
              (paragraph (((f.ml (1 10) (1 14)) (word "bar\"")))))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Since = struct
    let basic =
      test "@since foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@since foo)))) (warnings ())) |}]

    let bare =
      test "@since";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (@since ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@since' should not be empty."))) |}]

    let prefix =
      test "@sincefoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 9)) (paragraph (((f.ml (1 0) (1 9)) (word @sincefoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-9:\
           \nUnknown tag '@sincefoo'."))) |}]

    let with_whitespace =
      test "@since foo bar";
      [%expect
        {| ((output (((f.ml (1 0) (1 14)) (@since "foo bar")))) (warnings ())) |}]

    let leading_whitespace =
      test "@since  foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@since foo)))) (warnings ())) |}]

    let trailing_whitespace =
      test "@since foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@since foo)))) (warnings ())) |}]

    let whitespace_only =
      test "@since";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (@since ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'@since' should not be empty."))) |}]
  end in
  ()

let%expect_test _ =
  let module Before = struct
    let basic =
      test "@before Foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@before Foo)))) (warnings ())) |}]

    let bare =
      test "@before";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 7)) (@before ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'@before' expects version number on the same line."))) |}]

    let words =
      test "@before foo bar baz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 19))
            (@before foo
             ((f.ml (1 12) (1 19))
              (paragraph
               (((f.ml (1 12) (1 15)) (word bar)) ((f.ml (1 15) (1 16)) space)
                ((f.ml (1 16) (1 19)) (word baz)))))))))
         (warnings ())) |}]

    let prefix =
      test "@beforefoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph (((f.ml (1 0) (1 10)) (word @beforefoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \nUnknown tag '@beforefoo'."))) |}]
  end in
  ()

let%expect_test _ =
  let module Version = struct
    let basic =
      test "@version foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 12)) (@version foo)))) (warnings ())) |}]

    let bare =
      test "@version";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (@version ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-8:\
           \n'@version' should not be empty."))) |}]

    let prefix =
      test "@versionfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (paragraph (((f.ml (1 0) (1 11)) (word @versionfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-11:\
           \nUnknown tag '@versionfoo'."))) |}]

    let with_whitespace =
      test "@version foo bar";
      [%expect
        {| ((output (((f.ml (1 0) (1 16)) (@version "foo bar")))) (warnings ())) |}]

    let leading_whitespace =
      test "@version  foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 13)) (@version foo)))) (warnings ())) |}]

    let trailing_whitespace =
      test "@version foo";
      [%expect
        {| ((output (((f.ml (1 0) (1 12)) (@version foo)))) (warnings ())) |}]

    let whitespace_only =
      test "@version";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 8)) (@version ""))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-8:\
           \n'@version' should not be empty."))) |}]
  end in
  ()

let%expect_test _ =
  let module Canonical = struct
    let basic =
      test "@canonical Foo";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 14)) (@canonical ((f.ml (1 11) (1 14)) Foo)))))
         (warnings ())) |}]

    let empty =
      test "@canonical";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 10)) (@canonical ((f.ml (1 11) (1 10)) "")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \n'@canonical' should not be empty."))) |}]

    let whitespace_only =
      test "@canonical";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 10)) (@canonical ((f.ml (1 11) (1 10)) "")))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \n'@canonical' should not be empty."))) |}]

    let extra_whitespace =
      test "@canonical  Foo";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 15)) (@canonical ((f.ml (1 11) (1 15)) Foo)))))
         (warnings ())) |}]

    let prefix =
      test "@canonicalfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 13))
            (paragraph (((f.ml (1 0) (1 13)) (word @canonicalfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-13:\
           \nUnknown tag '@canonicalfoo'."))) |}]

    (* TODO This should probably be an error of some kind, as Foo Bar is not a
           valid module path. *)
    let with_whitespace =
      test "@canonical Foo Bar";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 18)) (@canonical ((f.ml (1 11) (1 18)) "Foo Bar")))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Inline = struct
    let basic =
      test "@inline";
      [%expect {| ((output (((f.ml (1 0) (1 7)) @inline))) (warnings ())) |}]

    let prefix =
      test "@inlinefoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph (((f.ml (1 0) (1 10)) (word @inlinefoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \nUnknown tag '@inlinefoo'."))) |}]

    let extra_whitespace =
      test "@inline";
      [%expect {| ((output (((f.ml (1 0) (1 7)) @inline))) (warnings ())) |}]

    let followed_by_junk =
      test "@inline foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @inline)
           ((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-11:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."
            "File \"f.ml\", line 1, characters 8-11:\
           \nParagraph should begin on its own line."))) |}]

    let followed_by_paragraph =
      test "@inline\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @inline)
           ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-3:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."))) |}]

    let followed_by_tag =
      test "@inline\n@deprecated";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 7)) @inline) ((f.ml (2 0) (2 11)) (@deprecated))))
         (warnings ())) |}]

    let with_list =
      test "@inline - foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @inline)
           ((f.ml (1 8) (1 13))
            (unordered light
             ((((f.ml (1 10) (1 13)) (paragraph (((f.ml (1 10) (1 13)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-9:\
           \n'-' (bulleted list item) should begin on its own line."
            "File \"f.ml\", line 1, characters 8-9:\
           \n'-' (bulleted list item) is not allowed in the tags section.\
           \nSuggestion: move '-' (bulleted list item) before any tags."))) |}]
  end in
  ()

let%expect_test _ =
  let module Open = struct
    let basic =
      test "@open";
      [%expect {| ((output (((f.ml (1 0) (1 5)) @open))) (warnings ())) |}]

    let prefix =
      test "@openfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 8)) (paragraph (((f.ml (1 0) (1 8)) (word @openfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-8:\
           \nUnknown tag '@openfoo'."))) |}]

    let extra_whitespace =
      test "@open";
      [%expect {| ((output (((f.ml (1 0) (1 5)) @open))) (warnings ())) |}]

    let followed_by_junk =
      test "@open foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5)) @open)
           ((f.ml (1 6) (1 9)) (paragraph (((f.ml (1 6) (1 9)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-9:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."
            "File \"f.ml\", line 1, characters 6-9:\
           \nParagraph should begin on its own line."))) |}]

    let followed_by_paragraph =
      test "@open\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5)) @open)
           ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-3:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."))) |}]

    let followed_by_tag =
      test "@open\n@deprecated";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 5)) @open) ((f.ml (2 0) (2 11)) (@deprecated))))
         (warnings ())) |}]

    let with_list =
      test "@open - foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5)) @open)
           ((f.ml (1 6) (1 11))
            (unordered light
             ((((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 6-7:\
           \n'-' (bulleted list item) should begin on its own line."
            "File \"f.ml\", line 1, characters 6-7:\
           \n'-' (bulleted list item) is not allowed in the tags section.\
           \nSuggestion: move '-' (bulleted list item) before any tags."))) |}]
  end in
  ()

let%expect_test _ =
  let module Closed = struct
    let basic =
      test "@closed";
      [%expect {| ((output (((f.ml (1 0) (1 7)) @closed))) (warnings ())) |}]

    let prefix =
      test "@closedfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph (((f.ml (1 0) (1 10)) (word @closedfoo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-10:\
           \nUnknown tag '@closedfoo'."))) |}]

    let extra_whitespace =
      test "@closed";
      [%expect {| ((output (((f.ml (1 0) (1 7)) @closed))) (warnings ())) |}]

    let followed_by_junk =
      test "@closed foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @closed)
           ((f.ml (1 8) (1 11)) (paragraph (((f.ml (1 8) (1 11)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-11:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."
            "File \"f.ml\", line 1, characters 8-11:\
           \nParagraph should begin on its own line."))) |}]

    let followed_by_paragraph =
      test "@closed\nfoo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @closed)
           ((f.ml (2 0) (2 3)) (paragraph (((f.ml (2 0) (2 3)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 2, characters 0-3:\
           \nParagraph is not allowed in the tags section.\
           \nSuggestion: move 'foo' before any tags."))) |}]

    let followed_by_tag =
      test "@closed\n@deprecated";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 7)) @closed) ((f.ml (2 0) (2 11)) (@deprecated))))
         (warnings ())) |}]

    let with_list =
      test "@closed - foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) @closed)
           ((f.ml (1 8) (1 13))
            (unordered light
             ((((f.ml (1 10) (1 13)) (paragraph (((f.ml (1 10) (1 13)) (word foo)))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-9:\
           \n'-' (bulleted list item) should begin on its own line."
            "File \"f.ml\", line 1, characters 8-9:\
           \n'-' (bulleted list item) is not allowed in the tags section.\
           \nSuggestion: move '-' (bulleted list item) before any tags."))) |}]
  end in
  ()

let%expect_test _ =
  let module Bad_markup = struct
    let left_brace =
      test "{";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word {)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."))) |}]

    let left_brace_with_letter =
      test "{g";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2))
            (paragraph (((f.ml (1 0) (1 1)) (word {)) ((f.ml (1 1) (1 2)) (word g)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."))) |}]

    let left_brace_with_letters =
      test "{gg";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3))
            (paragraph
             (((f.ml (1 0) (1 1)) (word {)) ((f.ml (1 1) (1 3)) (word gg)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."))) |}]

    let empty_braces =
      test "{}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word {)))))
           ((f.ml (1 1) (1 2)) (paragraph (((f.ml (1 1) (1 2)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."
            "File \"f.ml\", line 1, characters 1-2:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let left_space =
      test "{ foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6)) (paragraph (((f.ml (1 0) (1 6)) (code_span " foo")))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-6:\
           \n'{ foo}': bad markup.\
           \nSuggestion: did you mean '{! foo}' or '[ foo]'?"))) |}]

    let left_spaces =
      test "{  foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph (((f.ml (1 0) (1 7)) (code_span "  foo")))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'{  foo}': bad markup.\
           \nSuggestion: did you mean '{!  foo}' or '[  foo]'?"))) |}]

    let left_space_eof =
      test "{";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word {)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \n'{': bad markup.\
           \nSuggestion: escape the brace with '\\{'."))) |}]

    let braces_instead_of_brackets =
      test "{foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5)) (paragraph (((f.ml (1 0) (1 5)) (code_span foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-5:\
           \n'{foo}': bad markup.\
           \nSuggestion: did you mean '{!foo}' or '[foo]'?"))) |}]

    let right_brace =
      test "}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let right_brace_in_paragraph =
      test "foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (1 3) (1 4)) (paragraph (((f.ml (1 3) (1 4)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-4:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let multiple_right_brace =
      test "foo } bar } baz";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3)) (paragraph (((f.ml (1 0) (1 3)) (word foo)))))
           ((f.ml (1 4) (1 5)) (paragraph (((f.ml (1 4) (1 5)) (word })))))
           ((f.ml (1 6) (1 9)) (paragraph (((f.ml (1 6) (1 9)) (word bar)))))
           ((f.ml (1 10) (1 11)) (paragraph (((f.ml (1 10) (1 11)) (word })))))
           ((f.ml (1 12) (1 15)) (paragraph (((f.ml (1 12) (1 15)) (word baz)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-5:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."
            "File \"f.ml\", line 1, characters 10-11:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let right_brace_in_list_item =
      test "- foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (unordered light
             ((((f.ml (1 2) (1 5)) (paragraph (((f.ml (1 2) (1 5)) (word foo)))))))))
           ((f.ml (1 5) (1 6)) (paragraph (((f.ml (1 5) (1 6)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-6:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let right_brace_in_code_span =
      test "[foo}]";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6)) (paragraph (((f.ml (1 0) (1 6)) (code_span foo})))))))
         (warnings ())) |}]

    let right_brace_in_code_block =
      test "{[foo}]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 8)) (code_block ((f.ml (1 2) (1 6)) foo})))))
           (warnings ())) |}]

    let right_brace_in_verbatim_text =
      test "{v foo} v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (verbatim foo})))) (warnings ())) |}]

    let right_brace_in_author =
      test "@author Foo}";
      [%expect
        {| ((output (((f.ml (1 0) (1 12)) (@author Foo})))) (warnings ())) |}]

    let right_brace_in_deprecated =
      test "@deprecated }";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11)) (@deprecated))
           ((f.ml (1 12) (1 13)) (paragraph (((f.ml (1 12) (1 13)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 12-13:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]

    let right_bracket =
      test "]";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word ])))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-1:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_paragraph =
      test "foo]";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph
             (((f.ml (1 0) (1 3)) (word foo)) ((f.ml (1 3) (1 4)) (word ])))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-4:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_shorthand_list =
      test "- foo]";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (unordered light
             ((((f.ml (1 2) (1 6))
                (paragraph
                 (((f.ml (1 2) (1 5)) (word foo)) ((f.ml (1 5) (1 6)) (word ])))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 5-6:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_code_span =
      test "[]]";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 3))
            (paragraph
             (((f.ml (1 0) (1 2)) (code_span "")) ((f.ml (1 2) (1 3)) (word ])))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-3:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_style =
      test "{b]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 4)) (bold (((f.ml (1 2) (1 3)) (word ]))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-3:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."
            "File \"f.ml\", line 1, characters 0-2:\
           \n'{b' should be followed by space, a tab, or a new line."))) |}]

    let right_bracket_in_verbatim =
      test "{v ] v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 7)) (verbatim ])))) (warnings ())) |}]

    let right_bracket_in_list =
      test "{ul ]}";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 6)) (unordered heavy ()))))
         (warnings
          ( "File \"f.ml\", line 1, characters 4-5:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."
            "File \"f.ml\", line 1, characters 4-5:\
           \n']' is not allowed in '{ul ...}' (bulleted list).\
           \nSuggestion: move ']' into a list item, '{li ...}' or '{- ...}'."
            "File \"f.ml\", line 1, characters 0-3:\
           \n'{ul ...}' (bulleted list) should not be empty."))) |}]

    let right_bracket_in_list_item =
      test "{ul {li ]}}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 11))
            (unordered heavy
             ((((f.ml (1 8) (1 9)) (paragraph (((f.ml (1 8) (1 9)) (word ])))))))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 8-9:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_heading =
      test "{2 ]}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5)) (2 (label ()) (((f.ml (1 3) (1 4)) (word ])))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 3-4:\
           \nUnpaired ']' (end of code).\
           \nSuggestion: try '\\]'."))) |}]

    let right_bracket_in_author =
      test "@author Foo]";
      [%expect
        {| ((output (((f.ml (1 0) (1 12)) (@author Foo])))) (warnings ())) |}]

    let at =
      test "@";
      [%expect
        {|
        ((output (((f.ml (1 0) (1 1)) (paragraph (((f.ml (1 0) (1 1)) (word @)))))))
         (warnings ( "File \"f.ml\", line 1, characters 0-1:\
                    \nStray '@'."))) |}]

    let cr =
      test "";
      [%expect {| ((output ()) (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Utf_8 = struct
    let lambda =
      test "\xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word "\206\187")))))))
         (warnings ())) |}]

    let words =
      test "\xce\xbb \xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 2)) (word "\206\187")) ((f.ml (1 2) (1 3)) space)
              ((f.ml (1 3) (1 5)) (word "\206\187")))))))
         (warnings ())) |}]

    let no_validation =
      test "Î";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word "\195\142")))))))
         (warnings ())) |}]

    let escapes =
      test "\xce\xbb\\}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (word "\206\187}")))))))
         (warnings ())) |}]

    let newline =
      test "\xce\xbb \n \xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (2 3))
            (paragraph
             (((f.ml (1 0) (1 2)) (word "\206\187")) ((f.ml (1 2) (2 1)) space)
              ((f.ml (2 1) (2 3)) (word "\206\187")))))))
         (warnings ())) |}]

    let paragraphs =
      test "\xce\xbb \n\n \xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word "\206\187")))))
           ((f.ml (3 1) (3 3)) (paragraph (((f.ml (3 1) (3 3)) (word "\206\187")))))))
         (warnings ())) |}]

    let code_span =
      test "[\xce\xbb]";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (paragraph (((f.ml (1 0) (1 4)) (code_span "\206\187")))))))
         (warnings ())) |}]

    let minus =
      test "\xce\xbb-\xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph (((f.ml (1 0) (1 5)) (word "\206\187-\206\187")))))))
         (warnings ())) |}]

    let shorthand_list =
      test "- \xce\xbb";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4))
            (unordered light
             ((((f.ml (1 2) (1 4))
                (paragraph (((f.ml (1 2) (1 4)) (word "\206\187")))))))))))
         (warnings ())) |}]

    let styled =
      test "{b \xce\xbb}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 6))
            (paragraph
             (((f.ml (1 0) (1 6)) (bold (((f.ml (1 3) (1 5)) (word "\206\187"))))))))))
         (warnings ())) |}]

    let reference_target =
      test "{!\xce\xbb}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 5))
            (paragraph
             (((f.ml (1 0) (1 5)) (simple ((f.ml (1 2) (1 5)) "\206\187") ())))))))
         (warnings ())) |}]

    let code_block =
      test "{[\xce\xbb]}";
      [%expect
        {|
          ((output (((f.ml (1 0) (1 6)) (code_block ((f.ml (1 2) (1 4)) "\206\187")))))
           (warnings ())) |}]

    let verbatim =
      test "{v \xce\xbb v}";
      [%expect
        {| ((output (((f.ml (1 0) (1 8)) (verbatim "\206\187")))) (warnings ())) |}]

    let label =
      test "{2:\xce\xbb Bar}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (2 (label ("\206\187")) (((f.ml (1 6) (1 9)) (word Bar)))))))
         (warnings ())) |}]

    let author =
      test "@author \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@author "\206\187")))) (warnings ())) |}]

    let param =
      test "@param \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (@param "\206\187")))) (warnings ())) |}]

    let raise =
      test "@raise \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (@raise "\206\187")))) (warnings ())) |}]

    let see =
      test "@see <\xce\xbb>";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (@see url "\206\187")))) (warnings ())) |}]

    let since =
      test "@since \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 9)) (@since "\206\187")))) (warnings ())) |}]

    let before =
      test "@before \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 10)) (@before "\206\187")))) (warnings ())) |}]

    let version =
      test "@version \xce\xbb";
      [%expect
        {| ((output (((f.ml (1 0) (1 11)) (@version "\206\187")))) (warnings ())) |}]

    let right_brace =
      test "\xce\xbb}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 2)) (paragraph (((f.ml (1 0) (1 2)) (word "\206\187")))))
           ((f.ml (1 2) (1 3)) (paragraph (((f.ml (1 2) (1 3)) (word })))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 2-3:\
           \nUnpaired '}' (end of markup).\
           \nSuggestion: try '\\}'."))) |}]
  end in
  ()

let%expect_test _ =
  let module Comment_location = struct
    let error_on_first_line =
      test "@foo";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 4)) (paragraph (((f.ml (1 0) (1 4)) (word @foo)))))))
         (warnings ( "File \"f.ml\", line 1, characters 0-4:\
                    \nUnknown tag '@foo'."))) |}]

    let error_on_second_line =
      test "  \n  @foo";
      [%expect
        {|
        ((output
          (((f.ml (2 2) (2 6)) (paragraph (((f.ml (2 2) (2 6)) (word @foo)))))))
         (warnings ( "File \"f.ml\", line 2, characters 2-6:\
                    \nUnknown tag '@foo'."))) |}]
  end in
  ()

let%expect_test _ =
  let module Unsupported = struct
    (* test "index list"
       "{!indexlist}"
       (Ok []); *)

    let left_alignment =
      test "{L foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 3) (1 6)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'{L ...}' (left alignment) should not be used because it has no effect."))) |}]

    let center_alignment =
      test "{C foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 3) (1 6)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'{C ...}' (center alignment) should not be used because it has no effect."))) |}]

    let right_alignment =
      test "{R foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 3) (1 6)) (word foo)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'{R ...}' (right alignment) should not be used because it has no effect."))) |}]

    let custom_style =
      test "{c foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7))
            (paragraph (((f.ml (1 0) (1 7)) (code_span "c foo")))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \n'{c foo}': bad markup.\
           \nSuggestion: did you mean '{!c foo}' or '[c foo]'?"))) |}]

    let custom_tag =
      test "@custom";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 7)) (paragraph (((f.ml (1 0) (1 7)) (word @custom)))))))
         (warnings
          ( "File \"f.ml\", line 1, characters 0-7:\
           \nUnknown tag '@custom'."))) |}]

    let custom_reference_kind =
      test "{!custom:foo}";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 13))
            (paragraph
             (((f.ml (1 0) (1 13)) (simple ((f.ml (1 2) (1 13)) custom:foo) ())))))))
         (warnings ())) |}]

    let html_tag =
      test "<b>foo</b>";
      [%expect
        {|
        ((output
          (((f.ml (1 0) (1 10))
            (paragraph (((f.ml (1 0) (1 10)) (word <b>foo</b>)))))))
         (warnings ())) |}]
  end in
  ()

let%expect_test _ =
  let module Locations = struct
    (* test "index list"
       "{!indexlist}"
       (Ok []); *)

    let lexing_pos_to_sexp : Lexing.position -> sexp =
     fun v ->
      List
        [
          List [ Atom "pos_fname"; Atom v.pos_fname ];
          List [ Atom "pos_bol"; Atom (string_of_int v.pos_bol) ];
          List [ Atom "pos_lnum"; Atom (string_of_int v.pos_lnum) ];
          List [ Atom "pos_cnum"; Atom (string_of_int v.pos_cnum) ];
        ]

    let parser_output formatter pv =
      let ast, warnings = Odoc_parser.(ast pv, warnings pv) in
      let at conv v =
        let { Loc.start; end_; _ } = Loc.location v in
        let v' = Loc.value v |> conv in
        let start' =
          Odoc_parser.position_of_point pv start |> lexing_pos_to_sexp
        in
        let start'' = Location_to_sexp.point start in
        let end' =
          Odoc_parser.position_of_point pv end_ |> lexing_pos_to_sexp
        in
        let end'' = Location_to_sexp.point end_ in
        List
          [
            List [ Atom "start"; start' ];
            List [ Atom "start_loc"; start'' ];
            List [ Atom "end"; end' ];
            List [ Atom "end_loc"; end'' ];
            List [ Atom "value"; v' ];
          ]
      in
      let sexp = Ast_to_sexp.(docs { at } ast) in
      let warnings = List (List.map error warnings) in
      let output =
        List
          [ List [ Atom "output"; sexp ]; List [ Atom "warnings"; warnings ] ]
      in
      Sexplib0.Sexp.pp_hum formatter output;
      Format.pp_print_flush formatter ()

    let test
        ?(location =
          Lexing.{ pos_bol = 0; pos_cnum = 0; pos_lnum = 1; pos_fname = "none" })
        text =
      let ast = Odoc_parser.parse_comment ~location ~text in
      Format.printf "%a" parser_output ast

    let non_offset_location =
      test "one\n two\n  three";
      [%expect
        {|
        ((output
          (((start ((pos_fname none) (pos_bol 0) (pos_lnum 1) (pos_cnum 0)))
            (start_loc (1 0))
            (end ((pos_fname none) (pos_bol 9) (pos_lnum 3) (pos_cnum 16)))
            (end_loc (3 7))
            (value
             (paragraph
              (((start ((pos_fname none) (pos_bol 0) (pos_lnum 1) (pos_cnum 0)))
                (start_loc (1 0))
                (end ((pos_fname none) (pos_bol 0) (pos_lnum 1) (pos_cnum 3)))
                (end_loc (1 3)) (value (word one)))
               ((start ((pos_fname none) (pos_bol 0) (pos_lnum 1) (pos_cnum 3)))
                (start_loc (1 3))
                (end ((pos_fname none) (pos_bol 4) (pos_lnum 2) (pos_cnum 5)))
                (end_loc (2 1)) (value space))
               ((start ((pos_fname none) (pos_bol 4) (pos_lnum 2) (pos_cnum 5)))
                (start_loc (2 1))
                (end ((pos_fname none) (pos_bol 4) (pos_lnum 2) (pos_cnum 8)))
                (end_loc (2 4)) (value (word two)))
               ((start ((pos_fname none) (pos_bol 4) (pos_lnum 2) (pos_cnum 8)))
                (start_loc (2 4))
                (end ((pos_fname none) (pos_bol 9) (pos_lnum 3) (pos_cnum 11)))
                (end_loc (3 2)) (value space))
               ((start ((pos_fname none) (pos_bol 9) (pos_lnum 3) (pos_cnum 11)))
                (start_loc (3 2))
                (end ((pos_fname none) (pos_bol 9) (pos_lnum 3) (pos_cnum 16)))
                (end_loc (3 7)) (value (word three)))))))))
         (warnings ())) |}]

    let offset_location =
      test
        ~location:
          Lexing.
            { pos_bol = 10; pos_cnum = 20; pos_lnum = 2; pos_fname = "none" }
        "one\n two\n  three";
      [%expect
        {|
          ((output
            (((start ((pos_fname none) (pos_bol 10) (pos_lnum 2) (pos_cnum 20)))
              (start_loc (2 10))
              (end ((pos_fname none) (pos_bol 29) (pos_lnum 4) (pos_cnum 36)))
              (end_loc (4 7))
              (value
               (paragraph
                (((start ((pos_fname none) (pos_bol 10) (pos_lnum 2) (pos_cnum 20)))
                  (start_loc (2 10))
                  (end ((pos_fname none) (pos_bol 10) (pos_lnum 2) (pos_cnum 23)))
                  (end_loc (2 13)) (value (word one)))
                 ((start ((pos_fname none) (pos_bol 10) (pos_lnum 2) (pos_cnum 23)))
                  (start_loc (2 13))
                  (end ((pos_fname none) (pos_bol 24) (pos_lnum 3) (pos_cnum 25)))
                  (end_loc (3 1)) (value space))
                 ((start ((pos_fname none) (pos_bol 24) (pos_lnum 3) (pos_cnum 25)))
                  (start_loc (3 1))
                  (end ((pos_fname none) (pos_bol 24) (pos_lnum 3) (pos_cnum 28)))
                  (end_loc (3 4)) (value (word two)))
                 ((start ((pos_fname none) (pos_bol 24) (pos_lnum 3) (pos_cnum 28)))
                  (start_loc (3 4))
                  (end ((pos_fname none) (pos_bol 29) (pos_lnum 4) (pos_cnum 31)))
                  (end_loc (4 2)) (value space))
                 ((start ((pos_fname none) (pos_bol 29) (pos_lnum 4) (pos_cnum 31)))
                  (start_loc (4 2))
                  (end ((pos_fname none) (pos_bol 29) (pos_lnum 4) (pos_cnum 36)))
                  (end_loc (4 7)) (value (word three)))))))))
           (warnings ())) |}]
  end in
  ()
