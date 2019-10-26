%{
open Common
open Types
open Errors

(* Convert lexing position into error position *)
let position p =
  { line = p.Lexing.pos_lnum;
    column = p.Lexing.pos_cnum - p.Lexing.pos_bol; }

(* Get the location of the symbol at a given position *)
let rhs_loc n =
  { start = position (Parsing.rhs_start_pos n);
    finish = position (Parsing.rhs_end_pos n); }

(* Useful strings *)

let sempty = ""

let sspace = " "

let snewline = "\n"

let sblank_line = "\n\n"

let sminus = "-"

let splus = "+"

(* Accumulators for text elements *)

type text_item =
    Blank
  | Newline
  | Blank_line
  | String of string
  | Element of text_element

let iminus = String sminus

let iplus = String splus

let skip_blank_or_newline = function
  | Blank :: rest -> rest
  | Newline :: rest -> rest
  | til -> til

let rec skip_whitespace = function
  | Blank :: rest -> skip_whitespace rest
  | Newline :: rest -> skip_whitespace rest
  | Blank_line :: rest -> skip_whitespace rest
  | til -> til

let rec convert acc stracc = function
  | [] ->
        if stracc = [] then acc
        else (Raw (String.concat sempty stracc)) :: acc
  | ti :: rest ->
      let acc, stracc =
        match ti with
        | Blank -> acc, (sspace :: stracc)
        | Newline -> acc, (snewline :: stracc)
        | String s -> acc, (s :: stracc)
        | Blank_line ->
            let acc =
              if stracc = [] then acc
              else (Raw (String.concat sempty stracc)) :: acc
            in
              (Newline :: acc), []
        | Element e ->
            let acc =
              if stracc = [] then acc
              else (Raw (String.concat sempty stracc)) :: acc
            in
              (e :: acc), []
      in
        convert acc stracc rest

let text til =
  let til = skip_whitespace til in
  let til = skip_whitespace (List.rev til) in
    convert [] [] til

let inner til =
  let til = skip_blank_or_newline til in
  let til = skip_blank_or_newline (List.rev til) in
    convert [] [] til

(* Error messages *)

let unclosed opening_name opening_num items closing_name closing_num =
  let error =
    let opening_loc = rhs_loc opening_num in
    let opening = opening_name in
    let closing = closing_name in
    Unclosed { opening_loc; opening; items; closing }
  in
  let loc = rhs_loc closing_num in
    raise (ParserError(loc, error))

let expecting num nonterm =
  let error = Expecting nonterm in
  let loc = rhs_loc num in
    raise (ParserError(loc, error))

(* Utilities for error messages *)

let title_to_string (i, _) =
  let i = string_of_int i in
    "{" ^ i

let style_to_string = function
  | SK_bold -> "{b"
  | SK_italic -> "{i"
  | SK_emphasize -> "{e"
  | SK_center -> "{C"
  | SK_left -> "{L"
  | SK_right -> "{R"
  | SK_superscript -> "{^"
  | SK_subscript -> "{_"
  | SK_custom s -> "{" ^ s

let item_to_string i = if i then "{-" else "{li"

let html_open_to_string t = "<" ^ t ^ ">"
let html_close_to_string t = "</" ^ t ^ ">"

%}

%token <string> Param
%token AUTHOR
%token <string> Version
%token <Types.see_ref> See
%token <string> Since
%token <string> Before
%token DEPRECATED
%token <string> Raise
%token RETURN
%token INLINE
%token <string> Custom
%token <string> Canonical

%token BEGIN
%token END

%token <int * string option> Title
%token <Types.style_kind> Style
%token LIST
%token ENUM
%token <bool> Item

%token <Types.ref_kind * string> Ref
%token <Types.special_ref_kind> Special_Ref

%token <string> Code
%token <string> Pre_Code
%token <string> Verb
%token <string option * string> Target

%token <string> HTML_Bold
%token HTML_END_BOLD
%token <string> HTML_Center
%token HTML_END_CENTER
%token <string> HTML_Left
%token HTML_END_LEFT
%token <string> HTML_Right
%token HTML_END_RIGHT
%token <string> HTML_Italic
%token HTML_END_ITALIC
%token <string * int> HTML_Title
%token <int> HTML_END_Title
%token <string> HTML_List
%token HTML_END_LIST
%token <string> HTML_Enum
%token HTML_END_ENUM
%token <string> HTML_Item
%token HTML_END_ITEM

%token MINUS
%token PLUS

%token NEWLINE
%token EOF
%token BLANK
%token <string> Char

%token DOT
%token <string> Ref_part

%start main
%type <Types.t> main

%start reference_parts
%type <(string option * string) list> reference_parts

%nonassoc Shift_error
%right error
%nonassoc Reduce_error

%%

/* Main symbol */

main:
| text              { (text $1, []) }
| text tags         { (text $1, List.rev $2) }
;

/* Entry point for reference parsing */
reference_parts:
| reference_part { [$1] }
| reference_parts DOT reference_part { $3 :: $1 }
;

reference_part:
| Ref_part { (None, $1) }
| Ref_part MINUS Ref_part { (Some $1, $3) }
;

/* Tags */

tags:
| simple_tag whitespace            { [$1] }
| simple_tag error                 { expecting 2 "tag" }
| text_tag                         { [$1] }
| tags simple_tag whitespace       { $2 :: $1 }
| tags simple_tag error            { expecting 3 "tag" }
| tags text_tag                    { $2 :: $1 }
;

simple_tag:
| Version           { Version $1 }
| Since             { Since $1 }
| Canonical         { Canonical $1 }
;

text_tag:
| AUTHOR string          { Author (String.concat sempty $2) }
| See text               { See($1, (text $2)) }
| Before text            { Before($1, (text $2)) }
| DEPRECATED text        { Deprecated (text $2) }
| Param text             { Param($1, (text $2)) }
| Raise text             { Raised_exception($1, (text $2)) }
| RETURN text            { Return_value (text $2) }
| INLINE whitespace      { Inline }
| Custom text            { Custom($1, (text $2)) }
;

/* Various forms of whitespace */

blanks:
| BLANK             { () }
| blanks BLANK      { () }
;

newline:
| NEWLINE           { () }
| blanks NEWLINE    { () }
| newline BLANK     { () }
;

blank_line:
| newline NEWLINE   { () }
| blank_line BLANK   { () }
| blank_line NEWLINE { () }
;

whitespace:
| /* empty */           { [] } %prec Shift_error
| blanks                { [Blank] }
| newline               { [Newline] }
| blank_line            { [Blank_line] }
;

/* Strings */

string:
| whitespace                                      { [] }
| error                                           { expecting 1 "string" }
| string_body whitespace                          { List.rev $1 }
;

string_body:
| string_item                      { [snd $1] }
| string_body string_item          { (snd $2) :: (fst $2) :: $1 }
;

string_item:
| string_char                 { (sempty, $1) }
| blanks string_char          { (sspace, $2) }
| newline string_char         { (snewline, $2) }
| blank_line string_char      { (sblank_line, $2) }
;

string_char:
| Char                        { $1 }
| MINUS                       { sminus }
| PLUS                        { splus }

/* Basic text */

text:
| whitespace                                 { $1 }
| error                                      { expecting 1 "text" }
| text_body whitespace                       { List.rev_append $1 $2 }
| text_body_with_line shortcut_list_final    { List.rev_append $1 [Element (List $2)] }
| text_body_with_line shortcut_enum_final    { List.rev_append $1 [Element (Enum $2)] }
;

text_body:
| text_item                                  { [$1] }
| blanks text_item                           { [$2; Blank] }
| text_body text_item                        { $2 :: $1 }
| text_body blanks text_item                 { $3 :: Blank :: $1 }
| text_body_with_line text_item_after_line   { List.rev_append $2 $1 }
;

text_body_with_line:
| newline                                    { [Newline] }
| blank_line                                 { [Blank_line] }
| text_body newline                          { Newline :: $1 }
| text_body blank_line                        { Blank_line :: $1 }
| text_body_with_line shortcut_list          { (Element (List $2)) :: $1 }
| text_body_with_line shortcut_enum          { (Element (Enum $2)) :: $1 }
;

text_item:
| MINUS                              { iminus }
| PLUS                               { iplus }
| text_element                       { Element $1 }
| html_text_element                  { Element $1 }
| Char                               { String $1 }
;

text_item_after_line:
| MINUS text_item            { [iminus; $2] }
| PLUS text_item             { [iplus; $2] }
| text_element               { [Element $1] }
| html_text_element          { [Element $1] }
| Char                       { [String $1] }
;

/* Text within shortcut lists and enums */

shortcut_text_body:
| blanks text_item                                  { [$2; Blank] }
| newline text_item_after_line                      { List.rev_append $2 [Newline] }
| shortcut_text_body text_item                      { $2 :: $1 }
| shortcut_text_body blanks text_item               { $3 :: Blank :: $1 }
| shortcut_text_body newline text_item_after_line   { List.rev_append $3 (Newline :: $1) }
;

/* Shortcut lists and enums */

shortcut_list:
| MINUS blank_line                                  { [[]] }
| MINUS shortcut_text_body blank_line               { [inner (List.rev $2)] }
| MINUS newline shortcut_list                       { [] :: $3 }
| MINUS shortcut_text_body newline shortcut_list    { (inner (List.rev $2)) :: $4 }
| MINUS error                                       { expecting 2 "list item" }
;

shortcut_enum:
| PLUS blank_line                                   { [[]] }
| PLUS shortcut_text_body blank_line                { [inner (List.rev $2)] }
| PLUS newline shortcut_enum                        { [] :: $3 }
| PLUS shortcut_text_body newline shortcut_enum     { (inner (List.rev $2)) :: $4 }
| PLUS error                                        { expecting 2 "enumerated list item" }
;

/* Shortcut lists and enums that don't require a final blank line */

shortcut_list_final:
| MINUS whitespace                                      { [[]] }
| MINUS shortcut_text_body whitespace                   { [inner (List.rev $2)] }
| MINUS newline shortcut_list_final                     { [] :: $3 }
| MINUS shortcut_text_body newline shortcut_list_final  { (inner (List.rev $2)) :: $4 }
;

shortcut_enum_final:
| PLUS whitespace                                       { [[]] }
| PLUS shortcut_text_body whitespace                    { [inner (List.rev $2)] }
| PLUS newline shortcut_enum_final                      { [] :: $3 }
| PLUS shortcut_text_body newline shortcut_enum_final   { (inner (List.rev $2)) :: $4 }
;

/* Text elements */

text_element:
| Title text END
    { let n, l = $1 in
        Title (n, l, (inner $2)) }
| Title text error
    { unclosed (title_to_string $1) 1 "text" "}" 3 }
| Style text END
    { Style($1, (inner $2)) }
| Style text error
    { unclosed (style_to_string $1) 1 "text" "}" 3 }
| LIST whitespace list whitespace END
    { List (List.rev $3) }
| LIST whitespace list error
    { unclosed "{ul" 1 "list item" "}" 4 }
| LIST whitespace error
    { expecting 3 "list item" }
| ENUM whitespace list whitespace END
    { Enum (List.rev $3) }
| ENUM whitespace list error
    { unclosed "{ol" 1 "list item" "}" 4 }
| ENUM whitespace error
    { expecting 3 "enumerated list item" }
| Ref
    { let k, n = $1 in
        Ref (k, n, None) }
| BEGIN Ref text END
    { let k, n = $2 in
        Ref (k, n, Some (inner $3)) }
| BEGIN Ref text error
    { unclosed "{" 1 "text" "}" 3 }
| Special_Ref
    { Special_ref $1 }
| Code
    { Code $1 }
| Pre_Code
    { PreCode $1 }
| Verb
    { Verbatim $1 }
| Target
    { let t, s = $1 in
        Target (t, s) }
;

/* Lists */

list:
| item                   { [ $1 ] }
| list whitespace item   { $3 :: $1 }
;

item:
  Item text END       { inner $2 }
| Item text error     { unclosed (item_to_string $1) 1 "text" "}" 3 }
;

/* HTML-sytle text elements */

html_text_element:
  HTML_Title text HTML_END_Title
    { let _, n = $1 in
      if n <> $3 then raise Parse_error;
      Title(n, None, (inner $2)) }
| HTML_Title text error
    { let tag, _ = $1 in
      unclosed (html_open_to_string tag) 1
        "text" (html_close_to_string tag) 3 }
| HTML_Bold text HTML_END_BOLD
    { Style(SK_bold, (inner $2)) }
| HTML_Bold text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
| HTML_Italic text HTML_END_ITALIC
    { Style(SK_italic, (inner $2)) }
| HTML_Italic text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
| HTML_Center text HTML_END_CENTER
    { Style(SK_center, (inner $2)) }
| HTML_Center text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
| HTML_Left text HTML_END_LEFT
    { Style(SK_left, (inner $2)) }
| HTML_Left text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
| HTML_Right text HTML_END_RIGHT
    { Style(SK_right, (inner $2)) }
| HTML_Right text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
| HTML_List whitespace html_list whitespace HTML_END_LIST
    { List (List.rev $3) }
| HTML_List whitespace html_list error
    { unclosed (html_open_to_string $1) 1
         "HTML list item" (html_close_to_string $1) 4 }
| HTML_List whitespace error
    { expecting 2 "HTML list item" }
| HTML_Enum whitespace html_list whitespace HTML_END_ENUM
    { Enum (List.rev $3) }
| HTML_Enum whitespace html_list error
    { unclosed (html_open_to_string $1) 1
        "HTML list item" (html_close_to_string $1) 4 }
| HTML_Enum whitespace error
    { expecting 3 "HTML list item" }
;

/* HTML-style lists */

html_list:
| html_item                          { [ $1 ] }
| html_list whitespace html_item     { $3 :: $1 }
;

html_item:
  HTML_Item text HTML_END_ITEM
    { inner $2 }
| HTML_Item text error
    { unclosed (html_open_to_string $1) 1
        "text" (html_close_to_string $1) 3 }
;

%%
