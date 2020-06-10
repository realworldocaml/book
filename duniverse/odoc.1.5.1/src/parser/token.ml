(* This module contains the token type, emitted by the lexer, and consumed by
   the comment syntax parser. It also contains two functions that format tokens
   for error messages. *)



type section_heading = [
  `Begin_section_heading of int * string option
]

type tag = [
  | `Tag of [
    | `Author of string
    | `Deprecated
    | `Param of string
    | `Raise of string
    | `Return
    | `See of [ `Url | `File | `Document ] * string
    | `Since of string
    | `Before of string
    | `Version of string
    | `Canonical of string
    | `Inline
    | `Open
    | `Closed
  ]
]

type t = [
  (* End of input. *)
  | `End

  (* Runs of whitespace. [Blank_line] is any run of whitespace that contains two
     or more newline characters. [Single_newline] is any run of whitespace that
     contains exactly one newline character. [Space] is any run of whitespace
     that contains no newline characters.

     It is an important invariant in the parser that no adjacent whitespace
     tokens are emitted by the lexer. Otherwise, there would be the need for
     unbounded lookahead, a (co-?)ambiguity between
     [Single_newline Single_newline] and [Blank_line], and other problems. *)
  | `Space of string
  | `Single_newline of string
  | `Blank_line of string

  (* A right curly brace ([}]), i.e. end of markup. *)
  | `Right_brace

  (* Words are anything that is not whitespace or markup. Markup symbols can be
     be part of words if escaped.

     Words can contain plus and minus symbols, but those are emitted as [Plus]
     and [Minus] tokens. The parser combines plus and minus into words, except
     when they appear first on a line, in which case the tokens are list item
     bullets. *)
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Begin_style of Odoc_model.Comment.style

  (* Other inline element markup. *)
  | `Simple_reference of string
  | `Begin_reference_with_replacement_text of string
  | `Simple_link of string
  | `Begin_link_with_replacement_text of string

  (* Leaf block element markup. *)
  | `Code_block of string
  | `Verbatim of string
  | `Modules of string

  (* List markup. *)
  | `Begin_list of [ `Unordered | `Ordered ]
  | `Begin_list_item of [ `Li | `Dash ]
  | `Minus
  | `Plus

  | section_heading
  | tag
]



let print : [< t ] -> string = function
  | `Begin_style `Bold ->
    "'{b'"
  | `Begin_style `Italic ->
    "'{i'"
  | `Begin_style `Emphasis ->
    "'{e'"
  | `Begin_style `Superscript ->
    "'{^'"
  | `Begin_style `Subscript ->
    "'{_'"
  | `Begin_reference_with_replacement_text _ ->
    "'{{!'"
  | `Begin_link_with_replacement_text _ ->
    "'{{:'"
  | `Begin_list_item `Li ->
    "'{li ...}'"
  | `Begin_list_item `Dash ->
    "'{- ...}'"
  | `Minus ->
    "'-'"
  | `Plus ->
    "'+'"
  | `Begin_section_heading (level, label) ->
    let label =
      match label with
      | None -> ""
      | Some label -> ":" ^ label
    in
    Printf.sprintf "'{%i%s'" level label
  | `Tag (`Author _) ->
    "'@author'"
  | `Tag `Deprecated ->
    "'@deprecated'"
  | `Tag (`Param _) ->
    "'@param'"
  | `Tag (`Raise _) ->
    "'@raise'"
  | `Tag `Return ->
    "'@return'"
  | `Tag (`See _) ->
    "'@see'"
  | `Tag (`Since _) ->
    "'@since'"
  | `Tag (`Before _) ->
    "'@before'"
  | `Tag (`Version _) ->
    "'@version'"
  | `Tag (`Canonical _) ->
    "'@canonical'"
  | `Tag `Inline ->
    "'@inline'"
  | `Tag `Open ->
    "'@open'"
  | `Tag `Closed ->
    "'@closed'"
  | `Raw_markup (None, _) ->
    "'{%...%}'"
  | `Raw_markup (Some target, _) ->
    "'{%" ^ target ^ ":...%}'"

(* [`Minus] and [`Plus] are interpreted as if they start list items. Therefore,
   for error messages based on [Token.describe] to be accurate, formatted
   [`Minus] and [`Plus] should always be plausibly list item bullets. *)
let describe : [< t | `Comment ] -> string = function
  | `Word w ->
    Printf.sprintf "'%s'" w
  | `Code_span _ ->
    "'[...]' (code)"
  | `Raw_markup _ ->
    "'{%...%}' (raw markup)"
  | `Begin_style `Bold ->
    "'{b ...}' (boldface text)"
  | `Begin_style `Italic ->
    "'{i ...}' (italic text)"
  | `Begin_style `Emphasis ->
    "'{e ...}' (emphasized text)"
  | `Begin_style `Superscript ->
    "'{^...}' (superscript)"
  | `Begin_style `Subscript ->
    "'{_...}' (subscript)"
  | `Simple_reference _ ->
    "'{!...}' (cross-reference)"
  | `Begin_reference_with_replacement_text _ ->
    "'{{!...} ...}' (cross-reference)"
  | `Simple_link _ ->
    "'{:...} (external link)'"
  | `Begin_link_with_replacement_text _ ->
    "'{{:...} ...}' (external link)"
  | `End ->
    "end of text"
  | `Space _ ->
    "whitespace"
  | `Single_newline _ ->
    "line break"
  | `Blank_line _ ->
    "blank line"
  | `Right_brace ->
    "'}'"
  | `Code_block _ ->
    "'{[...]}' (code block)"
  | `Verbatim _ ->
    "'{v ... v}' (verbatim text)"
  | `Modules _ ->
    "'{!modules ...}'"
  | `Begin_list `Unordered ->
    "'{ul ...}' (bulleted list)"
  | `Begin_list `Ordered ->
    "'{ol ...}' (numbered list)"
  | `Begin_list_item `Li ->
    "'{li ...}' (list item)"
  | `Begin_list_item `Dash ->
    "'{- ...}' (list item)"
  | `Minus ->
    "'-' (bulleted list item)"
  | `Plus ->
    "'+' (numbered list item)"
  | `Begin_section_heading (level, _) ->
    Printf.sprintf "'{%i ...}' (section heading)" level
  | `Tag (`Author _) ->
    "'@author'"
  | `Tag `Deprecated ->
    "'@deprecated'"
  | `Tag (`Param _) ->
    "'@param'"
  | `Tag (`Raise _) ->
    "'@raise'"
  | `Tag `Return ->
    "'@return'"
  | `Tag (`See _) ->
    "'@see'"
  | `Tag (`Since _) ->
    "'@since'"
  | `Tag (`Before _) ->
    "'@before'"
  | `Tag (`Version _) ->
    "'@version'"
  | `Tag (`Canonical _) ->
    "'@canonical'"
  | `Tag `Inline ->
    "'@inline'"
  | `Tag `Open ->
    "'@open'"
  | `Tag `Closed ->
    "'@closed'"
  | `Comment ->
    "top-level text"
