(* This module is a recursive descent parser for the ocamldoc syntax. The parser
   consumes a token stream of type [Token.t Stream.t], provided by the lexer,
   and produces a comment AST of the type defined in [Parser_.Ast].

   The AST has two main levels: inline elements, which can appear inside
   paragraphs, and are spaced horizontally when presented, and block elements,
   such as paragraphs and lists, which are spaced vertically when presented.
   Block elements contain inline elements, but not vice versa.

   Corresponding to this, the parser has three "main" functions:

   - [delimited_inline_element_list] parses a run of inline elements that is
     delimited by curly brace markup ([{...}]).
   - [paragraph] parses a run of inline elements that make up a paragraph, and
     is not explicitly delimited with curly braces.
   - [block_element_list] parses a sequence of block elements. A comment is a
     sequence of block elements, so [block_element_list] is the top-level
     parser. It is also used for list item and tag content. *)



module Location = Odoc_model.Location_
module Error = Odoc_model.Error
module Comment = Odoc_model.Comment

type 'a with_location = 'a Location.with_location



(* {2 Input} *)

type input = {
  tokens : (Token.t Location.with_location) Stream.t;
  warnings : Error.warning_accumulator;
}

let junk input = Stream.junk input.tokens

let peek input =
  match Stream.peek input.tokens with
  | Some token -> token
  | None -> assert false
  (* The last token in the stream is always [`End], and it is never consumed by
     the parser, so the [None] case is impossible. *)

let npeek n input = Stream.npeek n input.tokens



(* {2 Non-link inline elements} *)

(* Convenient abbreviation for use in patterns. *)
type token_that_always_begins_an_inline_element = [
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Begin_style of Comment.style
  | `Simple_reference of string
  | `Begin_reference_with_replacement_text of string
  | `Simple_link of string
  | `Begin_link_with_replacement_text of string
]

(* Check that the token constructors above actually are all in [Token.t]. *)
let _check_subset : token_that_always_begins_an_inline_element -> Token.t =
  fun t -> (t :> Token.t)

(* Consumes tokens that make up a single non-link inline element:

   - a horizontal space ([`Space], significant in inline elements),
   - a word (see [word]),
   - a code span ([...], [`Code_span _]), or
   - styled text ({e ...}).

   The latter requires a recursive call to [delimited_inline_element_list],
   defined below.

   This should be part of [delimited_inline_element_list]; however, it is also
   called by function [paragraph]. As a result, it is factored out, and made
   mutually-recursive with [delimited_inline_element_list].

   This is called only when it is known that the first token in the list is the
   beginning of an inline element. In the case of [`Minus] and [`Plus], that
   means the caller has determined that they are not a list bullet (i.e., not
   the first non-whitespace tokens on their line).

   This function consumes exactly the tokens that make up the element. *)
let rec inline_element
    : input -> Location.span -> _ -> Ast.inline_element with_location =
    fun input location next_token ->

  match next_token with
  | `Space _ as token ->
    junk input;
    Location.at location token

  | `Word _ as token ->
    junk input;
    Location.at location token
    (* This is actually the same memory representation as the token, complete
       with location, and is probably the most common case. Perhaps the token
       can be reused somehow. The same is true of [`Space], [`Code_span]. *)

  | `Minus ->
    junk input;
    Location.at location (`Word "-")

  | `Plus ->
    junk input;
    Location.at location (`Word "+")

  | `Code_span c ->
    junk input;
    Location.at location (`Code_span c)

  | `Raw_markup (raw_markup_target, s) ->
    junk input;
    Location.at location (`Raw_markup (raw_markup_target, s))

  | `Begin_style s as parent_markup ->
    junk input;

    let requires_leading_whitespace =
      match s with
      | `Bold | `Italic | `Emphasis -> true
      | `Superscript | `Subscript -> false
    in
    let content, brace_location =
      delimited_inline_element_list
        ~parent_markup
        ~parent_markup_location:location
        ~requires_leading_whitespace
        input
    in

    let location = Location.span [location; brace_location] in

    if content = [] then
      Parse_error.should_not_be_empty
        ~what:(Token.describe parent_markup) location
      |> Error.warning input.warnings;

    Location.at location (`Styled (s, content))

  | `Simple_reference r ->
    junk input;

    let r_location = Location.nudge_start (String.length "{!") location in
    let r = Location.at r_location r in

    Location.at location (`Reference (`Simple, r, []))

  | `Begin_reference_with_replacement_text r as parent_markup ->
    junk input;

    let r_location = Location.nudge_start (String.length "{{!") location in
    let r = Location.at r_location r in

    let content, brace_location =
      delimited_inline_element_list
        ~parent_markup
        ~parent_markup_location:location
        ~requires_leading_whitespace:false
        input
    in

    let location = Location.span [location; brace_location] in

    if content = [] then
      Parse_error.should_not_be_empty
        ~what:(Token.describe parent_markup) location
      |> Error.warning input.warnings;

    Location.at location (`Reference (`With_text, r, content))

  | `Simple_link u ->
    junk input;

    let u = String.trim u in

    if u = "" then
      Parse_error.should_not_be_empty ~what:(Token.describe next_token) location
      |> Error.warning input.warnings;

    Location.at location (`Link (u, []))

  | `Begin_link_with_replacement_text u as parent_markup ->
    junk input;

    let u = String.trim u in

    if u = "" then
      Parse_error.should_not_be_empty
        ~what:(Token.describe parent_markup) location
      |> Error.warning input.warnings;

    let content, brace_location =
      delimited_inline_element_list
        ~parent_markup
        ~parent_markup_location:location
        ~requires_leading_whitespace:false
        input
    in

    `Link (u, content)
    |> Location.at (Location.span [location; brace_location])

(* Consumes tokens that make up a sequence of inline elements that is ended by
   a '}', a [`Right_brace] token. The brace token is also consumed.

   The sequences are also preceded by some markup like '{b'. Some of these
   markup tokens require whitespace immediately after the token, and others not.
   The caller indicates which way that is through the
   [~requires_leading_whitespace] argument.

   Whitespace is significant in inline element lists. In particular, "foo [bar]"
   is represented as [`Word "foo"; `Space; `Code_span "bar"], while "foo[bar]"
   is [`Word "foo"; `Code_span "bar"]. It doesn't matter how much whitespace is
   there, just whether it is present or not. Single newlines and horizontal
   space in any amount are allowed. Blank lines are not, as these are separators
   for {e block} elements.

   In correct input, the first and last elements emitted will not be [`Space],
   i.e. [`Space] appears only between other non-link inline elements. In
   incorrect input, there might be [`Space] followed immediately by something
   like an @author tag.

   The [~parent_markup] and [~parent_markup_location] arguments are used for
   generating error messages. *)
and delimited_inline_element_list
    : parent_markup:[< Token.t ] ->
      parent_markup_location:Location.span ->
      requires_leading_whitespace:bool ->
      input ->
        (Ast.inline_element with_location) list * Location.span =
    fun
      ~parent_markup
      ~parent_markup_location
      ~requires_leading_whitespace
      input ->

  (* [~at_start_of_line] is used to interpret [`Minus] and [`Plus]. These are
     word tokens if not the first non-whitespace tokens on their line. Then,
     they are allowed in a non-link element list. *)
  let rec consume_elements
      : at_start_of_line:bool -> (Ast.inline_element with_location) list ->
          (Ast.inline_element with_location) list * Location.span =
      fun ~at_start_of_line acc ->

    let next_token = peek input in
    match next_token.value with
    | `Right_brace ->
      junk input;
      List.rev acc, next_token.location

    (* The [`Space] token is not space at the beginning or end of line, because
       that is combined into [`Single_newline] or [`Blank_line] tokens. It is
       also not at the beginning of markup (after e.g. '{b'), because that is
       handled separately before calling
       [consume_non_link_inline_elements], and not immediately before '}',
       because that is combined into the [`Right_brace] token by the lexer. So,
       it is an internal space, and we want to add it to the non-link inline
       element list. *)
    | `Space _
    | #token_that_always_begins_an_inline_element as token ->
      let acc = (inline_element input next_token.location token)::acc in
      consume_elements ~at_start_of_line:false acc

    | `Single_newline ws ->
      junk input;
      let element = Location.same next_token (`Space ws) in
      consume_elements ~at_start_of_line:true (element::acc)

    | `Blank_line ws as blank ->
      Parse_error.not_allowed
        ~what:(Token.describe blank)
        ~in_what:(Token.describe parent_markup)
        next_token.location
      |> Error.warning input.warnings;

      junk input;
      let element = Location.same next_token (`Space ws) in
      consume_elements ~at_start_of_line:true (element::acc)

    | `Minus
    | `Plus as bullet ->
      if at_start_of_line then begin
        let suggestion =
          Printf.sprintf
            "move %s so it isn't the first thing on the line."
            (Token.print bullet)
        in
        Parse_error.not_allowed
          ~what:(Token.describe bullet)
          ~in_what:(Token.describe parent_markup)
          ~suggestion
          next_token.location
        |> Error.warning input.warnings
      end;

      let acc = (inline_element input next_token.location bullet)::acc in
      consume_elements ~at_start_of_line:false acc

    | other_token ->
      Parse_error.not_allowed
        ~what:(Token.describe other_token)
        ~in_what:(Token.describe parent_markup)
        next_token.location
      |> Error.warning input.warnings;

      let last_location =
        match acc with
        | last_token::_ -> last_token.location
        | [] -> parent_markup_location
      in

      List.rev acc, last_location
  in

  let first_token = peek input in
  match first_token.value with
  | `Space _ ->
    junk input;
    consume_elements ~at_start_of_line:false []
    (* [~at_start_of_line] is [false] here because the preceding token was some
       some markup like '{b', and we didn't move to the next line, so the next
       token will not be the first non-whitespace token on its line. *)

  | `Single_newline _ ->
    junk input;
    consume_elements ~at_start_of_line:true []

  | `Blank_line _ as blank ->
    (* In case the markup is immediately followed by a blank line, the error
       message printed by the catch-all case below can be confusing, as it will
       suggest that the markup must be followed by a newline (which it is). It
       just must not be followed by two newlines. To explain that clearly,
       handle that case specifically. *)
    Parse_error.not_allowed
      ~what:(Token.describe blank)
      ~in_what:(Token.describe parent_markup)
      first_token.location
    |> Error.warning input.warnings;

    junk input;
    consume_elements ~at_start_of_line:true []

  | `Right_brace ->
    junk input;
    [], first_token.location

  | _ ->
    if requires_leading_whitespace then begin
      Parse_error.should_be_followed_by_whitespace
        ~what:(Token.print parent_markup) parent_markup_location
      |> Error.warning input.warnings
    end;
    consume_elements ~at_start_of_line:false []



(* {2 Paragraphs} *)

(* Consumes tokens that make up a paragraph.

   A paragraph is a sequence of inline elements that ends on a blank line, or
   explicit block markup such as a verbatim block on a new line.

   Because of the significance of newlines, paragraphs are parsed line-by-line.
   The function [paragraph] is called only when the current token is the first
   non-whitespace token on its line, and begins an inline element. [paragraph]
   then parses a line of inline elements. Afterwards, it looks ahead to the next
   line. If that line also begins with an inline element, it parses that line,
   and so on. *)
let paragraph : input -> Ast.nestable_block_element with_location =
    fun input ->

  (* Parses a single line of a paragraph, consisting of inline elements. The
     only valid ways to end a paragraph line are with [`End], [`Single_newline],
     [`Blank_line], and [`Right_brace]. Everything else either belongs in the
     paragraph, or signifies an attempt to begin a block element inside a
     paragraph line, which is an error. These errors are caught elsewhere; the
     paragraph parser just stops. *)
  let rec paragraph_line
      : (Ast.inline_element with_location) list ->
          (Ast.inline_element with_location) list =
      fun acc ->

    let next_token = peek input in
    match next_token.value with
    | `Space _
    | `Minus
    | `Plus
    | #token_that_always_begins_an_inline_element as token ->
      let element = inline_element input next_token.location token in
      paragraph_line (element::acc)

    | _ ->
      acc
  in

  (* After each line is parsed, decides whether to parse more lines. *)
  let rec additional_lines
      : (Ast.inline_element with_location) list ->
          (Ast.inline_element with_location) list =
      fun acc ->

    match npeek 2 input with
    | {value = `Single_newline ws; location}::
      {value = #token_that_always_begins_an_inline_element; _}::_ ->
      junk input;
      let acc = (Location.at location (`Space ws))::acc in
      let acc = paragraph_line acc in
      additional_lines acc

    | _ ->
      List.rev acc
  in

  let elements = paragraph_line [] |> additional_lines in
  `Paragraph elements
  |> Location.at (Location.span (List.map Location.location elements))



(* {2 Block elements} *)

(* {3 Helper types} *)

(* The interpretation of tokens in the block parser depends on where on a line
   each token appears. The five possible "locations" are:

   - [`At_start_of_line], when only whitespace has been read on the current
     line.
   - [`After_tag], when a valid tag token, such as [@deprecated], has been read,
     and only whitespace has been read since.
   - [`After_shorthand_bullet], when a valid shorthand list item bullet, such as
     [-], has been read, and only whitespace has been read since.
   - [`After_explicit_list_bullet], when a valid explicit bullet, such as [{li],
     has been read, and only whitespace has been read since.
   - [`After_text], when any other valid non-whitespace token has already been
     read on the current line.

   Here are some examples of how this affects the interpretation of tokens:

   - A paragraph can start anywhere except [`After_text] (two paragraphs cannot
     be on the same line, but paragraphs can be nested in just about anything).
   - [`Minus] is interpreted as a list item bullet [`At_start_of_line],
     [`After_tag], and [`After_explicit_list_bullet].
   - Tags are only allowed [`At_start_of_line].

  To track the location accurately, the functions that make up the block parser
  pass explicit [where_in_line] values around and return them.

  In a few cases, [where_in_line] can be inferred from what helper was called.
  For example, the [paragraph] parser always stops on the same line as the last
  significant token that is in the paragraph it consumed, so the location must
  be [`After_text]. *)
type where_in_line = [
  | `At_start_of_line
  | `After_tag
  | `After_shorthand_bullet
  | `After_explicit_list_bullet
  | `After_text
]

(* The block parsing loop, function [block_element_list], stops when it
   encounters certain tokens.

   When it is called for the whole comment, or for in explicit list item
   ([{li foo}]), it can only stop on end of input or a right brace.

   When it is called inside a shorthand list item ([- foo]), it stops on end of
   input, right brace, a blank line (indicating end of shorthand list), plus or
   minus (indicating the start of the next liste item), or a section heading or
   tag, which cannot be nested in list markup.

   The block parser [block_element_list] explicitly returns the token that
   stopped it, with a type more precise than [Token.t stream_head]: if it was
   called for the whole comment or an explicit list item, the stop token will
   have type [stops_at_delimiters stream_head], and if it was called for a
   shorthand list item, the stop token will have type
   [implicit_stop stream_head]. This allows the calling parsers to write precise
   cases for exactly the tokens that might be at the front of the stream after
   the block parser returns. *)
type stops_at_delimiters = [
  | `End
  | `Right_brace
]

type stopped_implicitly = [
  | `End
  | `Blank_line of string
  | `Right_brace
  | `Minus
  | `Plus
  | Token.section_heading
  | Token.tag
]

(* Ensure that the above two types are really subsets of [Token.t]. *)
let _check_subset : stops_at_delimiters -> Token.t = fun t -> (t :> Token.t)
let _check_subset : stopped_implicitly -> Token.t = fun t -> (t :> Token.t)

(* The different contexts in which the block parser [block_element_list] can be
   called. The block parser's behavior depends somewhat on the context. For
   example, while paragraphs are allowed anywhere, shorthand lists are not
   allowed immediately inside other shorthand lists, while tags are not allowed
   anywhere except at the comment top level.

   Besides telling the block parser how to behave, each context also carries two
   types, which determine the return type of the block parser:

   - The type of blocks the parser returns. Note that [nestable_block_element]
     is included in [block_element]. However, the extra block kinds in
     [block_element] are only allowed at the comment top level.
   - The type of token that the block parser stops at. See discussion above. *)
type ('block, 'stops_at_which_tokens) context =
  | Top_level :
      (Ast.block_element, stops_at_delimiters) context
  | In_shorthand_list :
      (Ast.nestable_block_element, stopped_implicitly) context
  | In_explicit_list :
      (Ast.nestable_block_element, stops_at_delimiters) context
  | In_tag :
      (Ast.nestable_block_element, Token.t) context

(* This is a no-op. It is needed to prove to the type system that nestable block
   elements are acceptable block elements in all contexts. *)
let accepted_in_all_contexts
    : type block stops_at_which_tokens.
      (block, stops_at_which_tokens) context ->
      Ast.nestable_block_element ->
        block =
    fun context block ->
  match context with
  | Top_level -> (block :> Ast.block_element)
  | In_shorthand_list -> block
  | In_explicit_list -> block
  | In_tag -> block

(* Converts a tag to a series of words. This is used in error recovery, when a
   tag cannot be generated. *)
let tag_to_words = function
  | `Author s -> [`Word "@author"; `Space " "; `Word s]
  | `Before s -> [`Word "@before"; `Space " "; `Word s]
  | `Canonical s -> [`Word "@canonical"; `Space " "; `Word s]
  | `Deprecated -> [`Word "@deprecated"]
  | `Inline -> [`Word "@inline"]
  | `Open -> [`Word "@open"]
  | `Closed -> [`Word "@closed"]
  | `Param s -> [`Word "@param"; `Space " "; `Word s]
  | `Raise s -> [`Word "@raise"; `Space " "; `Word s]
  | `Return -> [`Word "@return"]
  | `See (`Document, s) -> [`Word "@see"; `Space " "; `Word ("\"" ^ s ^ "\"")]
  | `See (`File, s) -> [`Word "@see"; `Space " "; `Word ("'" ^ s ^ "'")]
  | `See (`Url, s) -> [`Word "@see"; `Space " "; `Word ("<" ^ s ^ ">")]
  | `Since s -> [`Word "@since"; `Space " "; `Word s]
  | `Version s -> [`Word "@version"; `Space " "; `Word s]

(* {3 Block element lists} *)

(* Consumes tokens making up a sequence of block elements. These are:

   - paragraphs,
   - code blocks,
   - verbatim text blocks,
   - lists, and
   - section headings. *)
let rec block_element_list
    : type block stops_at_which_tokens.
      (block, stops_at_which_tokens) context ->
      parent_markup:[< Token.t | `Comment ] ->
      input ->
        (block with_location) list *
        stops_at_which_tokens with_location *
        where_in_line =
    fun context ~parent_markup input ->

  let rec consume_block_elements
      : parsed_a_tag:bool ->
        where_in_line ->
        (block with_location) list ->
          (block with_location) list *
          stops_at_which_tokens with_location *
          where_in_line =
      fun ~parsed_a_tag where_in_line acc ->

    let describe token =
      match token with
      | #token_that_always_begins_an_inline_element -> "paragraph"
      | _ -> Token.describe token
    in

    let warn_if_after_text {Location.location; value = token} =
      if where_in_line = `After_text then
        Parse_error.should_begin_on_its_own_line ~what:(describe token) location
        |> Error.warning input.warnings
    in

    let warn_if_after_tags {Location.location; value = token} =
      if parsed_a_tag then
        let suggestion =
          Printf.sprintf
            "move %s before any tags." (Token.describe token)
        in
        Parse_error.not_allowed
          ~what:(describe token)
          ~in_what:"the tags section"
          ~suggestion
          location
        |> Error.warning input.warnings
    in

    let warn_because_not_at_top_level {Location.location; value = token} =
      let suggestion =
        Printf.sprintf
          "move %s outside of any other markup." (Token.print token)
      in
      Parse_error.not_allowed
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion
        location
      |> Error.warning input.warnings
    in



    match peek input with
    (* Terminators: the two tokens that terminate anything. *)
    | {value = `End; _}
    | {value = `Right_brace; _} as next_token ->
      (* This little absurdity is needed to satisfy the type system. Without it,
         OCaml is unable to prove that [stream_head] has the right type for all
         possible values of [context]. *)
      begin match context with
      | Top_level ->
        List.rev acc, next_token, where_in_line
      | In_shorthand_list ->
        List.rev acc, next_token, where_in_line
      | In_explicit_list ->
        List.rev acc, next_token, where_in_line
      | In_tag ->
        List.rev acc, next_token, where_in_line
      end



    (* Whitespace. This can terminate some kinds of block elements. It is also
       necessary to track it to interpret [`Minus] and [`Plus] correctly, as
       well as to ensure that all block elements begin on their own line. *)
    | {value = `Space _; _} ->
      junk input;
      consume_block_elements ~parsed_a_tag where_in_line acc

    | {value = `Single_newline _; _} ->
      junk input;
      consume_block_elements ~parsed_a_tag `At_start_of_line acc

    | {value = `Blank_line _; _} as next_token ->
      begin match context with
      (* Blank lines terminate shorthand lists ([- foo]). They also terminate
         paragraphs, but the paragraph parser is aware of that internally. *)
      | In_shorthand_list ->
        List.rev acc, next_token, where_in_line
      (* Otherwise, blank lines are pretty much like single newlines. *)
      | _ ->
        junk input;
        consume_block_elements ~parsed_a_tag `At_start_of_line acc
      end



    (* Explicit list items ([{li ...}] and [{- ...}]) can never appear directly
       in block content. They can only appear inside [{ul ...}] and [{ol ...}].
       So, catch those. *)
    | {value = `Begin_list_item _ as token; location} ->
      let suggestion =
        Printf.sprintf
          "move %s into %s, or use %s."
          (Token.print token)
          (Token.describe (`Begin_list `Unordered))
          (Token.describe (`Minus))
      in
      Parse_error.not_allowed
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion
        location
      |> Error.warning input.warnings;

      junk input;
      consume_block_elements ~parsed_a_tag where_in_line acc



    (* Tags. These can appear at the top level only. Also, once one tag is seen,
       the only top-level elements allowed are more tags. *)
    | {value = `Tag tag as token; location} as next_token ->
      let recover_when_not_at_top_level context =
        warn_because_not_at_top_level next_token;
        junk input;
        let words = List.map (Location.at location) (tag_to_words tag) in
        let paragraph =
          `Paragraph words
          |> accepted_in_all_contexts context
          |> Location.at location
        in
        consume_block_elements ~parsed_a_tag `At_start_of_line (paragraph::acc)
      in

      begin match context with
      (* Tags cannot make sense in an explicit list ([{ul {li ...}}]). *)
      | In_explicit_list ->
        recover_when_not_at_top_level context
      (* If a tag starts at the beginning of a line, it terminates the preceding
         tag and/or the current shorthand list. In this case, return to the
         caller, and let the caller decide how to interpret the tag token. *)
      | In_shorthand_list ->
        if where_in_line = `At_start_of_line then
          List.rev acc, next_token, where_in_line
        else
          recover_when_not_at_top_level context
      | In_tag ->
        if where_in_line = `At_start_of_line then
          List.rev acc, next_token, where_in_line
        else
          recover_when_not_at_top_level context

      (* If this is the top-level call to [block_element_list], parse the
         tag. *)
      | Top_level ->
        if where_in_line <> `At_start_of_line then
          Parse_error.should_begin_on_its_own_line
            ~what:(Token.describe token) location
          |> Error.warning input.warnings;

        junk input;

        begin match tag with
        | `Author s | `Since s | `Version s | `Canonical s as tag ->
          let s = String.trim s in
          if s = "" then
            Parse_error.should_not_be_empty
              ~what:(Token.describe token) location
            |> Error.warning input.warnings;
          let tag =
            match tag with
            | `Author _ -> `Author s
            | `Since _ -> `Since s
            | `Version _ -> `Version s
            | `Canonical _ ->
              (* TODO The location is only approximate, as we need lexer
                 cooperation to get the real location. *)
              let r_location =
                Location.nudge_start (String.length "@canonical ") location in
              `Canonical (Location.at r_location s)
          in

          let tag = Location.at location (`Tag tag) in
          consume_block_elements ~parsed_a_tag:true `After_text (tag::acc)

        | `Deprecated | `Return as tag ->
          let content, _stream_head, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let tag =
            match tag with
            | `Deprecated -> `Deprecated content
            | `Return -> `Return content
          in
          let location =
            location::(List.map Location.location content)
            |> Location.span
          in
          let tag = Location.at location (`Tag tag) in
          consume_block_elements ~parsed_a_tag:true where_in_line (tag::acc)

        | `Param _ | `Raise _ | `Before _ as tag ->
          let content, _stream_head, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let tag =
            match tag with
            | `Param s -> `Param (s, content)
            | `Raise s -> `Raise (s, content)
            | `Before s -> `Before (s, content)
          in
          let location =
            location::(List.map Location.location content)
            |> Location.span
          in
          let tag = Location.at location (`Tag tag) in
          consume_block_elements ~parsed_a_tag:true where_in_line (tag::acc)

        | `See (kind, target) ->
          let content, _next_token, where_in_line =
            block_element_list In_tag ~parent_markup:token input in
          let location =
            location::(List.map Location.location content)
            |> Location.span
          in
          let tag = `Tag (`See (kind, target, content)) in
          let tag = Location.at location tag in
          consume_block_elements ~parsed_a_tag:true where_in_line (tag::acc)

        | `Inline | `Open | `Closed as tag ->
          let tag = Location.at location (`Tag tag) in
          consume_block_elements ~parsed_a_tag:true `After_text (tag::acc)
        end
      end



    | {value = #token_that_always_begins_an_inline_element; _} as next_token ->
      warn_if_after_tags next_token;
      warn_if_after_text next_token;

      let block = paragraph input in
      let block =
        Odoc_model.Location_.map (accepted_in_all_contexts context) block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_tag `After_text acc

    | {value = `Code_block s | `Verbatim s as token; location} as next_token ->
      warn_if_after_tags next_token;
      warn_if_after_text next_token;
      if s = "" then
        Parse_error.should_not_be_empty ~what:(Token.describe token) location
        |> Error.warning input.warnings;

      junk input;
      let block =
        match token with
        | `Code_block _ -> `Code_block s
        | `Verbatim _ -> `Verbatim s
      in
      let block = accepted_in_all_contexts context block in
      let block = Location.at location block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_tag `After_text acc

    | {value = `Modules s as token; location} as next_token ->
      warn_if_after_tags next_token;
      warn_if_after_text next_token;

      junk input;

      (* TODO Use some library for a splitting function, or move this out into a
         Util module. *)
      let split_string delimiters s =
        let rec scan_delimiters acc index =
          if index >= String.length s then
            List.rev acc
          else
            if String.contains delimiters s.[index] then
              scan_delimiters acc (index + 1)
            else
              scan_word acc index (index + 1)

        and scan_word acc start_index index =
          if index >= String.length s then
            let word = String.sub s start_index (index - start_index) in
            List.rev (word::acc)
          else
            if String.contains delimiters s.[index] then
              let word = String.sub s start_index (index - start_index) in
              scan_delimiters (word::acc) (index + 1)
            else
              scan_word acc start_index (index + 1)

        in

        scan_delimiters [] 0
      in

      (* TODO Correct locations await a full implementation of {!modules}
         parsing. *)
      let modules =
        split_string " \t\r\n" s
        |> List.map (fun r -> Location.at location r)
      in

      if modules = [] then
        Parse_error.should_not_be_empty ~what:(Token.describe token) location
        |> Error.warning input.warnings;

      let block = accepted_in_all_contexts context (`Modules modules) in
      let block = Location.at location block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_tag `After_text acc



    | {value = `Begin_list kind as token; location} as next_token ->
      warn_if_after_tags next_token;
      warn_if_after_text next_token;

      junk input;

      let items, brace_location =
        explicit_list_items ~parent_markup:token input in
      if items = [] then
        Parse_error.should_not_be_empty ~what:(Token.describe token) location
        |> Error.warning input.warnings;

      let location = Location.span [location; brace_location] in
      let block = `List (kind, `Heavy, items) in
      let block = accepted_in_all_contexts context block in
      let block = Location.at location block in
      let acc = block::acc in
      consume_block_elements ~parsed_a_tag `After_text acc



    | {value = `Minus | `Plus as token; location} as next_token ->
      begin match where_in_line with
      | `After_text | `After_shorthand_bullet ->
        Parse_error.should_begin_on_its_own_line
          ~what:(Token.describe token) location
        |> Error.warning input.warnings
      | _ ->
        ()
      end;

      warn_if_after_tags next_token;

      begin match context with
      | In_shorthand_list ->
        List.rev acc, next_token, where_in_line
      | _ ->
        let items, where_in_line =
          shorthand_list_items next_token where_in_line input in
        let kind =
          match token with
          | `Minus -> `Unordered
          | `Plus -> `Ordered
        in
        let location =
          location::(List.map Location.location (List.flatten items))
          |> Location.span
        in
        let block = `List (kind, `Light, items) in
        let block = accepted_in_all_contexts context block in
        let block = Location.at location block in
        let acc = block::acc in
        consume_block_elements ~parsed_a_tag where_in_line acc
      end



    | {value = `Begin_section_heading (level, label) as token; location}
        as next_token ->

      warn_if_after_tags next_token;

      let recover_when_not_at_top_level context =
        warn_because_not_at_top_level next_token;
        junk input;
        let content, brace_location =
          delimited_inline_element_list
            ~parent_markup:token
            ~parent_markup_location:location
            ~requires_leading_whitespace:true
            input
        in
        let location = Location.span [location; brace_location] in
        let paragraph =
          `Paragraph content
          |> accepted_in_all_contexts context
          |> Location.at location
        in
        consume_block_elements ~parsed_a_tag `At_start_of_line (paragraph::acc)
      in

      begin match context with
      | In_shorthand_list ->
        if where_in_line = `At_start_of_line then
          List.rev acc, next_token, where_in_line
        else
          recover_when_not_at_top_level context
      | In_explicit_list ->
        recover_when_not_at_top_level context
      | In_tag ->
        recover_when_not_at_top_level context

      | Top_level ->
        if where_in_line <> `At_start_of_line then
          Parse_error.should_begin_on_its_own_line
            ~what:(Token.describe token) location
          |> Error.warning input.warnings;

        let label =
          match label with
          | Some "" ->
            Parse_error.should_not_be_empty ~what:"heading label" location
            |> Error.warning input.warnings;
            None
          | _ ->
            label
        in

        junk input;

        let content, brace_location =
          delimited_inline_element_list
            ~parent_markup:token
            ~parent_markup_location:location
            ~requires_leading_whitespace:true
            input
        in
        if content = [] then
          Parse_error.should_not_be_empty ~what:(Token.describe token) location
          |> Error.warning input.warnings;

        let location = Location.span [location; brace_location] in
        let heading = `Heading (level, label, content) in
        let heading = Location.at location heading in
        let acc = heading::acc in
        consume_block_elements ~parsed_a_tag `After_text acc
      end
  in

  let where_in_line =
    match context with
    | Top_level -> `At_start_of_line
    | In_shorthand_list -> `After_shorthand_bullet
    | In_explicit_list -> `After_explicit_list_bullet
    | In_tag -> `After_tag
  in

  consume_block_elements ~parsed_a_tag:false where_in_line []

(* {3 Lists} *)

(* Consumes a sequence of implicit list items. Each one consists of a [`Minus]
   or [`Plus] token, followed by block elements until:

   - a blank line, or
   - a list bullet of the opposite kind (e.g. [`Plus] for a [`Minus] list).

   This function is called when the next token is known to be [`Minus] or
   [`Plus]. It consumes that token, and calls the block element parser (see
   above). That parser returns to [implicit_list_items] only on [`Blank_line],
   [`End], [`Minus] or [`Plus] at the start of a line, or [`Right_brace]. *)
and shorthand_list_items
    : [ `Minus | `Plus ] with_location ->
      where_in_line ->
      input ->
        ((Ast.nestable_block_element with_location) list) list *
        where_in_line =
    fun first_token where_in_line input ->

  let bullet_token = first_token.value in

  let rec consume_list_items
      : [> ] with_location ->
        where_in_line ->
        ((Ast.nestable_block_element with_location) list) list ->
          ((Ast.nestable_block_element with_location) list) list *
          where_in_line =
      fun next_token where_in_line acc ->

    match next_token.value with
    | `End
    | `Right_brace
    | `Blank_line _
    | `Tag _
    | `Begin_section_heading _ ->
      List.rev acc, where_in_line

    | `Minus
    | `Plus as bullet ->
      if bullet = bullet_token then begin
        junk input;

        let content, stream_head, where_in_line =
          block_element_list In_shorthand_list ~parent_markup:bullet input in
        if content = [] then
          Parse_error.should_not_be_empty
            ~what:(Token.describe bullet) next_token.location
          |> Error.warning input.warnings;

        let acc = content::acc in
        consume_list_items stream_head where_in_line acc
      end
      else
        List.rev acc, where_in_line
  in

  consume_list_items
    (first_token :> stopped_implicitly with_location) where_in_line []

(* Consumes a sequence of explicit list items (starting with '{li ...}' and
   '{-...}', which are represented by [`Begin_list_item _] tokens).

   This function is called immediately after '{ul' or '{ol' ([`Begin_list _]) is
   read. The only "valid" way to exit is by reading a [`Right_brace] token,
   which is consumed.

   Whitespace inside the list, but outside list items, is not significant – this
   parsing function consumes all of it. Otherwise, only list item start tokens
   are accepted. Everything else is an error. *)
and explicit_list_items
    : parent_markup:[< Token.t ] ->
      input ->
        ((Ast.nestable_block_element with_location) list) list *
        Location.span =
    fun ~parent_markup input ->

  let rec consume_list_items
      : ((Ast.nestable_block_element with_location) list) list ->
          ((Ast.nestable_block_element with_location) list) list *
          Location.span =
      fun acc ->

    let next_token = peek input in
    match next_token.value with
    | `End ->
      Parse_error.not_allowed
        next_token.location
        ~what:(Token.describe `End)
        ~in_what:(Token.describe parent_markup)
      |> Error.warning input.warnings;
      List.rev acc, next_token.location

    | `Right_brace ->
      junk input;
      List.rev acc, next_token.location

    | `Space _
    | `Single_newline _
    | `Blank_line _ ->
      junk input;
      consume_list_items acc

    | `Begin_list_item kind as token ->
      junk input;

      (* '{li', represented by [`Begin_list_item `Li], must be followed by
         whitespace. *)
      if kind = `Li then begin
        match (peek input).value with
        | `Space _ | `Single_newline _ | `Blank_line _ | `Right_brace ->
          ()
          (* The presence of [`Right_brace] above requires some explanation:

             - It is better to be silent about missing whitespace if the next
               token is [`Right_brace], because the error about an empty list
               item will be generated below, and that error is more important to
               the user.
             - The [`Right_brace] token also happens to include all whitespace
               before it, as a convenience for the rest of the parser. As a
               result, not ignoring it could be wrong: there could in fact be
               whitespace in the concrete syntax immediately after '{li', just
               it is not represented as [`Space], [`Single_newline], or
               [`Blank_line]. *)
        | _ ->
          Parse_error.should_be_followed_by_whitespace
            next_token.location ~what:(Token.print token)
          |> Error.warning input.warnings
      end;

      let content, token_after_list_item, _where_in_line =
        block_element_list In_explicit_list ~parent_markup:token input in

      if content = [] then
        Parse_error.should_not_be_empty
          next_token.location ~what:(Token.describe token)
        |> Error.warning input.warnings;

      begin match token_after_list_item.value with
      | `Right_brace ->
        junk input
      | `End ->
        Parse_error.not_allowed
          token_after_list_item.location
          ~what:(Token.describe `End)
          ~in_what:(Token.describe token)
        |> Error.warning input.warnings
      end;

      let acc = content::acc in
      consume_list_items acc

    | token ->
      let suggestion =
        match token with
        | `Begin_section_heading _ | `Tag _ ->
          Printf.sprintf "move %s outside the list." (Token.describe token)
        | _ ->
          Printf.sprintf
            "move %s into a list item, %s or %s."
            (Token.describe token)
            (Token.print (`Begin_list_item `Li))
            (Token.print (`Begin_list_item `Dash))
      in
      Parse_error.not_allowed
        next_token.location
        ~what:(Token.describe token)
        ~in_what:(Token.describe parent_markup)
        ~suggestion
      |> Error.warning input.warnings;

      junk input;
      consume_list_items acc
  in

  consume_list_items []



(* {2 Entry point} *)

let parse warnings tokens =
  let input = {tokens; warnings} in

  let rec parse_block_elements () =
    let elements, last_token, _where_in_line =
      block_element_list
        Top_level ~parent_markup:`Comment input
    in

    match last_token.value with
    | `End ->
      elements

    | `Right_brace ->
      Parse_error.unpaired_right_brace last_token.location
      |> Error.warning input.warnings;

      let block =
        Location.same last_token
          (`Paragraph [Location.same last_token (`Word "}")])
      in

      junk input;
      elements @ (block::(parse_block_elements ()))
  in
  parse_block_elements ()
