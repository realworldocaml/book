let capitalize_ascii = Astring.String.Ascii.capitalize

let bad_markup : ?suggestion:string -> string -> Loc.span -> Warning.t =
 fun ?suggestion -> Warning.make ?suggestion "'%s': bad markup."

let leading_zero_in_heading_level : string -> Loc.span -> Warning.t =
  Warning.make "'%s': leading zero in heading level."

let should_not_be_empty : what:string -> Loc.span -> Warning.t =
 fun ~what -> Warning.make "%s should not be empty." (capitalize_ascii what)

let markup_should_not_be_used : what:string -> Loc.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should not be used because it has no effect."
    (capitalize_ascii what)

let should_begin_on_its_own_line : what:string -> Loc.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should begin on its own line." (capitalize_ascii what)

let should_be_followed_by_whitespace : what:string -> Loc.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should be followed by space, a tab, or a new line."
    (capitalize_ascii what)

let not_allowed :
    ?suggestion:string -> what:string -> in_what:string -> Loc.span -> Warning.t
    =
 fun ?suggestion ~what ~in_what ->
  Warning.make ?suggestion "%s is not allowed in %s." (capitalize_ascii what)
    in_what

let no_leading_whitespace_in_verbatim : Loc.span -> Warning.t =
  Warning.make "'{v' should be followed by whitespace."

let no_trailing_whitespace_in_verbatim : Loc.span -> Warning.t =
  Warning.make "'v}' should be preceded by whitespace."

let stray_at : Loc.span -> Warning.t = Warning.make "Stray '@'."

let stray_cr : Loc.span -> Warning.t =
  Warning.make "Stray '\\r' (carriage return character)."

let truncated_before : Loc.span -> Warning.t =
  Warning.make "'@before' expects version number on the same line."

let truncated_param : Loc.span -> Warning.t =
  Warning.make "'@param' expects parameter name on the same line."

let truncated_raise : string -> Loc.span -> Warning.t =
  Warning.make "'%s' expects exception constructor on the same line."

let truncated_see : Loc.span -> Warning.t =
  Warning.make
    "'@see' should be followed by <url>, 'file', or \"document title\"."

let unknown_tag : string -> Loc.span -> Warning.t =
  Warning.make "Unknown tag '%s'."

let unpaired_right_brace : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '\\}'." "Unpaired '}' (end of markup)."

let unpaired_right_bracket : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '\\]'." "Unpaired ']' (end of code)."

let no_language_tag_in_meta : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '{[ ... ]}' or '{@ocaml[ ... ]}'."
    "'{@' should be followed by a language tag."

let language_tag_invalid_char lang_tag : char -> Loc.span -> Warning.t =
  let suggestion = "try '{@" ^ lang_tag ^ "[ ... ]}'." in
  Warning.make ~suggestion "Invalid character '%c' in language tag."

let truncated_code_block_meta : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '{@ocaml[ ... ]}'." "Missing end of code block."

let truncated_code_block : Loc.span -> Warning.t =
  Warning.make ~suggestion:"add ']}'." "Missing end of code block."
