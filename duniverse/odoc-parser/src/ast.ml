(** Abstract syntax tree representing ocamldoc comments *)

(** This is a syntactic representation of ocamldoc comments. See
    {{:https://ocaml.org/releases/4.12/htmlman/ocamldoc.html}The manual} for a detailed
    description of the syntax understood. Note that there is no attempt at semantic
    analysis, and hence these types are capable of representing values that will
    be rejected by further stages, for example, invalid references or headings that
    are out of range. *)

type 'a with_location = 'a Loc.with_location
type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

type reference_kind = [ `Simple | `With_text ]
(** References in doc comments can be of two kinds: [{!simple}] or [{{!ref}With text}]. *)

type inline_element =
  [ `Space of string
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Styled of style * inline_element with_location list
  | `Reference of
    reference_kind * string with_location * inline_element with_location list
  | `Link of string * inline_element with_location list
  | `Math_span of string  (** @since 2.0.0 *) ]
(** Inline elements are equivalent to what would be found in a [span] in HTML.
    Mostly these are straightforward. The [`Reference] constructor takes a triple
    whose second element is the reference itself, and the third the replacement
    text. Similarly the [`Link] constructor has the link itself as first parameter
    and the second is the replacement text. *)

type nestable_block_element =
  [ `Paragraph of inline_element with_location list
  | `Code_block of
    (string with_location * string with_location option) option
    * string with_location
    (* [(language tag * metadata option) option * content] *)
  | `Verbatim of string
  | `Modules of string with_location list
  | `List of
    [ `Unordered | `Ordered ]
    * [ `Light | `Heavy ]
    * nestable_block_element with_location list list
  | `Math_block of string  (** @since 2.0.0 *) ]
(** Some block elements may be nested within lists or tags, but not all.
    The [`List] constructor has a parameter of type [\[`Light | `Heavy\]].
    This corresponds to the syntactic constructor used (see the
    {{:https://ocaml.org/releases/4.12/htmlman/ocamldoc.html#sss:ocamldoc-list}manual}).
    *)

type internal_tag =
  [ `Canonical of string with_location | `Inline | `Open | `Closed ]
(** Internal tags are used to exercise fine control over the output of odoc. They
    are never rendered in the output *)

type ocamldoc_tag =
  [ `Author of string
  | `Deprecated of nestable_block_element with_location list
  | `Param of string * nestable_block_element with_location list
  | `Raise of string * nestable_block_element with_location list
  | `Return of nestable_block_element with_location list
  | `See of
    [ `Url | `File | `Document ]
    * string
    * nestable_block_element with_location list
  | `Since of string
  | `Before of string * nestable_block_element with_location list
  | `Version of string ]
(** ocamldoc tags are those that are specified in the {{:https://ocaml.org/releases/4.12/htmlman/ocamldoc.html#ss:ocamldoc-tags}manual}) *)

type tag = [ ocamldoc_tag | internal_tag ]
type heading = int * string option * inline_element with_location list

type block_element =
  [ nestable_block_element | `Heading of heading | `Tag of tag ]

type t = block_element with_location list
