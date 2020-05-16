(** Parser for the contents of [<doc text="...">] annotations. *)

type inline_element = [
  | `Text of string
  | `Code of string
]

type block = [
  | `Paragraph of inline_element list
  | `Pre of string
]

type document = block list

val parse_string : string -> document
