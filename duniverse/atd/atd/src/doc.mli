(**
   Support for <doc text="..."> annotations:

   type foo = [ Bar of int ] <doc text="This type represents foo values.">

   This allows code generators to inject the documentation into the
   generated code.

   <doc> nodes that appear in the following positions should be taken into
   account by code generators that care about documentation:

   - after the type name on the left-hand side of a type definition
   - after record field names
   - after variant names
   - DEPRECATED: after the type expression on the right-hand side of a type
     definition (but not after any type expression)

   Formats:

   Currently only one format called "text" is supported:
   - Blank lines separate paragraphs.
   - [\{\{ \}\}] can be used to enclose inline verbatim text.
   - [\{\{\{ \}\}\}] can be used to enclose verbatim text where whitespace
    is preserved.
   - The backslash character is used to escape special character sequences.
    In regular paragraph mode the special sequences are [\ ], [\{\{]
    and [\{\{\{].
    In inline verbatim text, special sequences are [\ ] and [\}\}].
    In verbatim text, special sequences are [\ ] and [\}\}\}].

   Character encoding: UTF-8 is strongly recommended, if not plain ASCII.
*)

type inline =
  | Text of string
  | Code of string
  (** [Text] is regular text. [Code] is text that was enclosed
      within [\{\{ \}\}] and should be rendered using the
      same fixed-width font used in all verbatim text. *)

type block =
  | Paragraph of inline list
  | Pre of string
  (** [Paragraph] is a regular paragraph.
      [Pre] is preformatted text that was enclosed
      within [\{\{\{ \}\}\}] and should be rendered using a fixed-width
      font preserving all space and newline characters. *)

(** A document is a list of paragraph-like blocks. *)
type doc = block list

(** Parse the contents of a doc.text annotation. *)
val parse_text : Ast.loc -> string -> doc

(** Print documentation in ATD's "text" format.
    This performs whitespace normalization, i.e. some non-significant
    whitespace is removed and newlines are inserted if needed
    to ensure that [{{{] and [}}}] are on their own line. *)
val print_text : doc -> string

(** This is for checking the placement of <doc ...> annotations. *)
val annot_schema : Annot.schema

(** Get and parse doc data from annotations. *)
val get_doc : Ast.loc -> Ast.annot -> doc option

(** Convert parsed doc into HTML. *)
val html_of_doc : doc -> string
