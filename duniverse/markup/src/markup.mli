(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(** Error-recovering streaming HTML and XML parsers and writers.

    Markup.ml is an HTML and XML parsing and serialization library. It:

    - Is error-recovering, so you can get a best-effort parse of malformed
      input.
    - Reports all errors before recovery, so you can get strict parsing
      instead.
    - Conforms closely to the XML grammar and HTML parser from the respective
      specifications.
    - Accepts document fragments, but can be told to accept only full documents.
    - Detects character encodings automatically.
    - Supports both simple synchronous (this module) and non-blocking usage
      ({!Markup_lwt}).
    - Is streaming and lazy. Partial input is processed as soon as received, but
      only as needed.
    - Does one pass over the input and emits a stream of SAX-style parsing
      signals. A helper ({!tree}) allows that to be easily converted into
      DOM-style trees.

    The usage is straightforward. For example:

{[
open Markup

(* Correct and pretty-print HTML. *)
channel stdin
|> parse_html |> signals |> pretty_print
|> write_html |> to_channel stdout

(* Show up to 10 XML well-formedness errors to the user. Stop after
   the 10th, without reading more input. *)
let report =
  let count = ref 0 in
  fun location error ->
    error |> Error.to_string ~location |> prerr_endline;
    count := !count + 1;
    if !count >= 10 then raise_notrace Exit

string "some xml" |> parse_xml ~report |> signals |> drain

(* Load HTML into a custom document tree data type. *)
type html = Text of string | Element of string * html list

file "some_file"
|> fst
|> parse_html
|> signals
|> tree
  ~text:(fun ss -> Text (String.concat "" ss))
  ~element:(fun (_, name) _ children -> Element (name, children))
]}

    The interface is centered around four functions. In pseudocode:

{[
val parse_html : char stream   -> signal stream
val write_html : signal stream -> char stream
val parse_xml  : char stream   -> signal stream
val write_xml  : signal stream -> char stream
]}

    Most of the remaining functions create streams from, or write streams to,
    strings, files, and channels, or manipulate streams, such as {!next} and the
    combinators {!map} and {!fold}.

    Apart from this module, Markup.ml provides two other top-level modules:

    {!modules:Markup_lwt Markup_lwt_unix}

    Most of the interface of {!Markup_lwt} is specified in signature
    {!ASYNCHRONOUS}, which will be shared with a [Markup_async] module, should
    it be implemented.

    Markup.ml is developed on {{:https://github.com/aantron/markup.ml} GitHub}
    and distributed under the
    {{:https://github.com/aantron/markup.ml/blob/master/LICENSE.md} BSD
    license}. This documentation is for version 0.8.1 of the library.
    Documentation for older versions can be found on the
    {{: https://github.com/aantron/markup.ml/releases} releases page}. *)



(** {2 Streams} *)

type async
type sync
(** Phantom types for use with [('a, 's) stream] in place of ['s]. See
    explanation below. *)

type ('a, 's) stream
(** Streams of elements of type ['a].

    In simple usage, when using only this module [Markup], the additional type
    parameter ['s] is always [sync], and there is no need to consider it
    further.

    However, if you are using {!Markup_lwt}, you may create some [async]
    streams. The difference between the two is that {!next} on a [sync] stream
    retrieves an element before {!next} "returns," while {!next} on an [async]
    stream might not retrieve an element until later. As a result, it is not
    safe to pass an [async] stream where a [sync] stream is required. The
    phantom types are used to make the type checker catch such errors at compile
    time. *)



(** {2 Errors}

    The parsers recover from errors automatically. If that is sufficient, you
    can ignore this section. However, if you want stricter behavior, or need to
    debug parser output, use optional argument [?report] of the parsers, and
    look in module {!Error}. *)

type location = int * int
(** Line and column for parsing errors. Both numbers are one-based. *)

(** Error type and [to_string] function. *)
module Error :
sig
  type t =
    [ `Decoding_error of string * string
    | `Bad_token of string * string * string
    | `Unexpected_eoi of string
    | `Bad_document of string
    | `Unmatched_start_tag of string
    | `Unmatched_end_tag of string
    | `Bad_namespace of string
    | `Misnested_tag of string * string
    | `Bad_content of string ]
  (** Errors reported by the parsers. A few of these are also used by the
      writers.

      - [`Decoding_error (bytes, encoding)] is reported by the decoders in
        module {! Encoding}. For example, if the UTF-8 decoder encounters a bare
        [0xA0] byte, it will report [`Decoding_error ("\xA0", "utf-8")].

      - [`Bad_token (token, where, s)] is reported when there is a "local"
        problem with the syntax of the input stream, such as an invalid
        character or a duplicate attribute. For example, if the XML parser
        detects a [&] that is not part of an entity reference while reading an
        attribute, it will report
        [`Bad_token ("&", "attribute", "replace with '&amp;'")]

      - [`Unexpected_eoi where] is reported by the parsers when the input ends
        before an item, such as a tag, element, or comment, is closed. [where]
        describes the kind of item that wasn't closed.

      - [`Bad_document s] is reported by the parsers when there is a problem
        with the top-level structure of the document. For example, if you are
        parsing an input stream as XML with [~context:`Document], and the parser
        finds an element after the root element, it will report
        [`Bad_document "not allowed after root element"].

      - [`Unmatched_start_tag name] and [`Unmatched_end_tag name] are reported
        when tags aren't properly balanced. Note that not all unbalanced tags
        are parse errors in HTML.

      - [`Bad_namespace s] is reported by parsers when the prefix [s] can't be
        resolved to a namespace, and by the writers when the namespace [s] can't
        be resolved to a prefix (or the default namespace).

      - [`Misnested_tag (what, where)] is reported by the HTML parser when a tag
        appears where it is not allowed. For example, if the input has a
        [<body>] tag inside a [<p>] tag, the parser will report
        [`Misnested_tag ("body", "p")].

      - [`Bad_content where] is reported by the HTML parser if an element has
        content it is not allowed to have. For example, if there is stray text
        at the top level of a [<table>] element, the parser will report
        [`Bad_content "table"].
   *)

  val to_string : ?location:location -> t -> string
  (** Converts an error to human-readable form. If [~location] is specified,
      location information is prepended to the output. *)
end



(** {2 Encodings}

    The parsers detect encodings automatically. If you need to specify an
    encoding, use optional argument [?encoding] of the parsers, and look in
    module {!Encoding}. *)

(** Common Internet encodings such as UTF-8 and UTF-16; also includes some less
    popular encodings that are sometimes used for XML. *)
module Encoding :
sig
  type t
  (** Decoders. These are notionally maps from byte streams to Unicode scalar
      value streams, i.e. pseudocode type [char stream -> int stream]. *)

  val decode :
    ?report:(location -> Error.t -> unit) -> t ->
    (char, 's) stream -> (int, 's) stream
  (** Applies a decoder to a byte stream. Illegal input byte sequences result in
      calls to the error handler [~report] with error kind [`Decoding_error].
      The illegal bytes are then skipped, and zero or more U+FFFD replacement
      characters are emitted. The default handler ignores errors.

      The locations provided to the error handler by the built-in decoders below
      in this module are fully accurate only if the input byte stream uses LF
      characters as line breaks. *)

  val utf_8 : t
  val utf_16be : t
  val utf_16le : t
  val utf_16 : t
  val iso_8859_1 : t
  val us_ascii : t
  val windows_1251 : t
  val windows_1252 : t
  val ucs_4be : t
  val ucs_4le : t
  val ucs_4be_transposed : t
  val ucs_4le_transposed : t
  val ebcdic : t
  (** Code page 37. *)
end



(** {2 Signals} *)

type name = string * string
(** Expanded name: a namespace URI followed by a local name. *)

type xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}
(** Representation of an XML declaration, i.e.
    [<?xml version="1.0" encoding="utf-8"?>]. *)

type doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}
(** Representation of a document type declaration. The HTML parser fills in all
    fields besides [raw_text]. The XML parser reads declarations roughly, and
    fills only the [raw_text] field with the text found in the declaration. *)

type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string ]
(** Parsing signals. The parsers emit them according to the following grammar:

{[
doc     ::= `Xml? misc* `Doctype? misc* element misc*
misc    ::= `PI | `Comment
element ::= `Start_element content* `End_element
content ::= `Text | element | `PI | `Comment
]}

    As a result, emitted [`Start_element] and [`End_element] signals are always
    balanced, and, if there is an XML declaration, it is the first signal.

    If parsing with [~context:`Document], the signal sequence will match the
    [doc] production until the first error. If parsing with
    [~context:`Fragment], it will match [content*]. If [~context] is not
    specified, the parser will pick one of the two by examining the input.

    As an example, if the XML parser is parsing

{[
<?xml version="1.0"?><root>text<nested>more text</nested></root>
]}

    it will emit the signal sequence

{[
`Xml {version = "1.0"; encoding = None; standalone = None}
`Start_element (("", "root"), [])
`Text ["text"]
`Start_element (("", "nested"), [])
`Text ["more text"]
`End_element
`End_element
]}

    The [`Text] signal carries a [string list] instead of a single [string]
    because on 32-bit platforms, OCaml strings cannot be larger than 16MB. In
    case the parsers encounter a very long sequence of text, one whose length
    exceeds about [Sys.max_string_length / 2], they will emit a [`Text] signal
    with several strings. *)

type content_signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list ]
(** A restriction of type {!signal} to only elements and text, i.e. no comments,
    processing instructions, or declarations. This can be useful for pattern
    matching in applications that only care about the content and element
    structure of a document. See the helper {!content}. *)

val signal_to_string : [< signal ] -> string
(** Provides a human-readable representation of signals for debugging. *)



(** {2 Parsers} *)

type 's parser
(** An ['s parser] is a thin wrapper around a [(signal, 's) stream] that
    supports access to additional information that is not carried directly in
    the stream, such as source locations. *)

val signals : 's parser -> (signal, 's) stream
(** Converts a parser to its underlying signal stream. *)

val location : _ parser -> location
(** Evaluates to the location of the last signal emitted on the parser's signal
    stream. If no signals have yet been emitted, evaluates to [(1, 1)]. *)



(** {2 XML} *)

val parse_xml :
  ?report:(location -> Error.t -> unit) ->
  ?encoding:Encoding.t ->
  ?namespace:(string -> string option) ->
  ?entity:(string -> string option) ->
  ?context:[< `Document | `Fragment ] ->
  (char, 's) stream -> 's parser
(** Creates a parser that converts an XML byte stream to a signal stream.

    For simple usage, [string "foo" |> parse_xml |> signals].

    If [~report] is provided, [report] is called for every error encountered.
    You may raise an exception in [report], and it will propagate to the code
    reading the signal stream.

    If [~encoding] is {e not} specified, the parser detects the input encoding
    automatically. Otherwise, the given encoding is used.

    [~namespace] is called when the parser is unable to resolve a namespace
    prefix. If it evaluates to [Some s], the parser maps the prefix to [s].
    Otherwise, the parser reports [`Bad_namespace].

    [~entity] is called when the parser is unable to resolve an entity
    reference. If it evaluates to [Some s], the parser inserts [s] into the
    text or attribute being parsed without any further parsing of [s]. [s] is
    assumed to be encoded in UTF-8. If [entity] evaluates to [None] instead,
    the parser reports [`Bad_token]. See {!xhtml_entity} if you are parsing
    XHTML.

    The meaning of [~context] is described at {! signal}, above. *)

val write_xml :
  ?report:((signal * int) -> Error.t -> unit) ->
  ?prefix:(string -> string option) ->
  ([< signal ], 's) stream -> (char, 's) stream
(** Converts an XML signal stream to a byte stream.

    If [~report] is provided, it is called for every error encountered. The
    first argument is a pair of the signal causing the error and its index in
    the signal stream. You may raise an exception in [report], and it will
    propagate to the code reading the byte stream.

    [~prefix] is called when the writer is unable to find a prefix in scope
    for a namespace URI. If it evaluates to [Some s], the writer uses [s] for
    the URI. Otherwise, the writer reports [`Bad_namespace]. *)



(** {2 HTML} *)

val parse_html :
  ?report:(location -> Error.t -> unit) ->
  ?encoding:Encoding.t ->
  ?context:[< `Document | `Fragment of string ] ->
  (char, 's) stream -> 's parser
(** Similar to {!parse_xml}, but parses HTML with embedded SVG and MathML, never
    emits signals [`Xml] or [`PI], and [~context] has a different type on tag
    [`Fragment].

    For HTML fragments, you should specify the enclosing element, e.g.
    [`Fragment "body"]. This is because, when parsing HTML, error recovery and
    the interpretation of text depend on the current element. For example, the
    text

{[
foo</bar>
]}

    parses differently in [title] elements than in [p] elements. In the former,
    it is parsed as [foo</bar>], while in the latter, it is [foo] followed by a
    parse error due to unmatched tag [</bar>]. To get these behaviors, set
    [~context] to [`Fragment "title"] and [`Fragment "p"], respectively.

    If you use [`Fragment "svg"], the fragment is assumed to be SVG markup.
    Likewise, [`Fragment "math"] causes the parser to parse MathML markup.

    If [~context] is omitted, the parser guesses it from the input stream. For
    example, if the first signal would be [`Doctype], the context is set to
    [`Document], but if the first signal would be [`Start_element "td"], the
    context is set to [`Fragment "tr"]. If the first signal would be
    [`Start_element "g"], the context is set to [`Fragment "svg"].

 *)

val write_html :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  ([< signal ], 's) stream -> (char, 's) stream
(** Similar to {!write_xml}, but emits HTML5 instead of XML.
    If [~escape_attribute] and/or [~escape_text] are provided,
    they are used instead of default escaping functions.
*)



(** {2 Input sources} *)

val string : string -> (char, sync) stream
(** Evaluates to a stream that retrieves successive bytes from the given
    string. *)

val buffer : Buffer.t -> (char, sync) stream
(** Evaluates to a stream that retrieves successive bytes from the given buffer.
    Be careful of changing the buffer while it is being iterated by the
    stream. *)

val channel : in_channel -> (char, sync) stream
(** Evaluates to a stream that retrieves bytes from the given channel. If the
    channel cannot be read, the next read of the stream results in raising
    [Sys_error].

    Note that this input source is synchronous because [Pervasives.in_channel]
    reads are blocking. For non-blocking channels, see {!Markup_lwt_unix}. *)

val file : string -> (char, sync) stream * (unit -> unit)
(** [file path] opens the file at [path], then evaluates to a pair [s, close],
    where reading from stream [s] retrieves successive bytes from the file, and
    calling [close ()] closes the file.

    The file is closed automatically if [s] is read to completion, or if reading
    [s] raises an exception. It is not necessary to call [close ()] in these
    cases.

    If the file cannot be opened, raises [Sys_error] immediately. If the file
    cannot be read, reading the stream raises [Sys_error]. *)

val fn : (unit -> char option) -> (char, sync) stream
(** [fn f] is a stream that retrives bytes by calling [f ()]. If the call
    results in [Some c], the stream emits [c]. If the call results in [None],
    the stream is considered to have ended.

    This is actually an alias for {!stream}, restricted to type [char]. *)



(** {2 Output destinations} *)

val to_string : (char, sync) stream -> string
(** Eagerly retrieves bytes from the given stream and assembles a string. *)

val to_buffer : (char, sync) stream -> Buffer.t
(** Eagerly retrieves bytes from the given stream and places them into a
    buffer. *)

val to_channel : out_channel -> (char, sync) stream -> unit
(** Eagerly retrieves bytes from the given stream and writes them to the given
    channel. If writing fails, raises [Sys_error]. *)

val to_file : string -> (char, sync) stream -> unit
(** Eagerly retrieves bytes from the given stream and writes them to the given
    file. If writing fails, or the file cannot be opened, raises [Sys_error].
    Note that the file is truncated (cleared) before writing. If you wish to
    append to file, open it with the appropriate flags and use [to_channel] on
    the resulting channel. *)



(** {2 Stream operations} *)

val stream : (unit -> 'a option) -> ('a, sync) stream
(** [stream f] creates a stream that repeatedly calls [f ()]. Each time [f ()]
    evaluates to [Some v], the next item in the stream is [v]. The first time
    [f ()] evaluates to [None], the stream ends. *)

val next : ('a, sync) stream -> 'a option
(** Retrieves the next item in the stream, if any, and removes it from the
    stream. *)

val peek : ('a, sync) stream -> 'a option
(** Retrieves the next item in the stream, if any, but does not remove the item
    from the stream. *)

val transform :
  ('a -> 'b -> 'c list * 'a option) -> 'a -> ('b, 's) stream -> ('c, 's) stream
(** [transform f init s] lazily creates a stream by repeatedly applying
    [f acc v], where [acc] is an accumulator whose initial value is [init], and
    [v] is consecutive values of [s]. Each time, [f acc v] evaluates to a pair
    [(vs, maybe_acc')]. The values [vs] are added to the result stream. If
    [maybe_acc'] is [Some acc'], the accumulator is set to [acc']. Otherwise, if
    [maybe_acc'] is [None], the result stream ends. *)

val fold : ('a -> 'b -> 'a) -> 'a -> ('b, sync) stream -> 'a
(** [fold f init s] eagerly folds over the items [v], [v'], [v''], ... of [s],
    i.e. evaluates [f (f (f init v) v') v'']... *)

val map : ('a -> 'b) -> ('a, 's) stream -> ('b, 's) stream
(** [map f s] lazily applies [f] to each item of [s], and produces the resulting
    stream. *)

val filter : ('a -> bool) -> ('a, 's) stream -> ('a, 's) stream
(** [filter f s] is [s] without the items for which [f] evaluates to [false].
    [filter] is lazy. *)

val filter_map : ('a -> 'b option) -> ('a, 's) stream -> ('b, 's) stream
(** [filter_map f s] lazily applies [f] to each item [v] of [s]. If [f v]
    evaluates to [Some v'], the result stream has [v']. If [f v] evaluates to
    [None], no item corresponding to [v] appears in the result stream. *)

val iter : ('a -> unit) -> ('a, sync) stream -> unit
(** [iter f s] eagerly applies [f] to each item of [s], i.e. evaluates
    [f v; f v'; f v'']... *)

val drain : ('a, sync) stream -> unit
(** [drain s] eagerly consumes [s]. This is useful for observing side effects,
    such as parsing errors, when you don't care about the parsing signals
    themselves. It is equivalent to [iter ignore s]. *)

val of_list : 'a list -> ('a, sync) stream
(** Produces a (lazy) stream from the given list. *)

val to_list : ('a, sync) stream -> 'a list
(** Eagerly converts the given stream to a list. *)



(** {2 Utility} *)

val content : ([< signal ], 's) stream -> (content_signal, 's) stream
(** Converts a {!signal} stream into a {!content_signal} stream by filtering out
    all signals besides [`Start_element], [`End_element], and [`Text]. *)

val tree :
  ?text:(string list -> 'a) ->
  ?element:(name -> (name * string) list -> 'a list -> 'a) ->
  ?comment:(string -> 'a) ->
  ?pi:(string -> string -> 'a) ->
  ?xml:(xml_declaration -> 'a) ->
  ?doctype:(doctype -> 'a) ->
  ([< signal ], sync) stream -> 'a option
(** This function's type signature may look intimidating, but it is actually
    easy to use. It is best introduced by example:

{[
type my_dom = Text of string | Element of name * my_dom list

"<p>HTML5 is <em>easy</em> to parse"
|> string
|> parse_html
|> signals
|> tree
  ~text:(fun ss -> Text (String.concat "" ss))
  ~element:(fun (name, _) children -> Element (name, children))
]}

    results in the structure

{[
Element ("p" [
  Text "HTML5 is ";
  Element ("em", [Text "easy"]);
  Text " to parse"])
]}

    Formally, [tree] assembles a tree data structure of type ['a] from a signal
    stream. The stream is parsed according to the following grammar:

{[
stream  ::= node*
node    ::= element | `Text | `Comment | `PI | `Xml | `Doctype
element ::= `Start_element node* `End_element
]}

    Each time [trees] matches a production of [node], it calls the corresponding
    function to convert the node into your tree type ['a]. For example, when
    [trees] matches [`Text ss], it calls [~text ss], if [~text] is supplied.
    Similarly, when [trees] matches [element], it calls
    [~element name attributes children], if [~element] is supplied.

    See {!trees} if the input stream might have multiple top-level trees. This
    function [tree] only retrieves the first one. *)

val trees :
  ?text:(string list -> 'a) ->
  ?element:(name -> (name * string) list -> 'a list -> 'a) ->
  ?comment:(string -> 'a) ->
  ?pi:(string -> string -> 'a) ->
  ?xml:(xml_declaration -> 'a) ->
  ?doctype:(doctype -> 'a) ->
  ([< signal ], 's) stream -> ('a, 's) stream
(** Like {!tree}, but converts all top-level trees, not only the first one. The
    trees are emitted on the resulting stream, in the sequence that they appear
    in the input. *)

type 'a node =
  [ `Element of name * (name * string) list * 'a list
  | `Text of string
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string ]
(** See {!from_tree} below. *)

val from_tree : ('a -> 'a node) -> 'a -> (signal, sync) stream
(** Deconstructs tree data structures of type ['a] into signal streams. The
    function argument is applied to each data structure node. For example,

{[
type my_dom = Text of string | Element of string * my_dom list

let dom =
  Element ("p", [
    Text "HTML5 is ";
    Element ("em", [Text "easy"]);
    Text " to parse"])

dom |> from_tree (function
  | Text s -> `Text s
  | Element (name, children) -> `Element (("", name), [], children))
]}

    results in the signal stream

{[
`Start_element (("", "p"), [])
`Text ["HTML5 is "]
`Start_element (("", "em"), [])
`Text ["easy"]
`End_element
`Text " to parse"
`End_element
]} *)

val elements :
  (name -> (name * string) list -> bool) ->
  ([< signal ] as 'a, 's) stream ->
    (('a, 's) stream, 's) stream
(** [elements f s] scans the signal stream [s] for
    [`Start_element (name, attributes)] signals that satisfy
    [f name attributes]. Each such matching signal is the beginning of a
    substream that ends with the corresponding [`End_element] signal. The result
    of [elements f s] is the stream of these substreams.

    Matches don't nest. If there is a matching element contained in another
    matching element, only the top one results in a substream.

    Code using [elements] does not have to read each substream to completion, or
    at all. However, once the using code has tried to get the next substream, it
    should not try to read a previous one. *)

val text : ([< signal ], 's) stream -> (char, 's) stream
(** Extracts all the text in a signal stream by discarding all markup. For each
    [`Text ss] signal, the result stream has the bytes of the strings [ss], and
    all other signals are ignored. *)

val trim : ([> content_signal ] as 'a, 's) stream -> ('a, 's) stream
(** Trims insignificant whitespace in an HTML signal stream. Whitespace around
    flow ("block") content does not matter, but whitespace in phrasing
    ("inline") content does. So, if the input stream is

{[
<div>
 <p>
  <em>foo</em> bar
 </p>
</div>
]}

    passing it through [Markup.trim] will result in

{[
<div><p><em>foo</em> bar</p></div>
]}

    Note that whitespace around the [</em>] tag was preserved. *)

val normalize_text :
  ([> `Text of string list ] as 'a, 's) stream -> ('a, 's) stream
(** Concatenates adjacent [`Text] signals, then eliminates all empty strings,
    then all [`Text []] signals. Signals besides [`Text] are unaffected. Note
    that signal streams emitted by the parsers already have normalized text.
    This function is useful when you are inserting text into a signal stream
    after parsing, or generating streams from scratch, and would like to clean
    up the [`Text] signals. *)

val pretty_print : ([> content_signal ] as 'a, 's) stream -> ('a, 's) stream
(** Adjusts the whitespace in the [`Text] signals in the given stream so that
    the output appears nicely-indented when the stream is converted to bytes and
    written.

    This function is aware of the significance of whitespace in HTML, so it
    avoids changing the whitespace in phrasing ("inline") content. For example,
    pretty printing

{[
<div><p><em>foo</em>bar</p></div>
]}

    results in

{[
<div>
 <p>
  <em>foo</em>bar
 </p>
</div>
]}

    Note that no whitespace was inserted around [<em>] and [</em>], because
    doing so would create a word break that wasn't present in the original
    stream. *)

val html5 : ([< signal ], 's) stream -> (signal, 's) stream
(** Converts a signal stream into an HTML5 signal stream by stripping any
    document type declarations, XML declarations, and processing instructions,
    and prefixing the HTML5 doctype declaration. This is useful when converting
    between XHTML and HTML. *)

val xhtml :
  ?dtd:[< `Strict_1_0 | `Transitional_1_0 | `Frameset_1_0 | `Strict_1_1 ] ->
  ([< signal ], 's) stream -> (signal, 's) stream
(** Similar to {!html5}, but does not strip processing instructions, and
    prefixes an XHTML document type declaration and an XML declaration. The
    [~dtd] argument specifies which DTD to refer to in the doctype declaration.
    The default is [`Strict_1_1]. *)

val xhtml_entity : string -> string option
(** Translates XHTML entities. This function is for use with the [~entity]
    argument of {!parse_xml} when parsing XHTML. *)

val strings_to_bytes : (string, 's) stream -> (char, 's) stream
(** [strings_to_bytes s] is the stream of all the bytes of all strings in
    [s]. *)

val compare_locations : location -> location -> int
(** Orders locations according to their appearance in an input stream, i.e.
    first by line, and then, for locations on the same line, by column. *)



(** {2 Namespaces} *)

(** Common namespace URIs. *)
module Ns :
sig
  val html : string
  (** [http://www.w3.org/1999/xhtml]. Use for HTML and XHTML. *)

  val svg : string
  (** [http://www.w3.org/2000/svg]. *)

  val mathml : string
  (** [http://www.w3.org/1998/Math/MathML]. *)

  val xml : string
  (** [http://www.w3.org/XML/1998/namespace]. *)

  val xmlns : string
  (** [http://www.w3.org/2000/xmlns/]. *)

  val xlink : string
  (** [http://www.w3.org/1999/xlink]. *)
end



(** {2 Asynchronous interface} *)

(**/**)

module type IO =
sig
  type 'a t

  val return : 'a -> 'a t
  val of_cps : ((exn -> unit) -> ('a -> unit) -> unit) -> 'a t
  val to_cps : (unit -> 'a t) -> ((exn -> unit) -> ('a -> unit) -> unit)
end

(**/**)

(** Markup.ml interface for monadic I/O libraries such as Lwt and Async.

    This signature is implemented by {!Markup_lwt}, with a few additions.

    Each function here corresponds directly to the function in the basic module
    {!Markup} that has the same name. So, see {!Markup} for details.

    The only difference is that functions here, all of which are higher-order
    functions, take a function as argument that returns an ['a io] promise,
    rather than returning an already-computed value. *)
module type ASYNCHRONOUS =
sig
  (** {2 Promises} *)

  type 'a io
  (** Promise type. Replaced by ['a Lwt.t] in {!Markup_lwt}. *)

  (** {2 Encodings} *)

  (** Asynchronous counterpart to {!Markup.Encoding}. *)
  module Encoding :
  sig
    (**/**)
    type t = Encoding.t
    (**/**)

    val decode :
      ?report:(location -> Error.t -> unit io) -> Encoding.t ->
      (char, _) stream -> (int, async) stream
  end

  (** {2 XML} *)

  val parse_xml :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, _) stream -> async parser

  val write_xml :
    ?report:((signal * int) -> Error.t -> unit io) ->
    ?prefix:(string -> string option) ->
    ([< signal ], _) stream -> (char, async) stream

  (** {2 HTML} *)

  val parse_html :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?context:[< `Document | `Fragment of string ] ->
    (char, _) stream -> async parser

  val write_html :
    ?escape_attribute:(string -> string) ->
    ?escape_text:(string -> string) ->
    ([< signal ], _) stream -> (char, async) stream

  (** {2 I/O} *)

  val fn : (unit -> char option io) -> (char, async) stream

  val to_string : (char, _) stream -> string io
  val to_buffer : (char, _) stream -> Buffer.t io

  (** {2 Stream manipulation} *)

  val stream : (unit -> 'a option io) -> ('a, async) stream

  val next : ('a, _) stream -> 'a option io
  val peek : ('a, _) stream -> 'a option io

  val transform :
    ('a -> 'b -> ('c list * 'a option) io) -> 'a -> ('b, _) stream ->
      ('c, async) stream
  val fold : ('a -> 'b -> 'a io) -> 'a -> ('b, _) stream -> 'a io
  val map : ('a -> 'b io) -> ('a, _) stream -> ('b, async) stream
  val filter : ('a -> bool io) -> ('a, _) stream -> ('a, async) stream
  val filter_map : ('a -> 'b option io) -> ('a, _) stream -> ('b, async) stream
  val iter : ('a -> unit io) -> ('a, _) stream -> unit io
  val drain : ('a, _) stream -> unit io

  val to_list : ('a, _) stream -> 'a list io

  val load : ('a, _) stream -> ('a, sync) stream io
  (** [load s] converts a general stream [s] to a synchronous stream by
      buffering it. *)

  (** {2 Utility} *)

  val tree :
    ?text:(string list -> 'a) ->
    ?element:(name -> (name * string) list -> 'a list -> 'a) ->
    ?comment:(string -> 'a) ->
    ?pi:(string -> string -> 'a) ->
    ?xml:(xml_declaration -> 'a) ->
    ?doctype:(doctype -> 'a) ->
    ([< signal ], _) stream -> 'a option io
end

(**/**)

module Asynchronous (IO : IO) : ASYNCHRONOUS with type 'a io := 'a IO.t

val kstream : ('a, _) stream -> 'a Kstream.t
val of_kstream : 'a Kstream.t -> ('a, _) stream

val preprocess_input_stream :
  (int, 's) stream -> (location * int, 's) stream * (unit -> location)

(**/**)



(** {2 Conformance status}

    The HTML parser seeks to implement
    {{:https://www.w3.org/TR/html5/syntax.html} section 8 of the HTML5
    specification}. That section describes a parser, part of a full-blown user
    agent, that is building up a DOM representation of an HTML document.
    Markup.ml is neither inherently part of a user agent, nor does it build up a
    DOM representation. With respect to section 8 of HTML5, Markup.ml is
    concerned with only the syntax. When that section requires that the user
    agent perform an action, Markup.ml emits enough information for a
    hypothetical user agent based on it to be able to decide to perform this
    action. Likewise, Markup.ml seeks to emit enough information for a
    hypothetical user agent to build up a conforming DOM.

    The XML parser seeks to be a non-validating implementation of the
    {{:https://www.w3.org/TR/xml/} XML} and {{:https://www.w3.org/TR/xml-names/}
    Namespaces in XML} specifications.

    This rest of this section lists known deviations from HTML5, XML, and
    Namespaces in XML. Some of these deviations are meant to be corrected in
    future versions of Markup.ml, while others will probably remain. The latter
    satisfy some or all of the following properties:

    - They require non-local adjustment, especially of past nodes. For example,
      adjusting the start signal of the root node mid-way through the signal
      stream is difficult for a one-pass parser.
    - They are minor. Users implementing less than a conforming browser
      typically don't care about them. They typically have to do with obscure
      error recovery. There are no deviations affecting the parsing of
      well-formed input.
    - They can easily be corrected by code written over Markup.ml that builds up
      a DOM or maintains other auxiliary data structures during parsing.

    {3 To be corrected:}

    - XML: There is no attribute value normalization.
    - HTML: {e foster parenting} is not implemented, because it requires
      non-local adjustments.
    - HTML: Quirks mode is not honored. This affects the interaction between
      automatic closing of [p] elements and opening of [table] elements.
    - HTML: The parser has non-standard recovery from unmatched closing [form]
      tags in {{: https://github.com/aantron/markup.ml/commit/0bf4f1b} some
      situations}.
    - HTML: The parser ignores interactions between [form] and [template].
    - HTML: The form translation for [isindex] is completely ignored. [isindex]
      is handled as an unknown element.

    {3 To remain:}

    - HTML: Except when detecting encodings, the parser does not try to read
      [<meta>] tags for encoding declarations. The user of Markup.ml should read
      these, if necessary. They are part of the emitted signal stream.
    - HTML: [noscript] elements are always parsed, as are [script] elements. For
      conforming behavior, if the user of Markup.ml "supports scripts," the user
      should serialize the content of [noscript] to a [`Text] signal using
      [write_html].
    - HTML: Elements such as [title] that belong in [head], but are found
      between [head] and [body], are not moved into [head].
    - HTML: [<html>] tags found in the body do not have their attributes added
      to the [`Start_element "html"] signal emitted at the beginning of the
      document. *)
