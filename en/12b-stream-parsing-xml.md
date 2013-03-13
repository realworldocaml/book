# XML Streams and Trees

XML is a markup language designed to store tree-structured data in a format
that is (somewhat) human- and machine-readable. Like JSON, it is a textual
format commonly used in web technologies, with a complete
[specification](http://www.w3.org/TR/REC-xml/) available online.

We're going to explain the basics of XML manipulation here, and also introduce
the notion of a _visitor pattern_ to manipulate fragments of XML trees.

<note>
<title>Obtaining and installing XMLM</title>

The remainder of this chapter uses the freely available XMLM library.  It's
easiest to obtain it via OPAM.  See [xref](#packaging-and-build-systems) for
installation instructions if you don't have OPAM.

```ocaml
$ opam install xmlm
```

Once installed, the `xmlm` library will be available in your toplevel.

```ocaml
$ utop
# #require "xmlm";;
# open Xmlm ;;
```

The library documentation is also available [online](http://erratique.ch/software/xmlm/doc/Xmlm).

</note>

Since XML is such a common web format, we've taken our example document from
the [DuckDuckGo](http://duckduckgo.com) search engine. This is a smaller search
engine than the usual suspects, but has the advantage of a freely available API
that doesn't require you to register before using it.  We'll talk more about
how to use the live API later in [xref](#concurrent-programming-with-async),
but for now here's what a shortened XML search response from DuckDuckGo looks
like:

```ocaml
<DuckDuckGoResponse version="1.0">
<Heading>DuckDuckGo</Heading>
<AbstractText>DuckDuckGo is an Internet search engine.</AbstractText>
<AbstractURL>https://en.wikipedia.org/wiki/DuckDuckGo</AbstractURL>
<AbstractSource>Wikipedia</AbstractSource>
<Results>
<Result>
  <Text>Official site</Text>
  <FirstURL>https://duckduckgo.com/</FirstURL>
  </Result>
</Results>
<RelatedTopics>
 <RelatedTopic>
   <Text>Companies based in Pennsylvania</Text>
   <FirstURL>
     http://duckduckgo.com/c/Companies_based_in_Pennsylvania
   </FirstURL>
 </RelatedTopic>
 <RelatedTopic>
   <Text>Internet search engines</Text>
   <FirstURL>
     http://duckduckgo.com/c/Internet_search_engines
   </FirstURL>
 </RelatedTopic>
</RelatedTopics>
</DuckDuckGoResponse>
```

The XML document is structured as a set of opening `<tag>` tokens that are closed by a
corresponding end `</tag>` token.  Opening tags can have an optional set of key/value
attributes, for example `<tag name="foo" id="bar">`.
A tag usually contains data that can contain further tags, thus forming a tree
structure.

These XML documents can be very large, and we don't want to have to read it all into
memory before starting to process it.
Luckily there exists a low-level _streaming_ interface that parses an XML document incrementally.
This can be cumbersome to use for quick tasks, so we'll build a simpler tree API on top of it.
We'll start with the streaming API first though.

### Stream parsing XML

The XMLM documentation is a good place to read about the overall layout of the
library.  It tells us that:

> A well-formed sequence of `signal`s represents an XML document tree traversal
> in depth-first order. Input pulls a well-formed sequence of `signal`s from a
> data source and output pushes a well-formed sequence of `signal`s to a data
> destination. Functions are provided to easily transform sequences of
> `signal`s to/from arborescent data structures.

The `signal` type is at the heart of all XMLM functions, so let's look at its type definition first:

```ocaml
type signal = [
  | `Data of string
  | `Dtd of dtd
  | `El_end
  | `El_start of tag
]
```

XMLM parses input XML documents into an ordered sequence of these `signal`s.
The first `signal` that's received when parsing an XML document is always a `Dtd`.
The Document Type Description (DTD) optionally defines which tags are
allowed within the XML document.  Some XML parsers can validate a document
against a DTD, but XMLM is a _non-validating_ parser that reads the DTD if
present, but disregards its contents.

The `El_start` and `El_end` signals indicate the opening and closing of tags,
and `Data` passes the data contained between tags.

Let's take a shot at handling signals by writing the identity function that
parses some XML and outputs it unmodified. There is no buffering required
since this uses the XMLM streaming API directly:

```
let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) ;;
let o = Xmlm.make_output (`Channel stdout) ;;
```

Let's start at the bottom, where we open up input and output channels to pass
to `Xmlm` parser.  The `make_input` and `make_output` functions use a
polymorphic variant to define the mechanism that the library should use to read
and write the XML.  `Channel` is used above, but there are several others
defined in the library:

```ocaml
type source = [
  | `Channel of in_channel
  | `Fun of unit -> int
  | `String of int * string
]
```

Each of these sources uses a different strategy for obtaining input data:

* `Channel` uses the OCaml standard library channel system. When more data is
required, a blocking read is performed on that channel.
* `String` accepts the whole document as an OCaml `string`, starting from an
integer offset and continuing until the whole document has been parsed.
* `Fun` is  more general than the others, and supplies a function which
can be called repeatedly to obtain the next character.  The function can do this by
any means it chooses: for example from the network (hopefully with buffering
so it's not reading a single character at a time), or from a list of strings
from elsewhere in the application.

Although we use `Channel` in this small example, real applications will tend
to use either `String` or `Fun`.  This is because the `in_channel` interface
is deprecated in Core, and shouldn't be used in new code. _(avsm: this is a
somewhat unsatisfying explanation: what about adding a better Core interface?)_.

Now, let's define the `xml_id` function that uses the input and output values we 
defined above.

```ocaml
let xml_id i o =
  let rec pull depth =
    Xmlm.output o (Xmlm.peek i);
    match Xmlm.input i with
    | `El_start _ -> pull i o (depth + 1)
    | `El_end -> if depth > 1 then pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false
  in
  Xmlm.output o (Xmlm.input i); (* `Dtd *)
  pull 0;
  if not (Xmlm.eoi i) then invalid_arg "document not well-formed"
```

The `xml_id` function begins by defining a recursive helper `pull` function.
The  `pull` function  iterates over `signal`s and consumes them all.
It first uses `Xmlm.peek` to inspect the current input signal
and immediately output it.  The rest of the function is not strictly necessary,
but tracks that all of the tags that have been started via the `El_start`
signal are also closed by a corresponding `El_end` signal.  

The `pull` function isn't invoked immediately.  The first thing we need to
do is consume the `Dtd` signal, which is present in every well-formed XML
input.  Then we invoke `pull` with a depth of `0`, and it consumes the whole
document.  The final action is to call `Xmlm.eoi`
to verifiy that the "end of input" has been reached, since `pull` should
have consumed all of the XML signals.

### Tree parsing XML

Signals enforce a very iterative style of parsing XML, as your program has to
deal with signals arriving serially.  It's often more convenient to deal with
complete XML documents directly in-memory as an OCaml data structure.  You can
convert a signal stream into an OCaml structure by defining the following data
type and helper functions:

```ocaml
type tree =
  | Element of Xmlm.tag * tree list
  | Data of string

let in_tree i =
  let el tag children = Element (tag, children) in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function
  | Element (tag, childs) -> `El (tag, childs)
  | Data d -> `Data d
  in
  Xmlm.output_doc_tree frag o t
```

The type `tree` can be pattern-matched and traversed like a normal OCaml data
structure.  Let's see how this works by extracting out all the "Related Topics"
in the example document.  First, we'll need a few helper combinator functions
to filter through tags and trees, with the following signature:

```ocaml
(* Extract a textual name from an XML tag.
   Discards the namespace information. *)
val name : Xmlm.tag -> string

(* Filter out the contents of a tag [n] from a tagset,
   and return the concatenated contents of all of them *)
val filter_tag : string -> tree list -> tree list

(* Given a list of [trees], concatenate all of the data contents
   into a string, and discard any sub-tags within it *)
val concat_data : tree list -> string
```

The implementation of these signatures fold over the `tree` structure to filter
the tags which match the desired tag name.  A similar version that matches on
tag attributes is left as an exercise for you to try.

```ocaml
let name ((_,n),_) = n

let filter_tag n =
  List.fold_left ~init:[] ~f:(fun acc ->
    function
    |Element (tag, ts) when name tag = n ->
      ts @ acc
    |_ -> acc
  )

let concat_data =
  List.fold_left ~init:"" ~f:(fun acc ->
    function
    |Data s -> acc ^ s
    |_ -> acc
  )
```

(_avsm_: have we explained `fold_left` before this section or does it need a full intro?)

Notice the use of a *guard pattern* in the `filter_tag` pattern match. This
looks for an `Element` tag that matches the name parameter, and concatenates
the results with the accumulator list.

Once we have these helper functions, the selection of all the `<Text>` tags is
a matter of chaining the combinators together to peform the selection over the
`tree` data structure.

```ocaml
let topics trees =
  filter_tag "DuckDuckGoResponse" trees
  |> filter_tag "RelatedTopics"
  |> filter_tag "RelatedTopic"
  |> filter_tag "Text"
  |> List.iter ~f:(fun x -> concat_data [x] |> print_endline)

let _ =
  let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) in
  let (_,it) = in_tree i in
  topics [it]
```

The `filter_tag` combinator accepts a `tree list` parameter and outputs a `tree
list`. This lets us easily chain together the results of one filter to another,
and hence select hierarchical XML tags very easily.  When we get to the
`<Text>` tag, we iterate over all the results and print each one individually.

### Constructing XML documents using syntax extensions

In the earlier JSON chapter, we explained how to construct records by creating
the records directly.  You can do exactly the same thing for XML, but there is
also a more automated method available by using OCaml's facility for syntax
extensions.

The OCaml distribution provides the `camlp4` tool for this purpose, which you
can view as a type-safe preprocessor.  Camlp4 operates by loading in a set of
syntax extension modules that transform the Abstract Syntax Tree (AST) of
OCaml, usually by adding nodes that generate code.  We'll talk about how to
build your own syntax extensions later in the book, but for now we'll describe
how to *use* several syntax extensions that make it easier to manipulate
external data formats such as XML.

We'll use the Atom 1.0 syndication format as our example here. Atom feeds allow
web-based programs (such as browsers) to poll a website for updates.  The
website owner publishes a feed of content in a standardized XML format via
HTTP.  This feed is then parsed by clients and compared against previously
downloaded versions to determine which contents are available.

Here's an example of an Atom feed:

```xml
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
 <title>Example Feed</title>
 <subtitle>A subtitle.</subtitle>
 <link href="http://example.org/feed/" rel="self" />
 <link href="http://example.org/" />
 <id>urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6</id>
 <updated>2003-12-13T18:30:02Z</updated>

 <entry>
  <title>Atom-Powered Robots Run Amok</title>
  <link href="http://example.org/2003/12/13/atom03" />
  <link rel="alternate" type="text/html" href="http://example.org/2003/12/13/atom03.html"/>
  <link rel="edit" href="http://example.org/2003/12/13/atom03/edit"/>
  <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
  <updated>2003-12-13T18:30:02Z</updated>
  <summary>Some text.</summary>
  <author>
    <name>John Doe</name>
    <email>johndoe@example.com</email>
  </author>
 </entry>
</feed>
```

We want to build this by minimising the amount of repetitive XML generation
code.  The "Caml on the Web" (COW) library provides a syntax extension that is
useful here.

<note>
<title>Installing Caml on the Web (COW)</title>

The COW library and syntax extension can be installed via OPAM by `opam install
cow`.  There are two OCamlfind packages installed: the library is called `cow`
and the syntax extension is activated with the `cow.syntax` package.

One caveat to bear in mind is that COW isn't fully compatible with Core yet,
and so you must use the syntax extension before opening the Core modules.
(_avsm_: we can fix this easily, but note is here as a warning to reviewers).

</note>

Let's start to build up an Atom specification using Cow.  First, the `<author>`
tag can be represented with the following type:

```ocaml
type author = {
  name: string;
  uri: string option;
  email: string option;
} with xml
```

This is a standard record type definition with the addition of `with xml` at
the end.  This uses a syntax extension to signify that we wish to generate
boilerplate code for handling this record as an XML document.

<sidebar>
<title>Invoking `camlp4` syntax extensions</title>

The OCaml compiler can call `camlp4` automatically during a compilation to
preprocess the source files. This is specified via the `-pp` flag to the
compiler. You don't normally need to specify this flag yourself. Use the
`ocamlfind` utility instead to generate the right command-line flags for you.
Here's a small shell script which preprocesses a source file with the COW
syntax extension:

```bash
#!/bin/sh -x

file=$1
lib=cow.syntax
bin=ocamlfind
args=`$bin query -predicates syntax,preprocessor -r -format '-I %d %a' $lib`
camlp4o -printer o $args $file
```

You can supply `ocamlfind` with a number of different predicates to define the
type of build you are running (preprocessing, compilation or linking).  The
final part of the script invokes the `camlp4o` binary on your ML source file
and outputs the transformed source code to your terminal.

</sidebar>

Let's see the OCaml code that has been generated for our `author` record after
it has been preprocessed:

```ocaml
type author = {
  name: string;
  uri: string option;
  email: string option;
}

let rec xml_of_author author : Cow.Xml.t =
  List.flatten
    [ (match match author.email with
             | None -> []
             | Some var1 -> [ `Data var1 ]
       with
       | [] -> []
       | _ ->
         [ `El (((("", "email"), []) : Cow.Xml.tag),
           (match author.email with
            | None -> []
            | Some var1 -> [ `Data var1 ])) ]);
      (match match author.uri with
        | None -> [] | Some var2 -> [ `Data var2 ]
       with
       | [] -> []
       | _ ->
         [ `El (((("", "uri"), []) : Cow.Xml.tag),
           (match author.uri with
             | None -> []
             | Some var2 -> [ `Data var2 ])) ]);
      (match [ `Data author.name ] with
       | [] -> []
       | _ ->
         [ `El (((("", "name"), []) : Cow.Xml.tag),
         [ `Data author.name ]) ]) ]
```

Notice that the `with xml` clause has been replaced with a new `xml_of_author`
function that has been generated for you.  It accepts an `author` value and
returns an `Xml.t` value.  The generated code isn't really meant to be
human-readable, but you don't normally see it when using the syntax extension
(we've only dumped it out here to illustrate how `camlp4` works).

If we run `xml_of_author` and convert the result to a human-readable string,
our complete example looks like:

```ocaml
type author = {
  name: string;
  uri: string option;
  email: string option;
} with xml

let anil = {
  name = "Anil Madhavapeddy";
  uri = Some "http://anil.recoil.org";
  email = Some "anil@recoil.org"
}

let _ = print_endline (Cow.Xml.to_string (xml_of_author anil))
```

This will generate the following XML output on the terminal when you execute
it:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<email>anil@recoil.org</email>
<uri>http://anil.recoil.org</uri>
<name>Anil Madhavapeddy</name>
```

This is convenient, but just one small portion of Atom.  How do we express the
full Atom scheme from earlier?  The answer is with just a few more records that
match the Atom XML schema.

```ocaml
type author = {
  name: string;
  uri: string option;
  email: string option;
} with xml

type date =
  int * int * int * int * int (* year, month, date, hour, minute *)
with xml

let xml_of_date (year,month,day,hour,min) =
  let d = Printf.sprintf "%.4d-%.2d-%.2dT%.2d:%.2d:00Z" year month day hour min in
  <:xml< $str:d$ >>

type meta = {
  id: string;
  title: string;
  subtitle: string option;
  author: author option;
  rights: string option;
  updated: date;
} with xml
```

We've now filled in more of the Atom schema with these records. The first
problem we run into is that occasionally there is a mismatch between the syntax
extension's idea of what the auto-generated XML should look like, and the
reality of the protocol you are mapping to.

The Atom date field is a good example.  We define it as a tuple of integers,
but the format mandated by the specification is actually a free-form text
format and not XML.  However, because the syntax extension generates normal
OCaml functions, we can just override the `xml_of_date` function with a custom
one which returns the correct XML fragment.  Any references further down the
module will just use our overridden version and ignore the auto-generated one.

There's another interesting bit of new syntax in the `xml_of_date` function
known as a *quotation*.  OCaml not only allows code to be generated during
pre-processing, but also to override the core language grammar with new
constructs.  The most common way of doing this is by embedding the custom
grammars inside `<:foo< ... >>` tags, where `foo` represents the particular
grammar being used.  In the case of COW, this lets you generate XMLM-compatible
OCaml values just by typing in XML tags.

TODO antiquotations.

TODO finish the atom example.

### Working with XHTML

TODO use Cow.Html to generate a more complete Atom feed.

