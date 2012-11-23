# Serialization with JSON, XML and S-Expressions

Data serialization, _i.e._ reading and writing program data to a sequence
of bytes, is an important and common programming task.  Sometimes you
need to match someone else's data format (such as XML), and other times
you just want to quickly dump some values to disk and read them
back later.  To this end, OCaml comes with several techniques for
data serialization depending on what your problem is.

We'll start off by looking at JSON and XML first, as they
are very common third-party data formats.  After that, we'll introduce some
syntax extensions in Core that make it really easy to manipulate s-expressions and safe binary serialisers directly from OCaml types.

## JSON

JSON is a lightweight data-interchange format often used in web services and
browsers.  It is described in [RFC4627](http://www.ietf.org/rfc/rfc4627.txt),
and is designed to be easy to parse and generate.  You'll run into JSON very
often when working with modern APIs, and so we'll cover several different ways
to manipulate it in this chapter. Along the way we'll introduce several new
libraries and syntax extensions which make the job easier.

JSON consists of just two basic structures: an unordered collection of
key/value pairs, and an ordered list of values.  Values can be strings,
booleans, floats, integers or null.  Let's see what an example JSON record for
a book description looks like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .json }
{
  "title": "Real World OCaml",
  "tags" : [ "functional programming", "ocaml", "algorithms" ],
  "pages": 450,
  "authors": [
    { "name": "Jason Hickey", "affiliation": "Google" },
    { "name": "Anil Madhavapeddy", "affiliation": "Cambridge"},
    { "name": "Yaron Minsky", "affiliation": "Jane Street"}
  ],
  "is_online": true
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

JSON values usually start with an object at the top level that contains a set
of key/value pairs.  The keys must be strings, but values can be any JSON type.
In the example above, `tags` is a string list, while the `authors` field
contains a list of records.  Unlike OCaml lists, JSON lists can contain
completely different JSON types within them.

This free-form nature of JSON types is both a blessing and a curse.  It's very
easy to generate JSON values, but code parsing them also has to cope with
handling subtle variations in how values are represented. For example, what if
the `pages` value above is actually represented as a string value of `"450"`
instead of an integer?

Our first task is to parse the JSON into a more structured OCaml type so that
we can use static typing more effectively.  When manipulating JSON in Python or
Ruby, you might write unit tests to check that you have handled unusual inputs.
The OCaml model prefers compile-time static checking as well as unit tests. For
example, using pattern matching can warn you if you've not checked that a value
can be `Null` as well as contain an actual value.

There are several JSON parsers available for OCaml, and we've picked
[`Yojson`](http://mjambon.com/yojson.html) for the remainder of this chapter.
You can `opam install yojson` to get it via the OPAM package manager.
 
### Parsing standard JSON with Yojson

The JSON specification has very few data types, and Yojson implements these in
the `Yojson.Basic` module.  The `json` type shown below is sufficient to
express any valid JSON structure. Note that some of the types are recursive, so
that fields can contain references to more JSON fields, and that it also
specifically includes a `Null` variant for empty fields.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's parse the earlier JSON string example into this type now.  The first stop
is the `Yojson.Basic` documentation, where we find these helpful functions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val from_string : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> json
(* Read a JSON value from a string.
  [buf] : use this buffer at will during parsing instead of creating a new one.
  [fname] : data file name to be used in error messages. It does not have to be a real file.
  [lnum] : number of the first line of input. Default is 1.

val from_channel : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> in_channel -> json
(* Read a JSON value from a channel. See [from_string] for the meaning of the 
   optional arguments. *)

val from_file : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> json
(* Read a JSON value from a file. See [from_string] for the meaning of the optional
   arguments. *)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When first reading these interfaces, you can generally ignore the optional
arguments (with the question marks in the type), as they will be filled in with
sensible values. The simpler signature for these values with the optional
elements removed makes their purpose quite clear:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val from_string : string -> json
val from_file : string -> json
val from_channel : in_channel -> json
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `in_channel` constructor is from the original OCaml standard library, and
its use is considered deprecated when using the Core standard library.  This
leaves us with two ways of parsing the JSON: either from a string buffer, or
from a file on a filesystem.  The next example shows both in action, assuming
the JSON record is stored in a file called *book.json*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std

let _ =
  (* Read JSON file into an OCaml string *)
  let buf = In_channel.read_all "book.json" in

  (* Use the string JSON constructor *)
  let json1 = Yojson.Basic.from_string buf in

  (* Use the file JSON constructor *)
  let json2 = Yojson.Basic.from_file "book.json" in

  (* Test that the two values are the same *)
  print_endline (if json1 = json2 then "OK" else "FAIL")
  print_endline (if phys_equal json1 json2 then "FAIL" else "OK")
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`from_file` is a convenience function in Yojson that accepts an input filename
and takes care of opening and closing it for you. It's far more common to use
`from_string` to construct a JSON value an OCaml `string` buffers. These
strings can come from a network connection (we'll see more of this in in the
{{{ASYNC}}} chapter) or even a database. Finally, the example checks that the
two input mechanisms actually resulted in the same OCaml data structure.

<sidebar>
<title>The difference between `=` and `==`, and `phys_equal` in Core</title>

If you come from a C/C++ background, you will probably reflexively use `==` to
test two values for equality. In OCaml, `==` tests for *physical* equality, and
`=` tests for *structural* equality.

The `==` physical equality test will match if two data structures have
precisely the same pointer in memory.  Two data structures that have identical
contents, but are constructed separately, will not match using this operator.
In the JSON example, the `json1` and `json2` values are not identical and so
would fail the physical equality test.

The `=` structural equality operator recursively inspects each field in the two
values and tests them individually for equality. In the JSON parsing example,
every field will be traversed and checked, and they will check out as equal.
Crucially, if your data structure is cyclical (that is, a value recursively
points back to another field within the same structure), the `=` operator will
never terminate, and your program will hang!  In this situation, you must use
the physical equality operator, or write a custom comparison function that
breaks the recursion.

It's quite easy to mix up the use of `=` and `==`, so Core disables the `==`
operator and provides `phys_equal` instead.  You'll see a type error if you use
`==` anywhere:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# 1 == 2;;
Error: This expression has type int but an expression was expected of type
         [ `Consider_using_phys_equal ]
# phys_equal 1 2;;
- : bool = false
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you feel like hanging your OCaml interpreter, you can verify what happens
with recursive values for yourself:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t1 = { foo1:int; bar1:t2 } and t2 = { foo2:int; bar2:t1 } ;;
type t1 = { foo1 : int; bar1 : t2; }
and t2 = { foo2 : int; bar2 : t1; }
# let rec v1 = { foo1=1; bar1=v2 } and v2 = { foo2=2; bar2=v1 };; 
<lots of text>
# v1 == v1;;
- : bool = true
# phys_equal v1 v1;;
- : bool = true
# v1 = v1 ;;
<press ^Z and kill the process now>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

</sidebar>

#### Selecting values from JSON structures

Now that we've figured out how to parse the example JSON, lets see how we can
manipulate it from OCaml code with a more complete example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std
open Async.Std
open Printf

let _ =
  (* Read the JSON file *)
  let json = Yojson.Basic.from_file "book.json" in

  (* Locally open the JSON manipulation functions *)
  let open Yojson.Basic.Util in
  let title = json |! member "title" |! to_string in
  let tags = json |! member "tags" |! to_list |! filter_string in
  let pages = json |! member "pages" |! to_int in
  let is_online = json |! member "is_online" |! to_bool_option in
  let is_translated = json |! member "is_translated" |! to_bool_option in
  let authors = json |! member "authors" |! to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |! to_string) in

  (* Print the results of the parsing *)
  printf "Title: %s (%d)\n" title pages;
  printf "Authors: %s\n" (String.concat ~sep:", " names);
  printf "Tags: %s\n" (String.concat ~sep:", " tags);
  let string_of_bool_option =
    function
    | None -> "<none>"
    | Some true -> "yes"
    | Some false -> "no" in
  printf "Online: %s\n" (string_of_bool_option is_online);
  printf "Translated: %s\n" (string_of_bool_option is_translated)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This introduces the `Yojson.Basic.Util` module, which contains *combinator*
functions for JSON manipulation.  Combinators are a style of function that can
be chained together using the `|!` pipe operator to select and convert values
out of the JSON structure.  Let's examine some of them in more detail:

* For the `title` string, the `member` combinator extracts the key from the array, and casts it to an OCaml string. An exception is raised if the JSON value is not a string.
* The `tags` field is similar to `title`, but are passed through the `to_list` combinator since they are a JSON list.  The `filter_string` combinator folds all of the strings in the JSON list into an OCaml list (any non-strings also in there are simply ignored).
* The `is_online` and `is_translated` fields are optional in our JSON schema, and no error is raised if they are not present in the JSON array. The resulting OCaml type is a `string option` to reflect this. In our example, only `is_online` is present and `is_translated` will be `None`.

In the last part of the example, we simply print the parsed fields since they
are just normal OCaml values. This technique of using chained parsing functions
is very powerful in combination with the OCaml type system. Many errors that
don't make sense at runtime (for example, mixing up lists and objects) will be
caught statically via a type error.

### Constructing JSON values 

To build and print JSON values, you can just construct values of type `json`
and call the `to_string` function.  There are also pretty-printing functions
that lay out the output in a more human-readable style:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = `Assoc [ ("key", `String "value") ] ;;
val x : [> `Assoc of (string * [> `String of string ]) list ] =
  `Assoc [("key", `String "value")]
# Yojson.Basic.pretty_to_string x ;;
- : string = "{ \"key\": \"value\" }"
# Yojson.Basic.pretty_to_channel stdout x ;;
{ "key": "value" }
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the example above, although the value that `x` has is compatible with the
type `json`, it's not explicitly defined as such.  The type inference engine
will figure out a type that is based on how the value `x` is used, and in this
case only the `Assoc` and `String` variants are present.  One difficulty you
will encounter is that type errors involving polymorphic variants can be quite
verbose if you make a mistake in your code.  For example, suppose you build an
`Assoc` and include a single value instead of a list of keys:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = `Assoc ("key", `String "value");;
val x : [> `Assoc of string * [> `String of string ] ] =
  `Assoc ("key", `String "value")
# Yojson.Basic.pretty_to_string x;;
Error: This expression has type
         [> `Assoc of string * [> `String of string ] ]
       but an expression was expected of type Yojson.Basic.json
       Types for tag `Assoc are incompatible
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type error above isn't *wrong* as such, but can be inconvenient to wade
through for larger values.  An easy way to narrow down this sort of type error
is to add explicit type annotations as a compiler hint about your intentions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (x:Yojson.Basic.json) = `Assoc ("key", `String "value");;
Error: This expression has type 'a * 'b
       but an expression was expected of type
         (string * Yojson.Basic.json) list
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, we've marked the `x` as being of type `Yojson.Basic.json`, and
the compiler immediately spots that the argument to the `Assoc` variant has the
incorrect type.  This illustrates the strengths and drawbacks of using
polymorphic variants: they make it possible to easily subtype across module
boundaries (the `Basic` and `Safe` in Yojson's case), but the error messages
can be more confusing.  However, a bit of careful manual type annotation is 
all it takes to make tracking down such issues much easier.

_avsm_: segway into memory representation of polyvariants here?

#### Using non-standard JSON extensions

The standard JSON types are *really* basic, and OCaml types are far more
expressive. Yojson supports an extended JSON format for those times when you're
not interoperating with external systems and just want a convenient
human-readable local format.  The `Yojson.Safe.json` type is a superset of the
`Basic` polymorphic variant, and looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type json = [ 
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Floatlit of string
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Stringlit of string
  | `Tuple of json list
  | `Variant of string * json option ] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You should immediately be able to spot a benefit of using polymorphic variants
here.  A standard JSON type such as a `String` will type-check against both the
`Basic` module and also the non-standard `Safe` module.  However, if you use
extension values such as `Tuple` with the `Basic` module, they will not be a
valid sub-type and the compiler will complain.

The extensions includes with Yojson include:

* The `lit` suffix denotes that the value is stored as a JSON string. For example, a `Floatlit` will be stored as `"1.234"` instead of `1.234`.
* The `Tuple` type is stored as `("abc", 123)` instead of a list.
* The `Variant` type encodes OCaml variants more explicitly, as `<"Foo">` or `<"Bar":123>` for a variant with parameters.
 
The only purpose of these extensions is to make the data representation more
expressive without having to refer to the original OCaml types.  You can always
cast a `Safe.json` to a `Basic.json` type by using the `to_basic` function as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val to_basic : json -> Yojson.Basic.json
(** Tuples are converted to JSON arrays, Variants are converted to JSON strings
or arrays of a string (constructor) and a json value (argument). Long integers
are converted to JSON strings.  Examples: 

`Tuple [ `Int 1; `Float 2.3 ]   ->    `List [ `Int 1; `Float 2.3 ]
`Variant ("A", None)            ->    `String "A"
`Variant ("B", Some x)          ->    `List [ `String "B", x ]
`Intlit "12345678901234567890"  ->    `String "12345678901234567890"
 *)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Automatically mapping JSON to OCaml types

The combinators described earlier make it fairly easy to extract fields from
JSON records, but the process is still pretty manual.  We'll talk about how to
do larger-scale JSON parsing now, using a domain-specific language known as
[ATD](http://oss.wink.com/atdgen/).

The idea behind ATD is to specify the format of the JSON in a separate file,
and then run a compiler (`atdgen`) that outputs OCaml code to construct and
parse JSON values.  This means that you don't need to write any OCaml parsing
code at all, as it will all be auto-generated for you.

Let's go straight into looking at an example of how this works, by using a
small portion of the Github API.  Github is a popular code hosting and sharing
website that provides a JSON-based web [API](http://developer.github.com).  The
ATD code fragment below describes the Github authorization API.  It is based on
a pseudo-standard web protocol known as OAuth, and is used to authorized users
to access Github services.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type scope = [
    User <json name="user">
  | Public_repo <json name="public_repo">
  | Repo <json name="repo">
  | Repo_status <json name="repo_status">
  | Delete_repo <json name="delete_repo">
  | Gist <json name="gist">
]

type app = {
  name: string;
  url: string;
}  <ocaml field_prefix="app_">

type authorization_request = {
  scopes: scope list;
  note: string;
} <ocaml field_prefix="auth_req_">

type authorization_response = {
  scopes: scope list;
  token: string;
  app: app;
  url: string;
  id: int;
  ?note: string option;
  ?note_url: string option;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

ATD is (deliberately) similar to OCaml type definitions, with some important
differences. Each field can include extra annotations to customise the parsing
code for a particular backend. For example, the Github `scope` field above is
defined as a variant type, but with the actual JSON values being defined
explicitly (as lower-case versions).

The ATD spec can be compiled to a number of OCaml targets. Let's run the
compiler twice, to generate some OCaml type definitions, and a JSON serialiser.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .sh }
$ atdgen -t github.atd
$ atdgen -j github.atd
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This will generate some new files in your current directory. `Github_t.ml` and
`Github_t.mli` will contain an OCaml module with types defines that correspond
to the ATD file.  It looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type scope = [
  | `User | `Public_repo | `Repo | `Repo_status 
  | `Delete_repo | `Gist
]

type app = { 
  app_name (*atd name *): string; 
  app_url (*atd url *): string 
}

type authorization_request = {
  auth_req_scopes (*atd scopes *): scope list;
  auth_req_note (*atd note *): string
}

type authorization_response = {
  scopes: scope list;
  token: string;
  app: app;
  url: string;
  id: int;
  note: string option;
  note_url: string option
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is an obvious correspondence to the ATD definition.  Note in particular
that field names in separate OCaml records cannot shadow each other, and so we
specifically prefix every field with a prefix to distinguish it from other
records. For example, `<ocaml field_prefix="auth_req_">` in the ATD spec
prefixes every field name in the generated `authorization_request` record with
`auth_req`.

The `Github_t` module only contains the type definitions, while `Github_j` has
a concrete serialization module to and from JSON.  You can read the
`github_j.mli` to see the full interface, but the important functions for most
uses are the conversion functions to and from a string.  For our example above,
this looks like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val string_of_authorization_response :
  ?len:int -> authorization_response -> string
  (** Serialize a value of type {!authorization_response}
      into a JSON string.
      @param len specifies the initial length 
                 of the buffer used internally.
                 Default: 1024. *)

val authorization_response_of_string :
  string -> authorization_response
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is pretty convenient! We've written a single ATD file, and all the OCaml
boilerplate to convert between JSON and a strongly typed record has been
generated for us.  You can control various aspects of the serializer by passing
flags to `atdgen`. The important ones for JSON are:

* `-j-std`: work in standard JSON mode, and never print non-standard JSON extensions.
* `-j-custom-fields FUNCTION`: call a custom function for every unknown field encountered, instead of raising a parsing exception.
* `-j-defaults`: force the output a JSON value even if the specification defines it as the default value for that field.

The full ATD specification is quite sophisticated (and well documented online
at its homepage).  The ATD compiler can also target formats other than JSON,
and also outputs code for other languages such as Java if you need more
interoperability.  There are also several similar projects you can investigate
which automate the code generation process: [Piqi](http://piqi.org) uses the
Google protobuf format, and [Thrift](http://thrift.apache.org) supports a huge
variety of other programming languages.

We'll also return to the Github example here later in the book when discussing
the Async networking library, and you can find the full ATD specification for
Github in the [`ocaml-github`](http://github.com/avsm/ocaml-github) repository.

## XML

XML is a markup language designed to store tree-structured data in a format that is (somewhat) human- and machine-readable. Like JSON, it is a textual format  commonly used in web technologies, with a complete [specification](http://www.w3.org/TR/REC-xml/) available online. A complete description is beyond the scope of this book, but we are going to explain how to manipulate it using the [XMLM](http://erratique.ch/software/xmlm/doc/Xmlm) OCaml library.  You can install XMLM by `opam install xmlm` to get the latest version.

Since XML is such a common web format, we've taken our example document from the [DuckDuckGo](http://duckduckgo.com) search engine. This is a smaller search engine than the usual suspects, but has the advantage of a freely available API that doesn't require you to register before using it.  We'll talk more about how to use the API later in the {{{ASYNC}}} chapter, but for now here's what a shortened XML search response from DuckDuckGo looks like:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The XML document is structured as a series of `<tags>`. Tags can have an optional set of key/value attributes and usually contain text or further tags.
If the XML document is very large, we don't want to read the whole thing into memory before processing it.
Luckily we don't have to, as there are two parsing strategies for XML: a low-level *streaming* API that parses a document incrementally, and a simpler but more inefficient tree API.  We'll start with the streaming API first, as the tree API is built on top of it.

### Stream parsing XML

Let's start by looking at the XMLM docs, which tells us that:

> A well-formed sequence of `signal`s represents an XML document tree traversal in depth-first order. Input pulls a well-formed sequence of `signal`s from a data source and output pushes a well-formed sequence of `signal`s to a data destination. Functions are provided to easily transform sequences of `signal`s to/from arborescent data structures.

The type of a `signal` reveals the basic structure of the streaming API in XMLM:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type signal = [
  | `Data of string
  | `Dtd of dtd
  | `El_end 
  | `El_start of tag 
] 

(** The type for signals. A well-formed sequence of signals belongs to the 
    language of the doc grammar :
doc ::= `Dtd tree
tree ::= `El_start child `El_end
child ::= `Data | tree | epsilon 
*)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

XMLM outputs an ordered sequence of these signals to your code as it parses the  document.
Let's look at how to write the XML identity function that reads in a document and immediately outputs it.
Since this uses the streaming API, there is minimal buffering required.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let xml_id i o =
  let rec pull i o depth =
    Xmlm.output o (Xmlm.peek i);
    match Xmlm.input i with
    | `El_start _ -> pull i o (depth + 1)
    | `El_end -> if depth > 1 then pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false
  in
  Xmlm.output o (Xmlm.input i); (* `Dtd *)
  pull i o 0;
  if not (Xmlm.eoi i) then invalid_arg "document not well-formed"

let _ =
  let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) in
  let o = Xmlm.make_output (`Channel stdout) in
  xml_id i o
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start at the bottom, where we open up input and output channels for XMLM to use.
The `input` and `output` constructor functions use a polymorphic variant to define
the mechanism that the library should use to read the document.
`Channel` is the simplest, but there are several others available.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type source = [
  | `Channel of in_channel
  | `Fun of unit -> int
  | `String of int * string 
] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `Fun` channel returns one character at a time as an integer, and `String` starts parsing an OCaml string from the given integer offset.  Both of these are will normally be used in preference to `Channel`, which uses an interface that is deprecated in Core.

The `xml_id` function begins by reading one signal, which will always be a `Dtd`.  The structure of XML documents is defined via a optional "Document Type Description" (DTD).  Some XML parsers can validate a document against a DTD, but XMLM is a simpler *non-validating* parser that reads the DTD if present but disregards its contents.

The recursive `pull` function is then invoked to iterate over the remaining signals.
This uses `Xmlm.peek` to inspect the current input signal and immediately output it.
The rest of the function is not strictly necessary, but it tracks that all of the tags that have been started via the `El_start` signal are also closed by a corresponding `El_end` signal.
Once the `pull` function has finished due to the opening tag being closed, the `Xmlm.eoi` function verifies that the "end of input" has been reached.

### Tree parsing XML

The signals provide a very iterative style of parsing XML, as your program has to deal with signals arriving serially.  It's sometimes more convenient to deal with small XML documents directly in-memory as an OCaml data structure.
It's easy to convert a signal stream into a tree, as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type `tree` can be pattern-matched and traversed like a normal OCaml data structure.  Let's see how this works by extracting out all the "Related Topics" in the example document.
First, we'll need a few helper combinator functions to filter through tags and trees, with the following signature:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* Extract a textual name from an XML tag.
   Discards the namespace information. *)
val name : Xmlm.tag -> string

(* Filter out the contents of a tag [n] from a tagset,
   and return the concatenated contents of all of them *)
val filter_tag : string -> tree list -> tree list

(* Given a list of [trees], concatenate all of the data contents               
   into a string, and discard any sub-tags within it *)
val concat_data : tree list -> string
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implementation of these functions traverses the `tree` type that we defined earlier.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let name ((_,n),_) = n

let filter_tag n =
  List.fold_left ~init:[] ~f:(fun acc ->
    function
    |Element (tag, ts) when name tag = n -> ts @ acc
    |_ -> acc
  )
     
let concat_data =
  List.fold_left ~init:"" ~f:(fun acc ->
    function
    |Data s -> acc ^ s
    |_ -> acc                           
  )
~~~~~~~~~~~~~~~~~~~~~~~~~~~

(_avsm_: have we explained `fold_left` before this section or does it need a full intro?)

Notice the use of a *guard pattern* in the `filter_tag` pattern match. This looks for an `Element` tag that matches the name parameter, and concatenates the results with the accumulator list.

Once we have these helper functions, the selection of all the `<Text>` tags is easy:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let topics trees =
  filter_tag "DuckDuckGoResponse" trees |!
  filter_tag "RelatedTopics" |!
  filter_tag "RelatedTopic" |!
  filter_tag "Text" |!
  List.iter ~f:(fun x -> concat_data [x] |! print_endline)

let _ =
  let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) in
  let (_,it) = in_tree i in
  topics [it]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `filter_tag` combinator accepts a `tree list` parameter and outputs a `tree list`. This lets us easily chain together the results of one filter to another, and hence select hierarchical XML tags very easily.
When we get to the `<Text>` tag, we iterate over all the results and print each one individually.

### Using the COW syntax extension

TODO: Explain how the p4 XML extension works. This is a good excuse.

### Working with XHTML

TODO



## Serialization with s-expressions

So far, we've talked about interoperating with formats that are usually defined by third-parties.  It's also very common to just exchange and persist OCaml values safely, so we'll discuss how to do this now.

S-expressions are nested paranthetical strings whose atomic values are strings. 

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Sexp : sig
  type t = Atom of string | List of t list
end
~~~~~~~~~~~~~~~~~~~~~~~~

An s-expression is in essence a nested parenthetical list whose atomic
values are strings.  The `Sexp` module comes with functionality for
parsing and printing s-expressions.

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let sexp =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [a "this";l [a "is"; a "an"]; l [a "s"; a "expression"]];;
val sexp : Sexp.t = (this (is an) (s expression))
~~~~~~~~~~~~~~~~~~~~~~~~

In addition, most of the base types in Core support conversion to and
from s-expressions.  For example, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int.sexp_of_t 3;;
- : Sexp.t = 3
# List.sexp_of_t;;
- : ('a -> Sexp.t) -> 'a List.t -> Sexp.t = <fun>
# List.sexp_of_t Int.sexp_of_t [1;2;3];;
- : Sexp.t = (1 2 3)
~~~~~~~~~~~~~~~~~~~~~~~~

Notice that `List.sexp_of_t` is polymorphic, and takes as its first
argument another conversion function to handle the elements of the
list to be converted.  Core uses this scheme more generally for
defining sexp-converters for polymorphic types.

But what if you want a function to convert some brand new type to an
s-expression?  You can of course write it yourself:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float };;
# let sexp_of_t t =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ l [a "foo"; Int.sexp_of_t t.foo  ];
        l [a "bar"; Float.sexp_of_t t.bar]; ]
  ;;
val sexp_of_t : t -> Core.Std.Sexp.t = <fun>
# sexp_of_t { foo = 3; bar = -5.5 };;
- : Core.Std.Sexp.t = ((foo 3) (bar -5.5))
~~~~~~~~~~~~~~~~~~~~~~~~

This is somewhat tiresome to write, and it gets more so when you
consider the parser, _i.e._, `t_of_sexp`, which is considerably more
complex.  Writing this kind of parsing and printing code by hand is
mechanical and error prone, not to mention a drag.

Given how mechanical the code is, you could imagine writing a program
that inspected the type definition and auto-generated the conversion
code for you.  That is precisely where syntax extensions come in.
Using Sexplib and adding `with sexp` as an annotation to our type
definition, we get the functions we want for free.

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float } with sexp;;
type t = { foo : int; bar : float; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))");;
- : t = {foo = 3; bar = 35.}
~~~~~~~~~~~~~~~~~~~~~~~~

(You can ignore `t_of_sexp__`, which is a helper function that is
needed in very rare cases.)

The syntax-extensions in Core that we're going to discuss all have
this same basic structure: they auto-generate code based on type
definitions, implementing functionality that you could in theory have
implemented by hand, but with far less programmer effort.

There are several syntax extensions distributed with Core, including:

- **Sexplib**: provides serialization for s-expressions.
- **Bin_prot**: provides serialization to an efficient binary
  format.
- **Fieldslib**: generates first-class values that represent fields of
  a record, as well as accessor functions and setters for mutable
  record fields.
- **Variantslib**: like Fieldslib for variants, producing first-class
  variants and other helper functions for interacting with variant
  types.
- **Pa_compare**: generates efficient, type-specialized comparison
  functions.
- **Pa_typehash**: generates a hash value for a type definition,
  _i.e._, an integer that is highly unlikely to be the same for two
  distinct types.

We'll discuss each of these syntax extensions in detail, starting with
Sexplib.

## Sexplib

### Formatting of s-expressions

Sexplib's format for s-expressions is pretty straightforward: an
s-expression is written down as a nested parenthetical expression,
with whitespace-separated strings as the atoms.  Quotes are used for
atoms that contain parenthesis or spaces themselves, backslash is the
escape character, and semicolons are used to introduce comments.
Thus, if you create the following file:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; foo.scm

((foo 3.3) ;; Shall I compare thee  to a summer's dream?
 (bar "this is () an \" atom"))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

we can load it up and print it back out again:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Sexp.load_sexp "foo.scm";;
- : Sexp.t = ((foo 3.3) (bar "this is () an \" atom"))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the comments were dropped from the file upon reading.  This
is expected, since there's no place in the `Sexp.t` type to store
comments.

If we introduce an error into our s-expression, by, say, deleting the
open-paren in front of `bar`, we'll get a parse error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Exn.handle_uncaught ~exit:false (fun () ->
    ignore (Sexp.load_sexp "foo.scm"));;
  Uncaught exception:

  (Sexplib.Sexp.Parse_error
   ((location parse) (err_msg "unexpected character: ')'") (text_line 4)
    (text_char 29) (global_offset 94) (buf_pos 94)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

(In the above, we use `Exn.handle_uncaught` to make sure that the
exception gets printed out in full detail.)

### Sexp converters

The most important functionality provided by Sexplib is the
auto-generation of converters for new types.  We've seen a bit of how
this works already, but let's walk through a complete example.  Here's
the source for the beginning of a library for representing integer
intervals.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: int_interval.ml *)
(* Module for representing closed integer intervals *)

open Core.Std

(* Invariant: For any Range (x,y), y > x *)
type t = | Range of int * int
         | Empty
with sexp

let is_empty = function Empty -> true | Range _ -> false
let create x y = if x > y then Empty else Range (x,y)
let contains i x = match i with
   | Empty -> false
   | Range (low,high) -> x >= low && x <= high
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now use this module as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: test_interval.ml *)

open Core.Std

let intervals =
  let module I = Int_interval in
  [ I.create 3 4;
    I.create 5 4; (* should be empty *)
    I.create 2 3;
    I.create 1 6;
  ]

let () =
  intervals
  |! List.sexp_of_t Int_interval.sexp_of_t
  |! Sexp.to_string_hum
  |! print_endline
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But we're still missing something: we haven't created an `mli` for
`Int_interval` yet.  Note that we need to explicitly export the
s-expression converters that were created within the ml.  If we don't:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: int_interval.mli *)
(* Module for representing closed integer intervals *)

type t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
~~~~~~~~~~~~~~~~~~~~~~~~~~~

then we'll get the following error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
File "test_interval.ml", line 15, characters 20-42:
Error: Unbound value Int_interval.sexp_of_t
Command exited with code 2.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could export the types by hand:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type t
val sexp_of_t : Sexp.t -> t
val t_of_sexp : t -> Sexp.t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But Sexplib has a shorthand for this as well, so that we can instead
write simply:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type t with sexp
~~~~~~~~~~~~~~~~~~~~~~~~~~~

at which point `test_interval.ml` will compile again, and if we run
it, we'll get the following output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./test_interval.native
((Range 3 4) Empty (Range 2 3) (Range 1 6))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

<sidebar> <title>Preserving invariants</title>

One easy mistake to make when dealing with sexp converters is to
ignore the fact that those converters can violate the invariants of
your code.  For example, the `Int_interval` module depends for the
correctness of the `is_empty` check on the fact that for any value
`Range (x,y)`, `y` is greater than or equal to `x`.  The `create`
function preserves this invariant, but the `t_of_sexp` function does
not.

We can fix this problem by writing a custom sexp-converter, in this
case, using the sexp-converter that we already have:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type t = | Range of int * int
         | Empty
with sexp

let create x y = if x > y then Empty else Range (x,y)

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  begin match t with
  | Range (x,y) when y < x ->
    of_sexp_error "Upper and lower bound of Range swapped" sexp
  | Empty | Range _ -> ()
  end;
  t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We call the function `of_sexp_error` to raise an exception because
that improves the error reporting that Sexplib can provide when a
conversion fails.

</sidebar>

### Getting good error messages

There are two steps to deserializing a type from an s-expression:
first, converting the bytes in a file to an s-expression, and the
second, converting that s-expression into the type in question.  One
problem with this is that it can be hard to localize errors to the
right place using this scheme.  Consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: read_foo.ml *)

open Core.Std

type t = { a: string; b: int; c: float option } with sexp

let run () =
  let t =
    Sexp.load_sexp "foo.scm"
    |! t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you were to run this on a malformatted file, say, this one:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; foo.scm
((a not-an-integer)
 (b not-an-integer)
 (c ()))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

you'll get the following error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_foo $ ./read_foo.native
Uncaught exception:

  (Sexplib.Conv.Of_sexp_error
   (Failure "int_of_sexp: (Failure int_of_string)") not-an-integer)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If all you have is the error message and the string, it's not terribly
informative.  In particular, you know that the parsing error-ed out on
the atom "not-an-integer", but you don't know which one!  In a large
file, this kind of bad error message can be pure misery.

But there's hope!  If we make small change to the `run` function as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let run () =
  let t = Sexp.load_sexp_conv_exn "foo.scm" t_of_sexp in
  printf "b is: %d\n%!" t.b
~~~~~~~~~~~~~~~~~~~~~~~~~~~

and run it again, we'll get the following much more helpful error
message:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
read_foo $ ./read_foo.native
Uncaught exception:

  (Sexplib.Conv.Of_sexp_error
   (Sexplib.Sexp.Annotated.Conv_exn foo.scm:3:4
    (Failure "int_of_sexp: (Failure int_of_string)"))
   not-an-integer)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the above error, "foo.scm:3:4" tells us that the error occurred on
"foo.scm", line 3, character 4, which is a much better start for
figuring out what has gone wrong.

### Sexp-conversion directives

Sexplib supports a collection of directives for modifying the default
behavior of the auto-generated sexp-converters.  These directives allow
you to customize the way in which types are represented as
s-expressions without having to write a custom parser.  We describe
these directives below.

#### `sexp-opaque`

The most commonly used directive is `sexp_opaque`, whose purpose is to
mark a given component of a type as being unconvertible.  Anything
marked with `sexp_opaque` will be presented as the atom `<opaque>` by
the to-sexp converter, and will trigger an exception from the
from-sexp converter.  Note that the type of a component marked as
opaque doesn't need to have a sexp-converter defined.  Here, if we
define a type without a sexp-converter, and then try to use it another
type with a sexp-converter, we'll error out:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type no_converter = int * int;;
type no_converter = int * int
# type t = { a: no_converter; b: string } with sexp;;
Characters 14-26:
  type t = { a: no_converter; b: string } with sexp;;
                ^^^^^^^^^^^^
Error: Unbound value no_converter_of_sexp
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But with `sexp_opaque`, we won't:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { a: no_converter sexp_opaque; b: string } with sexp;;
type t = { a : no_converter Core.Std.sexp_opaque; b : string; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And if we now convert a value of this type to an s-expression, we'll
see the contents of field `a` marked as opaque:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# sexp_of_t { a = (3,4); b = "foo" };;
- : Sexp.t = ((a <opaque>) (b foo))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### `sexp_option`

Another common directive is `sexp_opaque`, which is used to make an
optional field in a record.  Ordinary optional values are represented
either as `()` for `None`, or as `(x)` for `Some x`.  If you put an
option in a record field, then the record field will always be
required, and its value will be presented in the way an ordinary
optional value would.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { a: int option; b: string } with sexp;;
# sexp_of_t { a = None; b = "hello" };;
- : Sexp.t = ((a ()) (b hello))
# sexp_of_t { a = Some 3; b = "hello" };;
- : Sexp.t = ((a (3)) (b hello))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But what if we want a field to be optional, _i.e._, we want to allow
it to be omitted from the record entirely?  In that case, we can mark
it with `sexp_option`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type t = { a: int sexp_option; b: string } with sexp;;
# sexp_of_t { a = Some 3; b = "hello" };;
- : Sexp.t = ((a 3) (b hello))
# sexp_of_t { a = None; b = "hello" };;
- : Sexp.t = ((b hello))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### `sexp_list`

One problem with the auto-generated sexp-converters is that they can
have more parentheses than one would ideally like.  Consider, for
example, the following variant type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type compatible_versions = | Specific of string list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"]);;
- : Sexp.t = (Specific (3.12.0 3.12.1 3.13.0))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might prefer to make the syntax a bit less parenthesis-laden by
dropping the parentheses around the list.  `sexp_list` gives us this
alternate syntax:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type compatible_versions = | Specific of string sexp_list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"]);;
- : Sexp.t = (Specific 3.12.0 3.12.1 3.13.0)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Bin_prot

S-expressions are a good serialization format when you need something
machine-parseable as well as human readable and editable.  But
Sexplib's s-expressions are not particularly performant.  There are a
number of reasons for this.  For one thing, s-expression serialization
goes through an intermediate type, `Sexp.t`, which must be allocated
and is then typically thrown away, putting non-trivial pressure on the
GC.  In addition, parsing and printing to strings in an ASCII format
can be expensive for types like `int`s, `float`s and `Time.t`s where
some real computation needs to be done to produce or parse the ASCII
representation.

Bin_prot is a library designed to address these issues by providing
fast serialization in a compact binary format.  Kicking off the syntax
extension is done by putting `with bin_io`.  (This looks a bit
unsightly in the top-level because of all the definitions that are
generated.  We'll elide those definitions here, but you can see it for
yourself in the toplevel.)

Here's a small complete example of a program that can read and write
values using bin-io.  Here, the serialization is of types that might
be used as part of a message-queue, where each message has a topic,
some content, and a source, which is in turn a hostname and a port.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: message_example.ml *)

open Core.Std

(* The type of a message *)
module Message = struct
  module Source = struct
    type t = { hostname: string;
               port: int;
             }
    with bin_io
  end

  type t = { topic: string;
             content: string;
             source: Source.t;
           }
  with bin_io
end

(* Create the 1st-class module providing the binability of messages *)
let binable = (module Message : Binable.S with type t = Message.t)

(* Saves a message to an output channel.  The message is serialized to
   a bigstring before being written out to the channel.  Also, a
   binary encoding of an integer is written out to tell the reader how
   long of a message to expect.  *)
let save_message outc msg =
  let s = Binable.to_bigstring binable msg in
  let len = Bigstring.length s in
  Out_channel.output_binary_int outc len;
  Bigstring.really_output outc s

(* Loading the message is done by first reading in the length, and by
   then reading in the appropriate number of bytes into a Bigstring
   created for that purpose. *)
let load_message inc =
  match In_channel.input_binary_int inc with
  | None -> failwith "Couldn't load message: length missing from header"
  | Some len ->
    let buf = Bigstring.create len in
    Bigstring.really_input ~pos:0 ~len inc buf;
    Binable.of_bigstring binable buf

(* To generate some example messages *)
let example content =
  let source =
    { Message.Source.
      hostname = "ocaml.org"; port = 2322 }
  in
  { Message.
    topic = "rwo-example"; content; source; }

(* write out three messages... *)
let write_messages () =
  let outc = Out_channel.create "tmp.bin" in
  List.iter ~f:(save_message outc) [
    example "a wonderful";
    example "trio";
    example "of messages";
  ];
  Out_channel.close outc

(* ... and read them back in *)
let read_messages () =
  let inc = In_channel.create "tmp.bin" in
  for i = 1 to 3 do
    let msg = load_message inc in
    printf "msg %d: %s\n" i msg.Message.content
  done

let () =
  write_messages (); read_messages ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Fieldslib

One common idiom when using records is to provide field accessor
functions for a particular record.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
type t = { topic: string;
           content: string;
           source: Source.t;
         }

let topic   t = t.topic
let content t = t.content
let source  t = t.source
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similarly, sometimes you simultaneously want an accessor to a field of
a record and a textual representation of the name of that field.  This
might come up if you were validating a field and needed the string
representation to generate an error message, or if you wanted to
scaffold a form in a GUI automatically based on the fields of a
record.  Fieldslib provides a module `Field` for this purpose.  Here's
some code for creating `Field.t`'s for all the fields of our type `t`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# module Fields = struct
    let topic =
      { Field.
        name   = "topic";
        setter = None;
        getter = (fun t -> t.topic);
        fset   = (fun t topic -> { t with topic });
      }
    let content =
      { Field.
        name   = "content";
        setter = None;
        getter = (fun t -> t.content);
        fset   = (fun t content -> { t with content });
      }
    let source =
      { Field.
        name   = "source";
        setter = None;
        getter = (fun t -> t.source);
        fset   = (fun t source -> { t with source });
      }
  end ;;
module Fields :
  sig
    val topic : (t, string list) Core.Std.Field.t
    val content : (t, string) Core.Std.Field.t
    val source : (t, Source.t) Core.Std.Field.t
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

