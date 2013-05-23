# Handling JSON data

Data serialization, _i.e._ converting data to and from a sequence of
bytes that's suitable for writing to disk or sending across the
network, is an important and common programming task.  Sometimes you
need to match someone else's data format (such as XML), sometimes you
need a highly efficient format, and sometimes you just want something
that is easy for humans to read and edit.  To this end, OCaml comes
with several techniques for data serialization depending on what your
problem is.

We'll start by using the popular and simple JSON data format, and then look at
other serialization formats later in in the book.  This chapter introduces you
to a couple of new techniques that glue together the basic ideas from Part I of
the book:

* Using polymorphic variants to write more extensible libraries and protocols (but
  still retain the ability to extend them if needed)
* The use of _combinators_ to compose common operations over data
  structures in a type-safe way.
* Using external tools to generate boilerplate OCaml modules and
  signatures from external specification files.

## JSON Basics

JSON is a lightweight data-interchange format often used in web
services and browsers.  It's described in
[RFC4627](http://www.ietf.org/rfc/rfc4627.txt), and is easier to parse
and generate than alternatives such as XML.  You'll run into JSON very
often when working with modern web APIs, so we'll cover several
different ways to manipulate it in this chapter.

JSON consists of two basic structures: an unordered collection of
key/value pairs, and an ordered list of values.  Values can be strings,
booleans, floats, integers or null.  Let's see what a JSON record for
an example book description looks like:

```json
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
```

The outermost JSON value is usually a record (delimited by the curly
braces) and contains an unordered set of key/value pairs.  The keys
must be strings but values can be any JSON type.  In the example
above, `tags` is a string list, while the `authors` field contains a
list of records.  Unlike OCaml lists, JSON lists can contain multiple
different JSON types within a single list.

This free-form nature of JSON types is both a blessing and a curse.
It's very easy to generate JSON values, but code that parses them also
has to handle subtle variations in how the values are represented. For
example, what if the `pages` value above is actually represented as a
string value of `"450"` instead of an integer?

Our first task is to parse the JSON into a more structured OCaml type
so that we can use static typing more effectively.  When manipulating
JSON in Python or Ruby, you might write unit tests to check that you
have handled unusual inputs.  The OCaml model prefers compile-time
static checking as well as unit tests. For example, using pattern
matching can warn you if you've not checked that a value can be `Null`
as well as contain an actual value.

<note>
<title>Installing the Yojson library</title>

There are several JSON libraries available for OCaml.  For this
chapter, we've picked the [`Yojson`](http://mjambon.com/yojson.html)
library by Martin Jambon.  It's easiest to install via OPAM.

```
$ opam install yojson
```

See [xref](#installation) for installation instructions if you haven't already
got OPAM. Once installed, you can open it in the `utop` toplevel by:

```
#require "yojson" ;;
open Yojson ;;
```

</note>

### Parsing JSON with Yojson

The JSON specification has very few data types, and the
`Yojson.Basic.json` type shown below is sufficient to express any
valid JSON structure.

```ocaml
type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ]
```

Some interesting properties should leap out at you after reading this
definition:

* Some of the type definitions are _recursive_ (that is, one of the
  algebraic data types includes a reference to the name of the type
  being defined). Fields such as `Assoc` can contain references to
  more JSON fields, and thus precisely describe the underlying JSON
  data structure.  The JSON `List` can contain fields of different types,
  unlike the OCaml `list` whose contents must be uniform.
* The definition specifically includes a `Null` variant for empty
  fields.  OCaml doesn't allow null values by default, so this must be
  encoded like any other value.
* The type definition uses polymorphic variants and not normal
  variants. This will become significant later when we extend it with
  custom extensions to the JSON format.

Let's parse the earlier JSON example into this type now.  The first
stop is the `Yojson.Basic` documentation, where we find these helpful
functions:

```ocaml
val from_string : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> json
(* Read a JSON value from a string.
   [buf]   : use this buffer at will during parsing instead of
             creating a new one. 
   [fname] : data file name to be used in error messages. It does not 
             have to be a real file. 
   [lnum]  : number of the first line of input. Default is 1.

val from_channel : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> in_channel -> json
(* Read a JSON value from a channel. See [from_string] for the meaning of the
   optional arguments. *)

val from_file : ?buf:Bi_outbuf.t -> ?fname:string -> ?lnum:int -> string -> json
(* Read a JSON value from a file. See [from_string] for the meaning of the optional
   arguments. *)
```

When first reading these interfaces, you can generally ignore the
optional arguments (which have the question marks in the type
signature), as they will be filled in with sensible values. In the
above signature, the optional arguments offer finer control over the
memory buffer allocation and error messages from parsing incorrect
JSON.

The type signature for these functions with the optional elements
removed makes their purpose much clearer:

```ocaml
val from_string : string -> json
val from_file : string -> json
val from_channel : in_channel -> json
```

The `in_channel` constructor is from the original OCaml standard
library, and its use is considered deprecated when using the Core
standard library.  This leaves us with two ways of parsing the JSON:
either from a string or from a file on a filesystem.  The next example
shows both in action, assuming the JSON record is stored in a file
called `book.json`.

```ocaml
(* read_json.ml *)
open Core.Std

let () =
  (* Read JSON file into an OCaml string *)
  let buf = In_channel.read_all "book.json" in

  (* Use the string JSON constructor *)
  let json1 = Yojson.Basic.from_string buf in

  (* Use the file JSON constructor *)
  let json2 = Yojson.Basic.from_file "book.json" in

  (* Test that the two values are the same *)
  print_endline (if json1 = json2 then "OK" else "FAIL")
  print_endline (if phys_equal json1 json2 then "FAIL" else "OK")
```

You can build this by writing a `_tags` file to define the package
dependencies, and then running `ocamlbuild`.

```console
$ cat _tags
true: package(core,yojson)
true: thread, debug, annot

$ ocamlbuild -use-ocamlfind read_json.native
$ ./read_json.native
OK
OK
```

The `from_file` function accepts an input filename and takes care of
opening and closing it for you. It's far more common to use
`from_string` to construct JSON values though, since these strings
come in via a network connection (we'll see more of this in
[xref](#concurrent-programming-with-async)) or a database. Finally,
the example checks that the two input mechanisms actually resulted in
the same OCaml data structure.

<sidebar>
<title>The difference between `=` and `==`, and `phys_equal` in Core</title>

If you come from a C/C++ background, you will probably reflexively use
`==` to test two values for equality. In OCaml, `==` tests for
*physical* equality, and `=` tests for *structural* equality.

The `==` physical equality test will match if two data structures have
precisely the same pointer in memory.  Two data structures that have
identical contents, but are constructed separately, will not match
using this operator.  In the JSON example, the `json1` and `json2`
values are not identical and so would fail the physical equality test.

The `=` structural equality operator recursively inspects each field
in the two values and tests them individually for equality. In the
JSON parsing example, every field will be traversed and checked, and
they will check out as equal.  Crucially, if your data structure is
cyclical (that is, a value recursively points back to another field
within the same structure), the `=` operator will never terminate, and
your program will hang!  In this situation, you must use the physical
equality operator, or write a custom comparison function that breaks
the recursion.

It's quite easy to mix up the use of `=` and `==`, so Core disables
the `==` operator and provides the more explicit `phys_equal` function
instead.  You'll see a type error if you use `==` anywhere:

```ocaml
# 1 == 2;;
Error: This expression has type int but an expression was expected of type
         [ `Consider_using_phys_equal ]
# phys_equal 1 2;;
- : bool = false
```

If you feel like hanging your OCaml interpreter, you can verify what
happens with recursive values and structural equality for yourself:

```ocaml
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
```

</sidebar>

#### Selecting values from JSON structures

Now that we've figured out how to parse the example JSON into an OCaml
value, let's manipulate it from OCaml code and extract specific
fields.

```ocaml
(* parse_book.ml *)
open Core.Std

let () =
  (* Read the JSON file *)
  let json = Yojson.Basic.from_file "book.json" in

  (* Locally open the JSON manipulation functions *)
  let open Yojson.Basic.Util in
  let title = json |> member "title" |> to_string in
  let tags = json |> member "tags" |> to_list |> filter_string in
  let pages = json |> member "pages" |> to_int in
  let is_online = json |> member "is_online" |> to_bool_option in
  let is_translated = json |> member "is_translated" |> to_bool_option in
  let authors = json |> member "authors" |> to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |> to_string) in

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
```

Build this with the same `_tags` file as the earlier example, and run
`ocamlbuild` on the new file.

```console
$ ocamlbuild -use-ocamlfind parse_book.native
$ ./parse_book.native 
Title: Real World OCaml (450)
Authors: Jason Hickey, Anil Madhavapeddy, Yaron Minsky
Tags: functional programming, ocaml, algorithms
Online: yes
Translated: <none>
```

This code introduces the `Yojson.Basic.Util` module, which contains _combinator_
functions that let you easily map a JSON object into a more strongly-typed OCaml value.

<sidebar>
<title>Functional Combinators</title>

Combinators are a design pattern that crops up quite often in
functional programming.  John Hughes defines them as "a function which
builds program fragments from program fragments".  In a functional
language, this generally means higher-order functions that combine
other functions to apply useful transformations over values.

You've already run across several of these in the `List` module:

```ocaml
val map  : 'a list -> f:('a -> 'b)   -> 'b list
val fold : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
```

`map` and `fold` are extremely common combinators that transform an
input list by applying a function to each value of the list.  The
`map` combinator is simplest, with the resulting list being output
directly.  `fold` applies each value in the input list to a function
that accumulates a single result, and returns that instead.

```ocaml
val iter : 'a list -> f:('a -> unit) -> unit
```

`iter` is a more specialised combinator that is only useful in OCaml due to
side-effects being allowed.  The input function is applied to every value, but
no result is supplied. The function must instead apply some side-effect such
as changing a mutable record field or printing to the standard output.

</sidebar>

`Yojson` provides several combinators in the `Yojson.Basic.Util` module,
such as:

```ocaml
val member : string -> json -> json
val index : int -> json -> json
val to_string : json -> string
val to_int : json -> int
val filter_string : json list -> string list
```

We'll go through each of these uses one-by-one.  Core provides the
`|>` pipe-forward which can chain combinators together, and the
example code uses this to select and convert values out of the JSON
structure.  Let's examine some of them in more detail:

```ocaml
  let open Yojson.Basic.Util in
  let title = json |> member "title" |> to_string in
```

The `member` function accepts a JSON object and named key and returns
the JSON field associated with that key, or `Null`.  Since we know that
the `title` value is always a string in our example schema, we want
to convert it to an OCaml string.  The `to_string` function performs
this conversion, and raises an exception if there is an unexpected JSON
type.  The `|>` operator provides a convenient way to chain these
operations together.

```ocaml
  let tags = json |> member "tags" |> to_list |> filter_string in
  let pages = json |> member "pages" |> to_int in
```

The `tags` field is similar to `title`, but the field is a list of strings
instead of a single one.  Converting this to an OCaml `string list` is a two
stage process.  First, we convert the JSON `List` to an OCaml list of JSON
values, and then filter out the `String` values as an OCaml `string list`.
Remember that OCaml lists must contain values of the same type, so any JSON
values that cannot be converted to a `string` will be skipped from the output
of `filter_string`.

```ocaml
  let is_online = json |> member "is_online" |> to_bool_option in
  let is_translated = json |> member "is_translated" |> to_bool_option in
```

The `is_online` and `is_translated` fields are optional in our JSON
schema, so no error should be raised if they are not present. The
OCaml type is a `string option` to reflect this, and can be extracted
via `to_bool_option`.  In our example JSON, only `is_online` is
present and `is_translated` will be `None`.

```ocaml
  let authors = json |> member "authors" |> to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |> to_string) in
```

The final use of JSON combinators is to extract all the `name` fields from
the list of authors.  We first construct the `author list`, and then
`map` it into a `string list`.  Notice that the example explicitly
binds `authors` to a variable name.  It can also be written more
succinctly using the pipe-forward operator:

```ocaml
let names =
  json
  |> member "authors"
  |> to_list
  |> List.map ~f:(fun json -> member "name" json |> to_string)
```

This style of programming which omits variable names and chains
functions together is known as "point-free programming".  It's a
succinct style, but shouldn't be overused due to the increased
difficulty of debugging intermediate values.  If an explicit name is
assigned to each stage of the transformations, debuggers in particular
have an easier time making the program flow easier to represent to the
programer.

This technique of using chained parsing functions is very powerful in
combination with the OCaml type system. Many errors that don't make
sense at runtime (for example, mixing up lists and objects) will be
caught statically via a type error.

### Constructing JSON values

Building and printing JSON values is pretty straightforward given the
`Yojson.Basic.json` type.  You can just construct values of type
`json` and call the `to_string` function] on them.  There are also
pretty-printing functions available that lay out the output in a more
human-readable style:

```ocaml
# let x = `Assoc [ ("key", `String "value") ] ;;
val x : [> `Assoc of (string * [> `String of string ]) list ] =
  `Assoc [("key", `String "value")]

# Yojson.Basic.pretty_to_string x ;;
- : string = "{ \"key\": \"value\" }"

# Yojson.Basic.pretty_to_channel stdout x ;;
{ "key": "value" }
- : unit = ()
```

In the example above, although the type of `x` is compatible with the
type `json`, it's not explicitly defined as such.  The type inference
engine will figure out a type that is based on how the value `x` is
used and in this case only the `Assoc` and `String` variants are
present.  This "partial" type signature is checked against the bigger
`json` type it is applied to the `pretty_to_string` function, and
determined to be compatible.

<sidebar>
<title>Polymorphic variants and easier type checking</title>

One difficulty you will encounter is that type errors involving
polymorphic variants can be quite verbose if you make a mistake in
your code.  For example, suppose you build an `Assoc` and mistakenly
include a single value instead of a list of keys:

```ocaml
# let x = `Assoc ("key", `String "value");;
val x : [> `Assoc of string * [> `String of string ] ] =
  `Assoc ("key", `String "value")

# Yojson.Basic.pretty_to_string x;;
Error: This expression has type
         [> `Assoc of string * [> `String of string ] ]
       but an expression was expected of type Yojson.Basic.json
       Types for tag `Assoc are incompatible
```

The type error above isn't *wrong* as such, but can be inconvenient to
wade through for larger values.  An easy way to narrow down this sort
of type error is to add explicit type annotations as a compiler hint
about your intentions:

```ocaml
# let (x:Yojson.Basic.json) = `Assoc ("key", `String "value");;
Error: This expression has type 'a * 'b
       but an expression was expected of type
         (string * Yojson.Basic.json) list
```

In this case, we've marked the `x` as being of type
`Yojson.Basic.json`, and the compiler immediately spots that the
argument to the `Assoc` variant has the incorrect type.  This
illustrates the strengths and drawbacks of using polymorphic variants:
they make it possible to easily subtype across module boundaries
(`Basic` and `Safe` in Yojson's case), but the error messages can be
more confusing.  However, a bit of careful manual type annotation is
all it takes to make tracking down such issues much easier.

</sidebar>

#### Using non-standard JSON extensions

The standard JSON types are _really_ basic, and OCaml types are far
more expressive. Yojson supports an extended JSON format for those
times when you're not interoperating with external systems and just
want a convenient human-readable local format.  The `Yojson.Safe.json`
type is a superset of the `Basic` polymorphic variant, and looks like
this:

```ocaml
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
```

You should immediately be able to spot a benefit of using polymorphic
variants here.  A standard JSON type such as a `String` will
type-check against both the `Basic` module and also the non-standard
`Safe` module.  However, if you use extension values such as `Tuple`
with the `Basic` module, they will not be a valid sub-type and the
compiler will complain.

The extensions includes with Yojson include:

* The `lit` suffix denotes that the value is stored as a JSON
  string. For example, a `Floatlit` will be stored as `"1.234"`
  instead of `1.234`.
* The `Tuple` type is stored as `("abc", 123)` instead of a list.
* The `Variant` type encodes OCaml variants more explicitly, as
  `<"Foo">` or `<"Bar":123>` for a variant with parameters.

The only purpose of these extensions is to make the data
representation more expressive without having to refer to the original
OCaml types.  You can always cast a `Safe.json` to a `Basic.json` type
by using the `to_basic` function as follows:

```ocaml
val to_basic : json -> Yojson.Basic.json
(** Tuples are converted to JSON arrays, Variants are converted to
    JSON strings or arrays of a string (constructor) and a json value
    (argument). Long integers are converted to JSON strings.
    Examples:

    `Tuple [ `Int 1; `Float 2.3 ]   ->    `List [ `Int 1; `Float 2.3 ]
    `Variant ("A", None)            ->    `String "A"
    `Variant ("B", Some x)          ->    `List [ `String "B", x ]
    `Intlit "12345678901234567890"  ->    `String "12345678901234567890"
 *)
```

### Automatically mapping JSON to OCaml types

The combinators described earlier make it fairly easy to extract
fields from JSON records, but the process is still pretty manual.
We'll talk about how to do larger-scale JSON parsing now, using a
domain-specific language known as [ATD](http://mjambon.com/atd-biniou-intro.html).

<note>
<title>Installing the ATDgen library and tool</title>

ATDgen installs a few OCaml libraries that interface with Yojson,
and also a command-line tool that generates code.  It can all be
installed via OPAM:

```
$ opam install atdgen
$ atdgen -version
1.2.3
```

The command-line tool will be installed within your `~/.opam` directory,
and will already be on your `PATH` from running `opam config env`.  See
[xref](#installation) if this isn't working.

</note>


The idea behind ATD is to specify the format of the JSON in a separate
file, and then run a compiler (`atdgen`) that outputs OCaml code to
construct and parse JSON values.  This means that you don't need to
write any OCaml parsing code at all, as it will all be auto-generated
for you.

Let's go straight into looking at an example of how this works, by
using a small portion of the GitHub API.  GitHub is a popular code
hosting and sharing website that provides a JSON-based web
[API](http://developer.github.com).  The ATD code fragment below
describes the GitHub authorization API.  It is based on a
pseudo-standard web protocol known as OAuth, and is used to authorized
users to access GitHub services.

```ocaml
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
```

ATD is (deliberately) similar to OCaml type definitions.  Each field
can include extra annotations to customise the parsing code for a
particular backend. For example, the GitHub `scope` field above is
defined as a variant type, but with the actual JSON values being
defined explicitly (as lower-case versions).

The ATD spec can be compiled to a number of OCaml targets. Let's run
the compiler twice, to generate some OCaml type definitions, and a
JSON serialiser.

```bash
$ atdgen -t github.atd
$ atdgen -j github.atd
```

This will generate some new files in your current
directory. `Github_t.ml` and `Github_t.mli` will contain an OCaml
module with types defines that correspond to the ATD file.  It looks
like this:

```ocaml
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
```

There is an obvious correspondence to the ATD definition.  Note in
particular that field names in separate OCaml records cannot shadow
each other, and so we specifically prefix every field with a prefix to
distinguish it from other records. For example, `<ocaml
field_prefix="auth_req_">` in the ATD spec prefixes every field name
in the generated `authorization_request` record with `auth_req`.

The `Github_t` module only contains the type definitions, while
`Github_j` has a concrete serialization module to and from JSON.  You
can read the `github_j.mli` to see the full interface, but the
important functions for most uses are the conversion functions to and
from a string.  For our example above, this looks like:

```ocaml
val string_of_authorization_response :
  ?len:int -> authorization_response -> string
  (** Serialize a value of type {!authorization_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val authorization_response_of_string :
  string -> authorization_response
```

This is pretty convenient! We've written a single ATD file, and all
the OCaml boilerplate to convert between JSON and a strongly typed
record has been generated for us.  You can control various aspects of
the serializer by passing flags to `atdgen`. The important ones for
JSON are:

* `-j-std`: work in standard JSON mode, and never print non-standard
  JSON extensions.
* `-j-custom-fields FUNCTION`: call a custom function for every
  unknown field encountered, instead of raising a parsing exception.
* `-j-defaults`: force the output a JSON value even if the
  specification defines it as the default value for that field.

The full ATD specification is quite sophisticated (and well documented
online at its homepage).  The ATD compiler can also target formats
other than JSON, and also outputs code for other languages such as
Java if you need more interoperability.  There are also several
similar projects you can investigate which automate the code
generation process: [Piqi](http://piqi.org) uses the Google protobuf
format, and [Thrift](http://thrift.apache.org) supports a huge variety
of other programming languages.

We'll also return to the GitHub example here later in the book when
discussing the Async networking library, and you can find the full ATD
specification for GitHub in the
[`ocaml-github`](http://github.com/avsm/ocaml-github) repository.
