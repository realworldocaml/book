# Handling JSON data

Data serialization, _i.e._ converting data to and from a sequence of bytes
that's suitable for writing to disk or sending across the network, is an
important and common programming task.  You often have to match someone else's
data format (such as XML), sometimes you need a highly efficient format, and
other times you want something that is easy for humans to edit.  To this end,
OCaml comes with several techniques for data serialization depending on what
your problem is.

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

## Parsing JSON with Yojson

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
```

The `from_file` function accepts an input filename and takes care of
opening and closing it for you. It's far more common to use
`from_string` to construct JSON values though, since these strings
come in via a network connection (we'll see more of this in
[xref](#concurrent-programming-with-async)) or a database. Finally,
the example checks that the two input mechanisms actually resulted in
the same OCaml data structure.

## Selecting values from JSON structures

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
programmer.

This technique of using chained parsing functions is very powerful in
combination with the OCaml type system. Many errors that don't make
sense at runtime (for example, mixing up lists and objects) will be
caught statically via a type error.

## Constructing JSON values

Building and printing JSON values is pretty straightforward given the
`Yojson.Basic.json` type.  You can just construct values of type `json` and
call the `to_string` function] on them.  Let's remind ourselves of the
`Yojson.Basic.type` again:

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

We can directly build a JSON value against this type, and use the
pretty-printing functions in the `Yojson.Basic` module to lay the
output out in the JSON format.

```ocaml
# let x = `Assoc [ ("key", `String "value") ] ;;
val x : [> `Assoc of (string * [> `String of string ]) list ] =
  `Assoc [("key", `String "value")]
```

In the example above, we've constructed a value `x` that represents a simple
JSON object.  We haven't actually defined the type of `x` explicitly here, as
we're relying on the magic of polymorphic variants to make this all work.
The OCaml type system infers a type for `x` based on how you construct the value.
In this case only the `Assoc` and `String` variants are used, and the
inferred type only contains these fields without knowledge of the other possible
variants that you haven't used yet.

```ocaml
# Yojson.Basic.pretty_to_string ;;
- : ?std:bool -> Yojson.Basic.json -> string = <fun>  
```

`pretty_to_string` has a more explicit signature that wants an argument of type
`Yojson.Basic.json`.  When `x` is applied to `pretty_to_string`, the inferred
type of `x` is statically checked against the structure of the `json` type to
ensure that they're compatible.

```ocaml
# Yojson.Basic.pretty_to_string x ;;
- : string = "{ \"key\": \"value\" }"

# Yojson.Basic.pretty_to_channel stdout x ;;
{ "key": "value" }
- : unit = ()
```

In this case, there are no problems.  Our `x` value has an inferred type that is a valid sub-type of `json`, and the function application just works without us ever having to explicitly specify a type for `x`.  Type inference lets you write more succinct code without sacrificing runtime reliability, as all the uses of polymorphic variants are still checked at compile-time.

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

The type error above is more verbose than it needs to be, which can be
inconvenient to wade through for larger values.  You can help the compiler to
narrow down this error to a shorter form by adding explicit type annotations as
a hint about your intentions.

```ocaml
# let (x:Yojson.Basic.json) = `Assoc ("key", `String "value");;
Error: This expression has type 'a * 'b
       but an expression was expected of type
         (string * Yojson.Basic.json) list
```

In this case, we've marked the `x` as being of type `Yojson.Basic.json`, and
the compiler immediately spots that the argument to the `Assoc` variant has the
incorrect type.  This illustrates the strengths and weaknesses of polymorphic
variants: they make it possible to easily subtype across module boundaries, but
the error messages can be more confusing.  However, a bit of careful manual
type annotation is all it takes to make tracking down such issues much easier.

We'll discuss more techniques like this that help you interpret type errors
more easily in [xref](#the-compiler-frontend-parsing-and-type-checking).

</sidebar>

## Using non-standard JSON extensions

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

The `Safe.json` type includes all of the variants from `Basic.json` and extends
it with a few more useful ones.  A standard JSON type such as a `String` will
type-check against both the `Basic` module and also the non-standard `Safe`
module.  If you use the extension values with the `Basic` module however, the
compiler will reject your code until you make it compliant with the
portable subset of JSON.

Yojson supports the following JSON extensions:

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

## Automatically mapping JSON to OCaml types

The combinators described earlier make it easy to write functions that extract
fields from JSON records, but the process is still pretty manual.  When you
implement larger specifications, it's much easier to generate the mappings from
JSON schemas to OCaml values more mechanically than writing conversion
functions individually.  We'll cover an alternative JSON processing method that
is better for larger-scale JSON handling now, using the
[ATD](http://mjambon.com/atd-biniou-intro.html) tool.  This will introduce our
first _Domain Specific Language_ that compiles JSON specifications into OCaml
modules, which are then used throughout your application.

<note>
<title>Installing the ATDgen library and tool</title>

ATDgen installs some OCaml libraries that interface with Yojson,
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
pseudo-standard web protocol known as OAuth, and is used to authorize
users for GitHub services.

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

ATD specifications are deliberately similar to OCaml type definitions.  Each
field can include extra annotations to customise the parsing code for a
particular backend. For example, the GitHub `scope` field above is defined as a
variant type, but with the actual JSON values being defined explicitly (as
lower-case versions).

The ATD spec can be compiled to a number of OCaml targets. Let's run
the compiler twice, to generate some OCaml type definitions, and a
JSON serializer.

```bash
$ atdgen -t github.atd
$ atdgen -j github.atd
```

This will generate some new files in your current
directory. `Github_t.ml` and `Github_t.mli` will contain an OCaml
module with types defines that correspond to the ATD file.  The
signature looks like this:

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

There is an obvious correspondence to the ATD definition.  Note
that field names in OCaml records in the same module cannot shadow
each other, and so we instruct ATDgen to prefix every field with a
name that distinguishes it from other records in the same module.
For example, `<ocaml field_prefix="auth_req_">` in the ATD spec prefixes
every field name in the generated `authorization_request` record with `auth_req`.

The `Github_t` module only contains the type definitions, while
`Github_j` provides serialization functions to and from JSON.  You
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
* `-j-defaults`: always explicitly output a JSON value if possible. This requires the default value for that field to be defined in the ATD specification.

The full ATD specification is quite sophisticated (and well documented online
at its homepage).  The ATD compiler can also target formats other than JSON,
and outputs code for other languages such as Java if you need more
interoperability.

There are also several similar projects that automate the code generation
process: [Piqi](http://piqi.org) uses the Google protobuf format, and
[Thrift](http://thrift.apache.org) supports many other programming languages
and includes OCaml bindings.

### Example: Querying Github organization information

Let's finish up with an example of some live JSON parsing from Github, and
build a tool to query organization information via their API.  Look at
the online [API documentation](http://developer.github.com/v3/orgs/) for Github
to see what the JSON schema looks like retrieving the organization information. 
Then create an ATD file that covers the fields we need.  Any extra fields
present in the response will be ignored by the ATD parser.

```
(* github_org.atd *)
type org = {
  login: string;
  id: int;
  url: string;
  ?name: string option;
  ?blog: string option;
  ?email: string option;
  public_repos: int
}
```

The OCaml program that uses this will fetch the JSON and output a one-line summary.
You'll also need the `curl` tool installed on your system to fetch the HTTP web pages, as
our example below calls `curl` via the `Core_extended.Shell` interface.

```ocaml
(* github_org_info.ml *)
open Core.Std

let print_org file () =
  let url = sprintf "https://api.github.com/orgs/%s" file in
  Core_extended.Shell.run_full "curl" [url]
  |> Github_org_j.org_of_string
  |> fun org ->
      let open Github_org_t in
      let name = Option.value ~default:"???" org.name in
      printf "%s (%d) with %d public repos\n"
        name org.id org.public_repos

let () =
  Command.basic ~summary:"Print Github organization information"
    Command.Spec.(empty +> anon ("organization" %: string))
    print_org
  |> Command.run
```

Finally, write a short shell script to generate the OCaml `Github_org` parsers
via `atdgen`, and build the OCaml command-line interface.

```console
$ cat _tags 
true: package(core,core_extended,yojson,atdgen)
true: thread, debug, annot

$ cat buildgh.sh 
#!/bin/sh

atdgen -t github_org.atd
atdgen -j github_org.atd
ocamlbuild -use-ocamlfind github_org_info.native

$ ./buildgh.sh
```

You can now run the command-line tool with a single argument to specify the name of the organization, and it will dynamically fetch the JSON from the web, parse it, and render the summary to your console.

```
$ curl https://api.github.com/orgs/janestreet 
{
  "login": "janestreet",
  "id": 3384712,
  "url": "https://api.github.com/orgs/janestreet",
  "public_repos": 31,
  "public_gists": 0,
  "followers": 0,
  "following": 0,
  "html_url": "https://github.com/janestreet",
  "created_at": "2013-01-25T19:35:43Z",
  "updated_at": "2013-05-23T14:03:06Z",
  "type": "Organization"
}
$ ./github_org_info.native mirage
Mirage account (131943) with 32 public repos
$ ./github_org_info.native janestreet
??? (3384712) with 31 public repos
```

The JSON returned from the `janestreet` query is missing an organization name,
but this is explicitly reflected in the OCaml type since the ATD spec marked
`name` as an optional field.  Our OCaml code explicitly handles this case and
doesn't have to worry about null-pointer exceptions.  Similarly, the JSON
integer for the `id` is mapped into a native OCaml integer via the ATD
conversion.

While this tool is obviously quite simple, the ability to specify optional and
default fields is very powerful.  Take a look at the full ATD specification for
the GitHub API in the [`ocaml-github`](http://github.com/avsm/ocaml-github)
repository online, which has lots of quirks typical in real-world web APIs.

Our example shells out to `curl` on the command-line to obtain the JSON, which
is rather inefficient.  We'll explain how to integrate the HTTP fetch directly
into your OCaml application later on in [xref](#concurrent-programming-with-async).
