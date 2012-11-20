# Parsing JSON and XML

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

Constructing JSON values for simply involves composing the appropriate tree and
calling the relevant output functions on the value.

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

One difficulty you might have is with long type errors if you make a mistake 
constructing the value.  For example, suppose you build an association list and
include a single value instead of a list of keys:

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

An easy way to narrow down this sort of type error is to add explicit type
annotations to give the compiler a hint as to your intentions:

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

### Stream parsing XML

TODO: XMLM, stream parsing.  Main difference from JSON is the stream parsing via signals, and tree mapping.

### Using the COW syntax extension

TODO: Explain how the p4 XML extension works. This is a good excuse.

### Working with XHTML

TODO
