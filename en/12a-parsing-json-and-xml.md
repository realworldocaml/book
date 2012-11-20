# Parsing JSON and XML

## JSON

JSON is a lightweight data-interchange format often used in web services and
browsers.  It is described in [RFC4627](http://www.ietf.org/rfc/rfc4627.txt),
and is designed to be easy to parse and generate.  You'll run into JSON very
often when working with modern APIs, and so we'll cover several different
ways to manipulate it in this chapter. Along the way we'll introduce several
new libraries and syntax extensions which make the job easier.

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
we can use static typing more effectively.  For example, pattern matching can
statically check at compilation time that we've tested all the possible variations
of a given field, instead of relying on runtime unit tests (as you might in Python
or Ruby).

There are several JSON parsers available for OCaml, and we've picked
[`Yojson`](http://mjambon.com/yojson.html) for the remainder of this chapter.
You can `opam install yojson` to get it via the OPAM package manager.
 
### Parsing standard JSON with Yojson

The JSON specification has very few data types, and Yojson implements
these in the `Yojson.Basic` module.  A single `json` type is sufficient to
express any valid JSON structure. Note that it is recursive (so that
sub-elements can be further JSON fields), and also includes a `Null` variant
for empty fields.

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

Let's parse the earlier JSON string example into this type now.  We'll first
look at the `Yojson.Basic` documentation, and find these helpful functions:

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

When reading these interfaces, you can generally ignore the optional arguments
(with the question marks in the type) at first glance, as they will be filled
in with sensible values.  The simpler signature for these values with the
optional elements removed makes their purpose quite clear:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val from_string : string -> json
val from_file : string -> json
val from_channel : in_channel -> json
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The [in_channel] constructor is from the original OCaml standard library, and
its use is consider deprecated when using the Core standard library.  This
leaves us with two ways of parsing the JSON, so let's see what they look like.
The example below assumes the JSON fragment is in a file called *book.json*:

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`from_file` is a convenience function in Yojson that accepts an input filename
and takes care of closing it for you. It's far more common to construct JSON
values from `string` buffers that you obtain from a network connection, for
example (we'll see more of this in in the {{{ASYNC}}} chapter). Finally, the
example checks that the two input mechanisms did in fact have result in the
same OCaml data structure.

<sidebar>
<title>The difference between `=` and `==`</title>

If you come from a C/C++ background, you will probably reflexively use `==`
to test two values for equality. In OCaml, `==` tests for *physical* equality,
and `=` tests for *structural* equality. This has some important implications
for your code's performance.

The `==` physical equality test will not match if you have two data structures
that are have the same contents but were allocated separately. In the JSON
parsing example above, they would not match.

The `=` structural equality operator will inspect each field in the value and
test them separately for equality. In the JSON parsing example, this means that
every JSON value will be traversed and checked, and thus they will check out as
equal.  However, if you data structure is cyclical (that is, a value
recursively points back to another field within the same structure), the `=`
operator will never terminate.  In this situation, you must use the physical
equality operator, or write a custom comparison function that breaks the
recursion.

</sidebar>

WIP

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

Next, we open the `Yojson.Basic.Util` module.  This contains *combinator* functions that can be chained together using the Core `|!` pipe operator to select and convert values out of the JSON structure.  Let's examine some of their uses in a bit more detail:

* For the `title` string, the `member` function extracts the key from the array, and casts it to an OCaml string. An exception is raised if the JSON value is not a string.
* The `tags` are similar to the `title`, but are passed through the `to_list` function since they are a list of strings.  The `filter_string` function maps all of the strings in the JSON list into an OCaml list (and filters out any non-string values in the JSON).
* The `is_online` and `is_translated` functions are optional, and no error is raised if they are not present in the JSON array. The resulting OCaml type is a `string option` to reflect this. In our example, only `is_online` is present and `is_translated` will be `None`.

Finally, we print the parsed fields just as we would normal OCaml values. This technique of using chained parsing functions is very powerful in combination with the OCaml type system. Many errors that don't make sense at runtime (for example, mixing up lists and objects) will be caught statically via a type error.

### Using JSON extensions for richer types

The basic JSON types  can sometimes be a little too limited for expressing more complex data structures. Yojson also offers a more advanced module which extends the basic JSON types with some useful extras.  These should *not* be used when interoperating with external services, but are useful within your own applications.

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

The extensions here include the following:

* The `lit` suffix denotes that the value is stored as a JSON string. For example, a `Floatlit` might be stored as `"1.234"` instead of `1.234`.
* The `Tuple` type is stored as `("abc", 123)` instead of a list.
* The `Variant` type encodes OCaml variants more explicitly, as ` <"Foo">` or `<"Bar":123>`
 
_avsm_:  What's the purpose of these extensions? See next on ATDgen

### Using ATDgen to automatically convert JSON into an OCaml type

