# Parsing JSON and XML

## JSON

JSON is a lightweight data-interchange format often used in web services and browsers.  It is described in [RFC4627](http://www.ietf.org/rfc/rfc4627.txt),
and designed to be easy to parse and generate.

JSON consists of just two basic structures: an unordered collection of key/value pairs, and an ordered list of values.  Values can be strings, booleans, floats, integers or null.
Let's see what an example JSON record for a book description looks like:

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

JSON values usually start with an object at the top level.  The keys must be strings, but values can be any JSON type, such as the list of string tags or author record list.

This free-form nature of JSON types is both a blessing and a curse.  It's very easy to generate, but your program also has to cope with handling subtle variations in how values are represented. For example, what if the `pages` value above is actually represented as a string value of "450" instead of an integer?

Our first task is thus to parse the JSON into a more structured OCaml type so that we can use pattern matching to statically check that we've tested all the possible variations while parsing data. There are quite a few JSON parsers available for OCaml via OPAM, and we've picked [`Yojson`](http://mjambon.com/yojson.html) for the remainder of this chapter.  You can `opam install yojson` to get it via the package manager.

### Parsing standard JSON with Yojson

The basic JSON specification has very few data types, and Yojson implements these in the `Yojson.Basic` module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string 
] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This single type covers any valid JSON structure, as it is recursive and also includes a `Null` variant.  Now, let's parse the earlier JSON example and see how to manipulate the values from OCaml:

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

The first thing we do is to parse the contents of `book.json` directly from a disk file into a JSON structure. Yojson can also parse from a channel or a string, depending on where your data is coming from.

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

