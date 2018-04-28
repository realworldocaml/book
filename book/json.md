# Handling JSON Data {#handling-json-data data-type=chapter}

Data serialization, i.e., converting data to and from a sequence of bytes
that's suitable for writing to disk or sending across the network, is an
important and common programming task. You often have to match someone else's
data format (such as XML), sometimes you need a highly efficient format, and
other times you want something that is easy for humans to edit. To this end,
OCaml libraries provide several techniques for data serialization depending
on what your problem is. [JSON data/basics of]{.idx}[serialization
formats/JSON]{.idx #SERjson}[data serialization/with JSON]{.idx #DATjson}

We'll start by using the popular and simple JSON data format and then look at
other serialization formats later in the book. This chapter introduces you to
a couple of new techniques that glue together the basic ideas from Part I of
the book by using:

- *Polymorphic variants* to write more extensible libraries and protocols
  (but still retain the ability to extend them if needed)

- *Functional combinators* to compose common operations over data structures
  in a type-safe way

- External tools to generate boilerplate OCaml modules and signatures from
  external specification files

## JSON Basics {#json-basics data-type=sect1}

JSON is a lightweight data-interchange format often used in web services and
browsers. It's described in [RFC4627](http://www.ietf.org/rfc/rfc4627.txt)
and is easier to parse and generate than alternatives such as XML. You'll run
into JSON very often when working with modern web APIs, so we'll cover
several different ways to manipulate it in this chapter. [RFC4627]{.idx}

JSON consists of two basic structures: an unordered collection of key/value
pairs, and an ordered list of values. Values can be strings, Booleans,
floats, integers, or null. Let's see what a JSON record for an example book
description looks like: [values/in JSON data]{.idx}[key/value pairs]{.idx}

<link rel="import" href="code/json/book.json" />

The outermost JSON value is usually a record (delimited by the curly braces)
and contains an unordered set of key/value pairs. The keys must be strings,
but values can be any JSON type. In the preceding example, `tags` is a string
list, while the `authors` field contains a list of records. Unlike OCaml
lists, JSON lists can contain multiple different JSON types within a single
list.

This free-form nature of JSON types is both a blessing and a curse. It's very
easy to generate JSON values, but code that parses them also has to handle
subtle variations in how the values are represented. For example, what if the
preceding `pages` value is actually represented as a string value of "
`450`" instead of an integer? [JSON data/benefits and drawbacks of]{.idx}

Our first task is to parse the JSON into a more structured OCaml type so that
we can use static typing more effectively. When manipulating JSON in Python
or Ruby, you might write unit tests to check that you have handled unusual
inputs. The OCaml model prefers compile-time static checking as well as unit
tests. For example, using pattern matching can warn you if you've not checked
that a value can be `Null` as well as contain an actual value. [Yojson
library/installation of]{.idx}[static checking]{.idx}[compile-time static
checking]{.idx}[unit tests]{.idx}

::: {data-type=note}
### Installing the Yojson Library

There are several JSON libraries available for OCaml. For this chapter, we've
picked the [Yojson](http://mjambon.com/yojson.html) library by Martin Jambon.
It's easiest to install via OPAM by running `opam install yojson`. See
[this Real World OCaml page](http://realworldocaml.org/install) for
installation instructions if you haven't already got OPAM. Once installed,
you can open it in the `utop` toplevel by:
:::


<link rel="import" href="code/json/install.mlt" part="1" />

## Parsing JSON with Yojson {#parsing-json-with-yojson data-type=sect1}

The JSON specification has very few data types, and the `Yojson.Basic.json`
type that follows is sufficient to express any valid JSON structure: [JSON
data/parsing with Yojson]{.idx}[Yojson library/parsing JSON with]{.idx}

<link rel="import" href="code/json/yojson_basic.mli" />

Some interesting properties should leap out at you after reading this
definition:

- The `json` type is *recursive*, which is to say that some of the tags refer
  back to the overall `json` type. In particular, `Assoc` and `List` types
  can contain references to further JSON values of different types. This is
  unlike the OCaml lists, whose contents must be of a uniform type.
  [recursion/in json types]{.idx}

- The definition specifically includes a `Null` variant for empty fields.
  OCaml doesn't allow null values by default, so this must be encoded
  explicitly.

- The type definition uses polymorphic variants and not normal variants. This
  will become significant later, when we extend it with custom extensions to
  the JSON format. [polymorphic variant types/in JSON data]{.idx}

Let's parse the earlier JSON example into this type now. The first stop is
the `Yojson.Basic` documentation, where we find these helpful functions:

<link rel="import" href="code/json/yojson_basic.mli" part="1" />

When first reading these interfaces, you can generally ignore the optional
arguments (which have the question marks in the type signature), since they
should have sensible defaults. In the preceding signature, the optional
arguments offer finer control over the memory buffer allocation and error
messages from parsing incorrect JSON.

The type signature for these functions with the optional elements removed
makes their purpose much clearer. The three ways of parsing JSON are either
directly from a string, from a file on a filesystem, or via a buffered input
channel:

<link rel="import" href="code/json/yojson_basic_simple.mli" />

The next example shows both the `string` and `file` functions in action,
assuming the JSON record is stored in a file called *book.json*:

<link rel="import" href="code/json/read_json/read_json.ml" />

You can build this by running `jbuilder`:

<link rel="import" href="code/json/read_json/jbuild" />

<link rel="import" href="code/json/read_json/run_read_json.sh" />

The `from_file` function accepts an input filename and takes care of opening
and closing it for you. It's far more common to use `from_string` to
construct JSON values though, since these strings come in via a network
connection (we'll see more of this in
[Concurrent Programming With Async](concurrent-programming.html#concurrent-programming-with-async){data-type=xref})
or a database. Finally, the example checks that the two input mechanisms
actually resulted in the same OCaml data structure.

## Selecting Values from JSON Structures {#selecting-values-from-json-structures data-type=sect1}

Now that we've figured out how to parse the example JSON into an OCaml value,
let's manipulate it from OCaml code and extract specific fields:
[values/selecting from JSON structures]{.idx #VALjson}[JSON data/selecting
values from]{.idx #JSONselval}

<link rel="import" href="code/json/parse_book/parse_book.ml" />

Now build and run this in the same way as the previous example:

<link rel="import" href="code/json/parse_book/jbuild" />

<link rel="import" href="code/json/parse_book/run_parse_book.sh" />

This code introduces the `Yojson.Basic.Util` module, which contains
*combinator* functions that let you easily map a JSON object into a more
strongly typed OCaml value. [combinators/functional
combinators]{.idx}[functional combinators]{.idx}

<aside data-type="sidebar">
<h5>Functional Combinators</h5>

Combinators are a design pattern that crops up quite often in functional
programming. John Hughes defines them as "a function which builds program
fragments from program fragments." In a functional language, this generally
means higher-order functions that combine other functions to apply useful
transformations over values.

You've already run across several of these in the `List` module:

<link rel="import" href="code/json/list_excerpt.mli" />

`map` and `fold` are extremely common combinators that transform an input
list by applying a function to each value of the list. The `map` combinator
is simplest, with the resulting list being output directly. `fold` applies
each value in the input list to a function that accumulates a single result,
and returns that instead:

<link rel="import" href="code/json/list_excerpt.mli" part="1" />

`iter` is a more specialized combinator that is only useful when writing
imperative code. The input function is applied to every value, but no result
is supplied. The function must instead apply some side effect such as
changing a mutable record field or printing to the standard output.

</aside>

`Yojson` provides several combinators in the `Yojson.Basic.Util` module, some
of which are listed in [Table15_1](json.html#table15_1){data-type=xref}.
[combinators/in Yojson library]{.idx}[Yojson library/combinators in]{.idx}

::: {#table15_1 data-type=table}
Function | Type | Purpose
---------|------|--------
member | `string -> json -> json` | Select a named field from a JSON record.
to_string | `json -> string` | Convert a JSON value into an OCaml `string`. Raises an exception if this is impossible.
to_int | `json -> int` | Convert a JSON value into an OCaml `int`. Raises an exception if this is impossible.
filter_string | `json list -> string list` | Filter valid strings from a list of JSON fields, and return them as an OCaml list of strings.

Table:  Yojson combinators 
:::



We'll go through each of these uses one by one now. The following examples
also use the `|>` pipe-forward operator that we explained in
[Variables And Functions](variables-and-functions.html#variables-and-functions){data-type=xref}.
This lets us chain together multiple JSON selection functions and feed the
output from one into the next one, without having to create separate 
`let` bindings for each one. [filter_string function]{.idx}[to_init
function]{.idx}[functions/to_init function]{.idx}[to_string
function]{.idx}[functions/to_string function]{.idx}[functions/member
functions]{.idx}[member function]{.idx}

Let's start with selecting a single `title` field from the record:

<link rel="import" href="code/json/parse_book.mlt" part="1" />

The `member` function accepts a JSON object and named key and returns the
JSON field associated with that key, or `Null`. Since we know that the
`title` value is always a string in our example schema, we want to convert it
to an OCaml string. The `to_string` function performs this conversion and
raises an exception if there is an unexpected JSON type. The `|>` operator
provides a convenient way to chain these operations
<span class="keep-together">together</span>:

<link rel="import" href="code/json/parse_book.mlt" part="2" />

The `tags` field is similar to `title`, but the field is a list of strings
instead of a single one. Converting this to an OCaml `string list` is a
two-stage process. First, we convert the JSON `List` to an OCaml list of JSON
values and then filter out the `String` values as an OCaml `string list`.
Remember that OCaml lists must contain values of the same type, so any JSON
values that cannot be converted to a `string` will be skipped from the output
of `filter_string`:

<link rel="import" href="code/json/parse_book.mlt" part="3" />

The `is_online` and `is_translated` fields are optional in our JSON schema,
so no error should be raised if they are not present. The OCaml type is a
`bool option` to reflect this and can be extracted via `to_bool_option`. In
our example JSON, only `is_online` is present and `is_translated` will be
`None`:

<link rel="import" href="code/json/parse_book.mlt" part="4" />

The final use of JSON combinators is to extract all the `name` fields from
the list of authors. We first construct the `author list`, and then `map` it
into a `string list`. Notice that the example explicitly binds `authors` to a
variable name. It can also be written more succinctly using the pipe-forward
operator:

<link rel="import" href="code/json/parse_book.mlt" part="5" />

This style of programming, which omits variable names and chains functions
together, is known as *point-free programming*. It's a succinct style but
shouldn't be overused due to the increased difficulty of debugging
intermediate values. If an explicit <span class="keep-together">name</span>
is assigned to each stage of the transformations, debuggers in particular
have an easier time making the program flow simpler to represent to the
programmer.

This technique of using statically typed parsing functions is very powerful
in combination with the OCaml type system. Many errors that don't make sense
at runtime (for example, mixing up lists and objects) will be caught
statically via a type error.
<a data-type="indexterm" data-startref="JSONselval">&nbsp;</a><a data-type="indexterm" data-startref="VALjson">&nbsp;</a>

## Constructing JSON Values {#constructing-json-values data-type=sect1}

Building and printing JSON values is pretty straightforward given the
`Yojson.Basic.json` type. You can just construct values of type `json` and
call the `to_string` function on them. Let's remind ourselves of the
`Yojson.Basic.json` type again: [values/in JSON data]{.idx}[JSON
data/constructing values]{.idx}

<link rel="import" href="code/json/yojson_basic.mli" />

We can directly build a JSON value against this type and use the
pretty-printing functions in the `Yojson.Basic` module to display JSON
output:

<link rel="import" href="code/json/build_json.mlt" part="1" />

In the preceding example, we've constructed a simple JSON object that
represents a single person. We haven't actually defined the type of `person`
explicitly, as we're relying on the magic of polymorphic variants to make
this all work.

The OCaml type system infers a type for `person` based on how you construct
its value. In this case, only the `Assoc` and `String` variants are used to
define the record, and so the inferred type only contains these fields
without knowledge of the other possible allowed variants in JSON records that
you haven't used yet (e.g. `Int` or `Null`):

<link rel="import" href="code/json/build_json.mlt" part="2" />

The `pretty_to_string` function has a more explicit signature that requires
an argument of type `Yojson.Basic.json`. When `person` is applied to
`pretty_to_string`, the inferred type of `person` is statically checked
against the structure of the `json` type to ensure that they're compatible:

<link rel="import" href="code/json/build_json.mlt" part="3" />

In this case, there are no problems. Our `person` value has an inferred type
that is a valid subtype of `json`, and so the conversion to a string just
works without us ever having to explicitly specify a type for `person`. Type
inference lets you write more succinct code without sacrificing runtime
reliability, as all the uses of polymorphic variants are still checked at
compile time. [errors/type errors]{.idx}[type checking]{.idx}[polymorphic
variant types/type checking and]{.idx}[type inference/benefits of]{.idx}

<aside data-type="sidebar">
<h5>Polymorphic Variants and Easier Type Checking</h5>

One difficulty you will encounter is that type errors involving polymorphic
variants can be quite verbose. For example, suppose you build an `Assoc` and
mistakenly include a single value instead of a list of keys:

<link rel="import" href="code/json/build_json.mlt" part="4" />

The type error is more verbose than it needs to be, which can be inconvenient
to wade through for larger values. You can help the compiler to narrow down
this error to a shorter form by adding explicit type annotations as a hint
about your intentions:

<link rel="import" href="code/json/build_json.mlt" part="5" />

We've annotated `person` as being of type `Yojson.Basic.json`, and as a
result, the compiler spots that the argument to the `Assoc` variant has the
incorrect type. This illustrates the strengths and weaknesses of polymorphic
variants: they're lightweight and flexible, but the error messages can be
quite confusing. However, a bit of careful manual type annotation makes
tracking down such issues much easier.

We'll discuss more techniques like this that help you interpret type errors
more easily in
[The Compiler Frontend Parsing And Type Checking](compiler-frontend.html#the-compiler-frontend-parsing-and-type-checking){data-type=xref}.

</aside>

## Using Nonstandard JSON Extensions {#using-non-standard-json-extensions data-type=sect1}

The standard JSON types are *really* basic, and OCaml types are far more
expressive. Yojson supports an extended JSON format for those times when
you're not interoperating with external systems and just want a convenient
human-readable, local format. The `Yojson.Safe.json` type is a superset of
the `Basic` polymorphic variant and looks like this: [Yojson library/extended
JSON format support]{.idx}[JSON data/nonstandard extensions for]{.idx}

<link rel="import" href="code/json/yojson_safe.mli" />

The `Safe.json` type includes all of the variants from `Basic.json` and
extends it with a few more useful ones. A standard JSON type such as a
`String` will type-check against both the `Basic` module and also the
nonstandard `Safe` module. If you use the extended values with the `Basic`
module, however, the compiler will reject your code until you make it
compliant with the portable subset of JSON.

Yojson supports the following JSON extensions: [variant types/Yojson support
for]{.idx}[tuples]{.idx}[lit suffix]{.idx}

The `lit` suffix
: Denotes that the value is stored as a JSON string. For example, a
  `Floatlit` will be stored as `"1.234"` instead of `1.234`.

The `Tuple` type
: Stored as `("abc", 123)` instead of a list.

The `Variant` type
: Encodes OCaml variants more explicitly, as `<"Foo">` or `<"Bar":123>` for a
  variant with parameters.

The only purpose of these extensions is to have greater control over how
OCaml values are represented in JSON (for instance, storing a floating-point
number as a JSON string). The output still obeys the same standard format
that can be easily exchanged with other languages.

You can convert a `Safe.json` to a `Basic.json` type by using the `to_basic`
function as follows:

<link rel="import" href="code/json/yojson_safe.mli" part="1" />

## Automatically Mapping JSON to OCaml Types {#automatically-mapping-json-to-ocaml-types data-type=sect1}

The combinators described previously make it easy to write functions that
extract fields from JSON records, but the process is still pretty manual.
When you implement larger specifications, it's much easier to generate the
mappings from JSON schemas to OCaml values more mechanically than writing
conversion functions individually. [mapping/of JSON to OCaml
types]{.idx #MAPjson}[JSON data/automatic mapping of]{.idx #JSONautomap}

We'll cover an alternative JSON processing method that is better for
larger-scale JSON handling now, using the
[OCaml](http://mjambon.com/atd-biniou-intro.html) tool. This will introduce
our first *Domain Specific Language* that compiles JSON specifications into
OCaml modules, which are then used throughout your application. [ATDgen
Library/installation of]{.idx}[Domain Specific Language]{.idx}

::: {data-type=note}
### Installing the ATDgen Library and Tool

ATDgen installs some OCaml libraries that interface with Yojson, and also a
command-line tool that generates code. It can all be installed via OPAM:
:::


<link rel="import" href="code/json/install_atdgen.rawsh" />

The command-line tool will be installed within your
<em class="filename">~/.opam</em> directory and should already be on your
`PATH` from running `opam config env`. See
[this Real World OCaml page](http://realworldocaml.org/install) if this isn't
working.

### ATD Basics {#atd-basics data-type=sect2}

The idea behind ATD is to specify the format of the JSON in a separate file
and then run a compiler (`atdgen`) that outputs OCaml code to construct and
parse JSON values. This means that you don't need to write any OCaml parsing
code at all, as it will all be autogenerated for you. [ATDgen Library/basics
of]{.idx}

Let's go straight into looking at an example of how this works, by using a
small portion of the GitHub API. GitHub is a popular code hosting and sharing
website that provides a JSON-based web [API](http://developer.github.com).
The following ATD code fragment describes the GitHub authorization API (which
is based on a pseudostandard web protocol known as OAuth): [GitHub
API]{.idx}[OAuth web protocol]{.idx}

<link rel="import" href="code/json/github.atd" />

The ATD specification syntax is deliberately quite similar to OCaml type
definitions. Every JSON record is assigned a type name (e.g., `app` in the
preceding example). You can also define variants that are similar to OCaml's
variant types (e.g., `scope` in the example).

### ATD Annotations {#atd-annotations data-type=sect2}

ATD does deviate from OCaml syntax due to its support for annotations within
the specification. The annotations can customize the code that is generated
for a particular target (of which the OCaml backend is of most interest to
us). [ATDgen Library/annotations in]{.idx}

For example, the preceding GitHub `scope` field is defined as a variant type,
with each option starting with an uppercase letter as is conventional for
OCaml variants. However, the JSON values that come back from GitHub are
actually lowercase and so aren't exactly the same as the option name.

The annotation `<json name="user">` signals that the JSON value of the field
is `user`, but that the variable name of the parsed variant in OCaml should
be `User`. These annotations are often useful to map JSON values to reserved
keywords in OCaml (e.g., `type`).

### Compiling ATD Specifications to OCaml {#compiling-atd-specifications-to-ocaml data-type=sect2}

The ATD specification we defined can be compiled to OCaml code using the
`atdgen` command-line tool. Let's run the compiler twice to generate some
OCaml type definitions and a JSON serializing module that converts between
input data and those type definitions. [ATDgen Library/compiling
specifications to OCaml]{.idx}

The `atdgen` command will generate some new files in your current directory.
`github_t.ml` and `github_t.mli` will contain an OCaml module with types
defined that correspond to the ATD file:

<link rel="import" href="code/json/build_github_atd.sh" />

There is an obvious correspondence to the ATD definition. Note that field
names in OCaml records in the same module cannot shadow one another, and so
we instruct ATDgen to prefix every field with a name that distinguishes it
from other records in the same module. For example,
`<ocaml field_prefix="auth_req_">` in the ATD spec prefixes every field name
in the generated `authorization_request` record with `auth_req`.

The `Github_t` module only contains the type definitions, while `Github_j`
provides serialization functions to and from JSON. You can read the
`github_j.mli` to see the full interface, but the important functions for
most uses are the conversion functions to and from a string. For our
preceding example, this looks like:

<link rel="import" href="code/json/github_j_excerpt.mli" />

This is pretty convenient! We've now written a single ATD file, and all the
OCaml boilerplate to convert between JSON and a strongly typed record has
been generated for us. You can control various aspects of the serializer by
passing flags to `atdgen`. The important ones for JSON are:
[-j-defaults]{.idx data-primary-sortas=j-defaults}[-j-custom-fields
FUNCTION]{.idx data-primary-sortas=j-custom}[-j-std
flag]{.idx data-primary-sortas=j-std}[flags]{.idx}

`-j-std`
: Converts tuples and variants into standard JSON and refuse to print NaN and
  infinities. You should specify this if you intend to interoperate with
  services that aren't using ATD.

`-j-custom-fields FUNCTION`
: Calls a custom function for every unknown field encountered, instead of
  raising a parsing exception.

`-j-defaults`
: Always explicitly outputs a JSON value if possible. This requires the
  default value for that field to be defined in the ATD specification.

The full [ATD specification](http://mjambon.com/atdgen/atdgen-manual.html) is
quite sophisticated and documented online. The ATD compiler can also target
formats other than JSON and outputs code for other languages (such as Java)
if you need more interoperability.

There are also several similar projects that automate the code generation
process. [Piqi](http://piqi.org) supports conversions between XML, JSON, and
the Google protobuf format; and [Thrift](http://thrift.apache.org) supports
many other programming languages and includes OCaml bindings.

### Example: Querying GitHub Organization Information {#example-querying-github-organization-information data-type=sect2}

Let's finish up with an example of some live JSON parsing from GitHub and
build a tool to query organization information via their API. Start by
looking at the online
[API documentation](http://developer.github.com/v3/orgs/) for GitHub to see
what the JSON schema for retrieving the organization information looks like.
[GitHub API]{.idx}[ATDgen Library/example of]{.idx}

Now create an ATD file that covers the fields we need. Any extra fields
present in the response will be ignored by the ATD parser, so we don't need a
completely exhaustive specification of every field that GitHub might send
back:

<link rel="import" href="code/json/github_org_info/github_org.atd" />

Let's build the OCaml type declaration first by calling `atdgen -t` on the
specification file:

<link rel="import" href="code/json/github_org_info/github_org.sh" part=
"generate json" />

The OCaml type has an obvious mapping to the ATD spec, but we still need the
logic to convert JSON buffers to and from this type. Calling `atdgen -j` will
generate this serialization code for us in a new file called
`github_org_j.ml`:

<link rel="import" href="code/json/github_org_info/github_org.sh" part=
"generate json" />

The `Github_org_j` serializer interface contains everything we need to map to
and from the OCaml types and JSON. The easiest way to use this interface is
by using the `string_of_org` and `org_of_string` functions, but there are
also more advanced low-level buffer functions available if you need higher
performance (but we won't go into that in this tutorial).

All we need to complete our example is an OCaml program that fetches the JSON
and uses these modules to output a one-line summary. Our following example
does just that.

The following code calls the cURL command-line utility by using the
`Core_extended.Shell` interface to run an external command and capture its
output. You'll need to ensure that you have cURL installed on your system
before running the example. You might also need to
`opam install core_extended` if you haven't installed it previously:

<link rel="import" href="code/json/github_org_info/github_org_info.ml" />

The following is a short shell script that generates all of the OCaml code
and also builds the final executable:

<link rel="import" href="code/json/github_org_info/jbuild" />

<link rel="import" href="code/json/github_org_info/github_org.sh" part=
"build" />

You can now run the command-line tool with a single argument to specify the
name of the organization, and it will dynamically fetch the JSON from the
web, parse it, and render the summary to your console:

<link rel="import" href="code/json/github_org_info/github_org.sh" part=
"run" />

The JSON returned from the `janestreet` query is missing an organization
name, but this is explicitly reflected in the OCaml type, since the ATD spec
marked `name` as an optional field. Our OCaml code explicitly handles this
case and doesn't have to worry about null-pointer exceptions. Similarly, the
JSON integer for the `id` is mapped into a native OCaml integer via the ATD
conversion.

While this tool is obviously quite simple, the ability to specify optional
and default fields is very powerful. Take a look at the full ATD
specification for the GitHub API in the
[`ocaml-github`](http://github.com/avsm/ocaml-github) repository online,
which has lots of quirks typical in real-world web APIs.
<a data-type="indexterm" data-startref="MAPjson">&nbsp;</a><a data-type="indexterm" data-startref="JSONautomap">&nbsp;</a><a data-type="indexterm" data-startref="SERjson">&nbsp;</a><a data-type="indexterm" data-startref="DATjson">&nbsp;</a>

Our example shells out to `curl` on the command line to obtain the JSON,
which is rather inefficient. We'll explain how to integrate the HTTP fetch
directly into your OCaml application in
[Concurrent Programming With Async](concurrent-programming.html#concurrent-programming-with-async){data-type=xref}.



