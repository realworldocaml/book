# Data Serialization with S-Expressions

S-expressions are nested parenthetical expressions whose atomic values
are strings. They were first popularized by the Lisp programming
language in the 1960s. They have remained one of the simplest and most
effective ways to encode structured data in a human-readable and
editable form.
[serialization formats/s-expressions]{.idx}
[s-expressions/uses for]{.idx}
[data serialization/with s-expressions]{.idx}

An example s-expression might look like this.

```
(this (is an) (s expression))
```

S-expressions play a major role in Core, effectively acting as the
default serialization format. Indeed, we've encountered s-expressions
multiple times already, including in [Error
Handling](error-handling.html#error-handling){data-type=xref},
[Functors](functors.html#functors){data-type=xref}, and [First Class
Modules](first-class-modules.html#first-class-modules){data-type=xref}.

This chapter will go into s-expressions in more depth. In particular, we'll
discuss:

- The details of the s-expression format, including how to parse it while
  generating good error messages for debugging malformed inputs

- How to generate s-expressions from arbitrary OCaml types

- How to use custom type annotations to control the exact printing behavior
  for s-expression converters

- How to integrate s-expressions into your interfaces, in particular how to
  add s-expression converters to a module without breaking abstraction
  boundaries

We'll tie this together at the end of the chapter with a simple s-expression
formatted configuration file for a web server

## Basic Usage

The type used to represent an s-expression is quite simple:
[s-expressions/basic usage of]{.idx}

```ocaml file=examples/correct/sexp/sexp.mli
module Sexp : sig
  type t =
  | Atom of string
  | List of t list
end
```

An s-expression can be thought of as a tree where each node contains a list
of its children, and where the leaves of the tree are strings. Core provides
good support for s-expressions in its `Sexp` module, including functions for
converting s-expressions to and from strings. Let's rewrite our example
s-expression in terms of this type:

```ocaml env=print_sexp
# open Core_kernel
# Sexp.List [
    Sexp.Atom "this";
    Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an"];
    Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ];
  ]
- : Sexp.t = (this (is an) (s expression))
```

This prints out nicely because Core registers a pretty printer with the
toplevel. This pretty printer is based on the functions in `Sexp` for
converting s-expressions to and from strings: [pretty printers]{.idx}

```ocaml env=sexp_printer
# Sexp.to_string (Sexp.List [Sexp.Atom "1"; Sexp.Atom "2"])
- : string = "(1 2)"
# Sexp.of_string ("(1 2 (3 4))")
- : Sexp.t = (1 2 (3 4))
```

In addition to providing the `Sexp` module, most of the base types in Core
support conversion to and from s-expressions. For example, we can use the
conversion functions defined in the respective modules for integers, strings,
and exceptions:

```ocaml env=to_from_sexp
# Int.sexp_of_t 3
- : Sexp.t = 3
# String.sexp_of_t "hello"
- : Sexp.t = hello
# Exn.sexp_of_t (Invalid_argument "foo")
- : Sexp.t = (Invalid_argument foo)
```

It's also possible to convert more complex types such as lists or
arrays that are polymorphic across the types that they can contain:

```ocaml env=to_from_sexp
# List.sexp_of_t
- : ('a -> Sexp.t) -> 'a list -> Sexp.t = <fun>
# List.sexp_of_t Int.sexp_of_t [1; 2; 3]
- : Sexp.t = (1 2 3)
```

Notice that `List.sexp_of_t` is polymorphic and takes as its first
argument another conversion function to handle the elements of the
list to be converted. Core uses this scheme more generally for
defining sexp converters for polymorphic types.

The functions that go in the other direction, *i.e.*, reconstruct an
OCaml value from an s-expression, use essentially the same trick for
handling polymorphic types, as shown in the following example. Note
that these functions will fail with an exception when presented with
an s-expression that doesn't match the structure of the OCaml type in
question.

```ocaml env=to_from_sexp
# List.t_of_sexp
- : (Sexp.t -> 'a) -> Sexp.t -> 'a list = <fun>
# List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 3)")
- : int list = [1; 2; 3]
# List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 three)")
Exception:
(Of_sexp_error "int_of_sexp: (Failure int_of_string)" (invalid_sexp three))
```

::: {data-type=note}
##### More on Top-Level Printing

The values of the s-expressions that we created were printed properly
as s-expressions in the toplevel, instead of as the tree of `Atom` and
`List` variants that they're actually made of. [top-level
printers]{.idx}

This is due to OCaml's facility for installing custom *top-level
printers* that can rewrite some values into more top-level-friendly
equivalents. They are generally installed as `ocamlfind` packages
ending in `top`:

```sh dir=examples,non-deterministic=output
$ ocamlfind list | grep top
astring.top         (version: 0.8.3)
cohttp.top          (version: n/a)
compiler-libs.toplevel (version: [distributed with Ocaml])
core.top            (version: v0.10.0)
ctypes.top          (version: 0.13.1)
findlib.top         (version: 1.7.3)
fmt.top             (version: 0.8.5)
ipaddr.top          (version: 2.8.0)
js_of_ocaml.toplevel (version: n/a)
logs.top            (version: 0.6.2)
lwt.simple-top      (version: 3.2.1)
mtime.top           (version: 1.1.0)
num-top             (version: 1.1)
ocaml-compiler-libs.toplevel (version: v0.10.0)
react.top           (version: 1.2.1)
topkg               (version: 0.9.1)
toplevel_expect_test (version: v0.10.0)
toplevel_expect_test.types (version: v0.10.0)
uri.top             (version: 1.9.6)
utop                (version: 2.1.0)
```

The `core.top` package (which you should have loaded by default in
your `.ocamlinit` file) loads in printers for the Core extensions
already, so you don't need to do anything special to use the
s-expression printer.

:::


### Generating S-Expressions from OCaml Types

But what if you want a function to convert a brand new type to an
s-expression? You can of course write it yourself manually. Here's an
example: [s-expressions/generating from OCaml types]{.idx}

```ocaml env=manually_making_sexp
# type t = { foo: int; bar: float }
type t = { foo : int; bar : float; }
# let sexp_of_t t =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ l [a "foo"; Int.sexp_of_t t.foo  ];
  l [a "bar"; Float.sexp_of_t t.bar]; ]
val sexp_of_t : t -> Sexp.t = <fun>
# sexp_of_t { foo = 3; bar = -5.5 }
- : Sexp.t = ((foo 3) (bar -5.5))
```

This is somewhat tiresome to write, and it gets more so when you consider the
parser, i.e., `t_of_sexp`, which is considerably more complex. Writing this
kind of parsing and printing code by hand is mechanical and error prone, not
to mention a drag.

Given how mechanical the code is, you could imagine writing a program
that inspects the type definition and automatically generates the
conversion code for you. As it turns out, there's a *syntax extension*
called `ppx_sexp_conv` which does just that, creating the required
functions for every type annotated with `[@@deriving sexp]`.  To
enable `ppx_sexp_conv`, we're going to enable `ppx_jane`, which is a
larger collection of useful extensions that includes `ppx_sexp_conv`.
[Sexplib package/syntax extension in]{.idx} [syntax extension/in
Sexplib package]{.idx}

```ocaml env=auto_making_sexp
# #require "ppx_jane"
```

And now we can use the extension as follows.

```ocaml env=auto_making_sexp
# type t = { foo: int; bar: float } [@@deriving sexp]
type t = { foo : int; bar : float; }
val t_of_sexp : Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))")
- : t = {foo = 3; bar = 35.}
```

The syntax extension can be used outside of type declarations as well. As
discussed in
[Error Handling](error-handling.html#error-handling){data-type=xref},
`[@@deriving sexp]` can be attached to the declaration of an exception, which will
improve the ability of Core to generate a useful string representation:

```ocaml env=auto_making_sexp
# exception Bad_message of string list
exception Bad_message of string list
# Exn.to_string (Bad_message ["1";"2";"3"])
- : string = "(\"Bad_message(_)\")"
# exception Good_message of string list [@@deriving sexp]
exception Good_message of string list
# Exn.to_string (Good_message ["1";"2";"3"])
- : string = "(//toplevel//.Good_message (1 2 3))"
```

You don't always have to declare a named type to create an s-expression
converter. The following syntax lets you create one inline, as part of a
larger expression:

```ocaml env=inline_sexp
# let l = [(1,"one"); (2,"two")]
val l : (int * string) list = [(1, "one"); (2, "two")]
# List.iter l ~f:(fun x ->
    [%sexp_of: int * string ] x
    |> Sexp.to_string
  |> print_endline)
(1 one)
(2 two)
- : unit = ()
```

The declaration `[%sexp_of: int * string]` simply gets expanded to the sexp
converter for the type `int * string`. This is useful whenever you need a
sexp converter for an anonymous type.

The syntax extensions bundled with Core almost all have the same basic
structure: they autogenerate code based on type definitions, implementing
functionality that you could in theory have implemented by hand, but with far
less programmer effort.

::: {data-type=note}
##### Syntax Extensions and PPX

OCaml doesn't directly support deriving s-expression converters from type
definitions. Instead, it provides a mechanism called *PPX* which allows you
to add to the compilation pipeline code for transforming OCaml programs at
the syntactic level, via the `-ppx` compiler flag.

PPXs operate on OCaml's *abstract syntax tree*, or AST, which is a data type
that represents the syntax of a well-formed OCaml program. Annotations like
`[%sexp_of: int]` or `[@@deriving sexp]` are part of special extensions to
the syntax, called *extension points*, which were added to the language to
give a place to put information that would be consumed by syntax extensions
like `ppx_sexp_conv`. [PPX syntax extensions]{.idx}[syntax extension with
PPX]{.idx}

`ppx_sexp_conv` is part of a family of syntax extensions, including
`ppx_compare`, described in
[Maps And Hash Tables](maps-and-hashtables.html#maps-and-hash-tables){data-type=xref},
and `ppx_fields`, described in
[Records](records.html#records){data-type=xref}, that generate code based
on type declarations. [Type_conv library]{.idx}[Sexplib package/Type_conv
library and]{.idx}

Using these extensions from a `dune` file is as simple as adding this
directive to a `(library)` or `(executable)` stanza to indicate that the
files should be run through a preprocessor:

```sexp
(executable
  (name hello)
  (preprocess (pps ppx_sexp_conv))
)
```
:::



## The Sexp Format

The textual representation of s-expressions is pretty straightforward. An
s-expression is written down as a nested parenthetical expression, with
whitespace-separated strings as the atoms. Quotes are used for atoms that
contain parentheses or spaces themselves; backslash is the escape character;
and semicolons are used to introduce single-line comments. Thus, the
following file, <em class="filename">example.scm</em>: [s-expressions/format
of]{.idx}

```
;; example.scm

((foo 3.3) ;; This is a comment
 (bar "this is () an \" atom"))
```

can be loaded using Sexplib. As you can see, the commented data is not part
of the resulting s-expression:

```ocaml env=example_load,dir=examples/sexps
# Sexp.load_sexp "example.scm"
- : Sexp.t = ((foo 3.3) (bar "this is () an \" atom"))
```

All in, the s-expression format supports three comment syntaxes:

`;`
: Comments out everything to the end of line

`#|,|#`
: Delimiters for commenting out a block

`#;`
: Comments out the first complete s-expression that follows

The following example shows all of these in action:

```
;; comment_heavy_example.scm
((this is included)
 ; (this is commented out
 (this stays)
 #; (all of this is commented
     out (even though it crosses lines.))
  (and #| block delimiters #| which can be nested |#
     will comment out
    an arbitrary multi-line block))) |#
   now we're done
   ))
```

Again, loading the file as an s-expression drops the comments:

```ocaml env=example_load,dir=examples/sexps
# Sexp.load_sexp "comment_heavy.scm"
- : Sexp.t = ((this is included) (this stays) (and now we're done))
```

If we introduce an error into our s-expression, by, say, creating a file
`broken_example.scm` which is `example.scm`, without open-paren in front of
`bar`, we'll get a parse error:

```ocaml env=example_load,dir=examples/sexps
# Exn.handle_uncaught ~exit:false (fun () ->
  ignore (Sexp.load_sexp "example_broken.scm" : Sexp.t))
Uncaught exception:

  (Sexplib.Sexp.Parse_error
   ((err_msg "unexpected character: ')'") (text_line 4) (text_char 30)
    (global_offset 78) (buf_pos 78)))

- : unit = ()
```

In the preceding example, we use `Exn.handle_uncaught` to make sure that the
exception gets printed out in full detail. You should generally wrap every
Core program in this handler to get good error messages for any unexpected
exceptions.

## Preserving Invariants

One of the most important bits of sexp-related functionality is the
autogeneration of converters for new types via `ppx_sexp_conv`. We've
seen a bit of how this works already, but let's walk through a
complete example. Here's the contents of a file `int_interval.ml`,
which is a simple library for representing integer intervals, similar
to the one described in
[Functors](functors.html#functors){data-type=xref}.
[s-expressions/preserving invariants in]{.idx}

```ocaml file=examples/correct/test_interval/int_interval.ml
(* Module for representing closed integer intervals *)
open Core

(* Invariant: For any Range (x,y), y >= x *)
type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let is_empty =
  function
  | Empty -> true
  | Range _ -> false

let create x y =
  if x > y then
    Empty
  else
    Range (x,y)

let contains i x =
  match i with
  | Empty -> false
  | Range (low,high) -> x >= low && x <= high
```

Because of the filename, the resulting module will be available under
the name `Int_interval`. We can use this module as follows.

```ocaml file=examples/correct/test_interval/test_interval.ml
open Core

let intervals =
  let module I = Int_interval in
  [ I.create 3 4;
    I.create 5 4; (* should be empty *)
    I.create 2 3;
    I.create 1 6;
  ]

let () =
  intervals
  |> List.sexp_of_t Int_interval.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
```

But we're still missing something: we haven't created an `mli` to
express the signature of `Int_interval` yet.  In doing so, we'll need
to explicitly export the s-expression converters that were created
within the `ml` file. For example, here's an interface that doesn't
export the s-expression functions:

```ocaml file=examples/erroneous/test_interval_nosexp/int_interval.mli
type t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

Building this will give us the following error:

```sh dir=examples/erroneous/test_interval_nosexp
$ dune build test_interval_nosexp.exe
File "test_interval_nosexp.ml", line 13, characters 20-42:
13 |   |> List.sexp_of_t Int_interval.sexp_of_t
                         ^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value Int_interval.sexp_of_t
[1]
```

We could export the types by hand in the signature, by writing the signatures
for the extra functions generated by Sexplib:

```ocaml file=examples/correct/int_interval_manual_sexp/int_interval.mli
open Core

type t
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

This isn't an ideal solution, as it makes you repeatedly expose these extra
functions in every signature you create where you want to serialize values.
Sexplib solves this by exposing the same syntax extension in signature
definitions so that we can just use the same `with` shorthand in the
`mli` file. Here's the final version of the signature that does just this:

```ocaml file=examples/correct/test_interval/int_interval.mli
type t [@@deriving sexp]

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

At this point, `test_interval.ml` will compile again using this `dune` file:

```scheme file=examples/correct/test_interval/dune
(executable
  (name       test_interval)
  (libraries  core sexplib)
  (preprocess (pps ppx_sexp_conv)))
```

And if we run it, we'll get the following output:

```sh dir=examples/correct/test_interval
$ dune build test_interval.exe
$ dune exec ./test_interval.exe
((Range 3 4) Empty (Range 2 3) (Range 1 6))
```

One easy mistake to make when dealing with sexp converters is to ignore the
fact that those converters can violate the invariants of your code. For
example, the `Int_interval` module depends for the correctness of the
`is_empty` check on the fact that for any value `Range (x,y)`, `y` is greater
than or equal to `x`. The `create` function preserves this invariant, but the
`t_of_sexp` function does not. [invariant checks]{.idx}

We can fix this problem by overriding the autogenerated function and writing
a custom sexp converter that wraps the autogenerated converter with whatever
invariant checks are necessary:

```ocaml file=examples/correct/int_interval_sexp_override/int_interval.ml,part=1
open Core

type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  begin match t with
    | Empty -> ()
    | Range (x,y) ->
      if y < x then of_sexp_error "Upper and lower bound of Range swapped" sexp
  end;
  t
```

This trick of overriding an existing function definition with a new one is
perfectly acceptable in OCaml. Since `t_of_sexp` is defined with an ordinary
`let` rather than a `let rec`, the call to the `t_of_sexp` goes to the
Sexplib-generated version of the function, rather than being a recursive
call.

Another important aspect of our definition is that we call the function
`of_sexp_error` to raise an exception when the parsing process fails. This
improves the error reporting that Sexplib can provide when a conversion
fails, as we'll see in the next section.

## Getting Good Error Messages

There are two steps to deserializing a type from an s-expression: first,
converting the bytes in a file to an s-expression; and the second, converting
that s-expression into the type in question. One problem with this is that it
can be hard to localize errors to the right place using this scheme. Consider
the following example: [debugging/s-expressions]{.idx}[errors/error messages
with s-expressions]{.idx}[s-expressions/deserializing a type from]{.idx}

```scheme file=examples/correct/read_foo/dune
(executable
  (name       read_foo)
  (libraries  core sexplib)
  (preprocess (pps ppx_sexp_conv)))
```



```ocaml file=examples/correct/read_foo/read_foo.ml
open Core

type t = {
  a: string;
  b: int;
  c: float option
} [@@deriving sexp]

let run () =
  let t =
    Sexp.load_sexp "foo_broken_example.scm"
    |> t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
```

If you were to run this on a malformatted file, say, this one:

``` file=examples/correct/read_foo/foo_broken_example.scm
((a "not-an-integer")
 (b "not-an-integer")
 (c 1.0))
```

you'll get the following error:

```sh dir=examples/correct/read_foo
$ dune build read_foo.exe
$ dune exec -- ./read_foo.exe foo_example_broken.scm
Uncaught exception:

  (Of_sexp_error "int_of_sexp: (Failure int_of_string)"
   (invalid_sexp not-an-integer))

...
[1]
```

If all you have is the error message and the string, it's not terribly
informative. In particular, you know that the parsing errored out on the atom
"not-an-integer," but you don't know which one! In a large file, this kind of
bad error message can be pure misery.

But there's hope! We can make a small change to the code to improve the error
message greatly:

```scheme file=examples/correct/read_foo_better_errors/dune
(executable
  (name       read_foo_better_errors)
  (libraries  core sexplib)
  (preprocess (pps ppx_sexp_conv)))
```



```ocaml file=examples/correct/read_foo_better_errors/read_foo_better_errors.ml
open Core

type t = {
  a: string;
  b: int;
  c: float option
} [@@deriving sexp]

let run () =
  let t = Sexp.load_sexp_conv_exn "foo_broken_example.scm" t_of_sexp in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
```

If we run it again, we'll see a much more specific error:

```sh dir=examples/correct/read_foo_better_errors
$ dune build read_foo_better_errors.exe
$ dune exec -- ./read_foo_better_errors.exe foo_example_broken.scm
Uncaught exception:

  (Of_sexp_error foo_broken_example.scm:2:4
   "int_of_sexp: (Failure int_of_string)" (invalid_sexp not-an-integer))

Raised at file "duniverse/sexplib.v0.13.0+dune/src/pre_sexp.ml", line 742, characters 4-56
Called from file "read_foo_better_errors.ml", line 10, characters 10-68
Called from file "duniverse/base.v0.13.2/src/exn.ml", line 102, characters 6-10
[1]
```

In the preceding error, `foo_broken_example.scm:2:5` tells us that the
error occurred in the file `"foo_broken_example.scm"` on line 2,
character 5. This is a much better start for figuring out what went
wrong. The ability to find the precise location of the error depends
on the sexp converter reporting errors using the function
`of_sexp_error`. This is already done by converters generated by
Sexplib, but you should make sure to do the same when you write custom
converters.

## Sexp-Conversion Directives

Sexplib supports a collection of directives for modifying the default
behavior of the autogenerated sexp converters. These directives allow you to
customize the way in which types are represented as s-expressions without
having to write a custom converter. [s-expressions/modifying default behavior
of]{.idx}

Note that the extra directives aren't part of the standard OCaml syntax, but
are added via the Sexplib PPX syntax extension.  You can simply activate
the preprocessor in your own `dune` files by adding `(preprocess (pps ppx_sexp_conv))`
to your build descriptions.  We've shown you some examples of complete `dune`
files with this added previously in the chapter.

### sexp_opaque {#sexp_opaque}

The most commonly used directive is `[@sexp_opaque]`, whose purpose is to mark
a given component of a type as being unconvertible. Anything marked with the
`[@sexp.opaque]` attribute will be presented as the atom `<opaque>` by the
to-sexp converter, and will trigger an exception from the from-sexp converter.
[Sexplib package/sexp_opaque]{.idx}

Note that the type of a component marked as opaque doesn't need to have a
sexp converter defined. Here, if we define a type without a sexp converter
and then try to use another type with a sexp converter, we'll error out:

```ocaml env=sexp_opaque
# type no_converter = int * int
type no_converter = int * int
# type t = { a: no_converter; b: string } [@@deriving sexp]
Line 1, characters 15-27:
Error: Unbound value no_converter_of_sexp
```

But with `[@sexp.opaque]`, we can embed our opaque `no_converter` type within
the other data structure without an error.

```ocaml env=sexp_opaque
# type t = { a: (no_converter [@sexp.opaque]); b: string } [@@deriving sexp]
type t = { a : no_converter; b : string; }
val t_of_sexp : Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexp.t = <fun>
```

And if we now convert a value of this type to an s-expression, we'll see the
contents of field `a` marked as opaque:

```ocaml env=sexp_opaque
# sexp_of_t { a = (3,4); b = "foo" }
- : Sexp.t = ((a <opaque>) (b foo))
```

Note that the `t_of_sexp` function for an opaque type is generated, but will
fail at runtime if it is used:

```ocaml env=sexp_opaque
# t_of_sexp (Sexp.of_string "((a whatever) (b foo))")
Exception:
(Of_sexp_error "opaque_of_sexp: cannot convert opaque values"
  (invalid_sexp whatever))
```

This is there to allow for s-expression converters to be created for types
containing `sexp_opaque` values. This is useful because the resulting
converters won't necessarily fail on all inputs. For example, if you have a
record containing a `no_converter list`, the `t_of_sexp` function would still
succeed when the list is empty:

```ocaml env=sexp_opaque
# type t = { a: (no_converter [@sexp.opaque]) list; b: string } [@@deriving sexp]
type t = { a : no_converter list; b : string; }
val t_of_sexp : Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((a ()) (b foo))")
- : t = {a = []; b = "foo"}
```

If you really only want to generate one direction of converter, one can do
this by annotating the type with `[@@deriving sexp_of]` or
`[@@deriving of_sexp]` instead of `[@@deriving sexp]`:

```ocaml env=sexp_opaque
# type t = { a: (no_converter [@sexp.opaque]); b: string } [@@deriving sexp_of]
type t = { a : no_converter; b : string; }
val sexp_of_t : t -> Sexp.t = <fun>
# type t = { a: (no_converter [@sexp.opaque]); b: string } [@@deriving of_sexp]
type t = { a : no_converter; b : string; }
val t_of_sexp : Sexp.t -> t = <fun>
```

### sexp.list {#sexp_list}

Sometimes, sexp converters have more parentheses than one would ideally like.
Consider, for example, the following variant type: [Sexplib
package/sexp_list]{.idx}

```ocaml env=sexp_list
# type compatible_versions =
    | Specific of string list
  | All [@@deriving sexp]
type compatible_versions = Specific of string list | All
val compatible_versions_of_sexp : Sexp.t -> compatible_versions = <fun>
val sexp_of_compatible_versions : compatible_versions -> Sexp.t = <fun>
# sexp_of_compatible_versions
  (Specific ["3.12.0"; "3.12.1"; "3.13.0"])
- : Sexp.t = (Specific (3.12.0 3.12.1 3.13.0))
```

You might prefer to make the syntax a bit less parenthesis-laden by dropping
the parentheses around the list. We can replace the `string list` in the type
declaration with `string list [@sexp.list]` to give us this alternate syntax:

```ocaml env=sexp_list
# type compatible_versions =
    | Specific of string list [@sexp.list]
  | All [@@deriving sexp]
type compatible_versions = Specific of string list | All
val compatible_versions_of_sexp : Sexp.t -> compatible_versions = <fun>
val sexp_of_compatible_versions : compatible_versions -> Sexp.t = <fun>
# sexp_of_compatible_versions
  (Specific ["3.12.0"; "3.12.1"; "3.13.0"])
- : Sexp.t = (Specific 3.12.0 3.12.1 3.13.0)
```

### sexp.option {#sexp_option}

Another common directive is `[@sexp.option]`, which is used to make a record
field optional in the s-expression. Normally, optional values are represented
either as `()` for `None`, or as `(x)` for `Some x`, and a record field
containing an option would be rendered accordingly. For example: [Sexplib
package/sexp_option]{.idx}

```ocaml env=sexp_option
# type t = { a: int option; b: string } [@@deriving sexp]
type t = { a : int option; b : string; }
val t_of_sexp : Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexp.t = <fun>
# sexp_of_t { a = None; b = "hello" }
- : Sexp.t = ((a ()) (b hello))
# sexp_of_t { a = Some 3; b = "hello" }
- : Sexp.t = ((a (3)) (b hello))
```

But what if we want a field to be optional, i.e., we want to allow it to be
omitted from the record entirely? In that case, we can mark it with
`[@sexp.option]`:

```ocaml env=sexp_option
# type t = { a: int option [@sexp.option]; b: string } [@@deriving sexp]
type t = { a : int option; b : string; }
val t_of_sexp : Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexp.t = <fun>
# sexp_of_t { a = Some 3; b = "hello" }
- : Sexp.t = ((a 3) (b hello))
# sexp_of_t { a = None; b = "hello" }
- : Sexp.t = ((b hello))
```

### Specifying Defaults

The `sexp_option` declaration is really just an example of specifying a
default behavior for dealing with an unspecified field. In particular,
`sexp_option` fills in absent fields with `None`. But you might want to allow
other ways of filling in default values. [s-expressions/specifying defaults
in]{.idx}

Consider the following type, which represents the configuration of a very
simple web server:

```ocaml env=sexp_default
# type http_server_config = {
    web_root: string;
    port: int;
    addr: string;
  } [@@deriving sexp]
type http_server_config = { web_root : string; port : int; addr : string; }
val http_server_config_of_sexp : Sexp.t -> http_server_config = <fun>
val sexp_of_http_server_config : http_server_config -> Sexp.t = <fun>
```

One could imagine making some of these parameters optional; in particular, by
default, we might want the web server to bind to port 80, and to listen as
localhost. We can do this as follows:

```ocaml env=sexp_default
# type http_server_config = {
    web_root: string;
    port: int [@default 80];
    addr: string [@default "localhost"];
  } [@@deriving sexp]
type http_server_config = { web_root : string; port : int; addr : string; }
val http_server_config_of_sexp : Sexp.t -> http_server_config = <fun>
val sexp_of_http_server_config : http_server_config -> Sexp.t = <fun>
```

Now, if we try to convert an s-expression that specifies only the `web_root`,
we'll see that the other values are filled in with the desired defaults:

```ocaml env=sexp_default
# let cfg = http_server_config_of_sexp
  (Sexp.of_string "((web_root /var/www/html))")
val cfg : http_server_config =
  {web_root = "/var/www/html"; port = 80; addr = "localhost"}
```

If we convert the configuration back out to an s-expression, you'll notice
that all of the fields are present, even though they're not strictly
necessary:

```ocaml env=sexp_default
# sexp_of_http_server_config cfg
- : Sexp.t = ((web_root /var/www/html) (port 80) (addr localhost))
```

We could make the generated s-expression also drop exported values, by using
the `sexp_drop_default` directive:

```ocaml env=sexp_default
# type http_server_config = {
    web_root: string;
    port: int [@default 80] [@sexp_drop_default.equal];
    addr: string [@default "localhost"] [@sexp_drop_default.equal];
  } [@@deriving sexp]
type http_server_config = { web_root : string; port : int; addr : string; }
val http_server_config_of_sexp : Sexp.t -> http_server_config = <fun>
val sexp_of_http_server_config : http_server_config -> Sexp.t = <fun>
# let cfg = http_server_config_of_sexp
  (Sexp.of_string "((web_root /var/www/html))")
val cfg : http_server_config =
  {web_root = "/var/www/html"; port = 80; addr = "localhost"}
# sexp_of_http_server_config cfg
- : Sexp.t = ((web_root /var/www/html))
```

As you can see, the fields that are at their default values are simply
omitted from the s-expression. On the other hand, if we convert a config with
other values, then those values will be included in the s-expression:

```ocaml env=sexp_default
# sexp_of_http_server_config { cfg with port = 8080 }
- : Sexp.t = ((web_root /var/www/html) (port 8080))
# sexp_of_http_server_config
  { cfg with port = 8080; addr = "192.168.0.1" }
- : Sexp.t = ((web_root /var/www/html) (port 8080) (addr 192.168.0.1))
```

This can be very useful in designing config file formats that are both
reasonably terse and easy to generate and maintain. It can also be useful for
backwards compatibility: if you add a new field to your config record, but
you make that field optional, then you should still be able to parse older
version of your config.
<a data-type="indexterm" data-startref="SERFORMsexp">&nbsp;</a>[files/config
files]{.idx}[config file formats]{.idx}

The exact attribute you use depends on the comparison functions available
over the type that you wish to drop:

- `[@sexp_drop_default.compare]` if the type supports `[%compare]`
- `[@sexp_drop_default.equal]` if the type supports `[%equal]`
- `[@sexp_drop_default.sexp]` if you want to compare the sexp representations
- `[@sexp_drop_default f]` and give an explicit equality function ([f = Poly.(=)] corresponds to the old behavior)

Most of the type definitions supplied with Base and Core provide the comparison
and equality operations, so those are reasonable default attributes to use.
