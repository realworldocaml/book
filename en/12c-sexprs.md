# Data Serialization with S-Expressions

Data serialization, _i.e._ converting data to and from a sequence of
bytes that's suitable for writing to desk or sending across the
network, is an important and common programming task.  Sometimes you
need to match someone else's data format (such as XML), and other
times you just want to quickly dump some values to disk and read them
back later.  To this end, OCaml comes with several techniques for data
serialization depending on what your problem is.

We'll start by considering the question of how to serialize data in a
human-readable and editable format when you're not constrained to
using a particular third-party format.  Core's solution to this
problem is to use s-expressions

S-expressions are nested paranthetical strings whose atomic values are
strings.  They were first popularized by the Lisp programming language
in the 1960s, and have remained a simple way to encode data structures
since then.  An example s-expression might look like this:

```scheme
(this (is an) (s expression))
```

The corresponding OCaml type for an s-expression is quite simple:

```ocaml
module Sexp : sig
  type t = Atom of string | List of t list
end
```

An s-expression is in essence a nested parenthetical list whose atomic
values are strings.  The `Sexp` module in Core comes with functionality
for parsing and printing s-expressions.

```ocaml
# let sexp =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [a "this";l [a "is"; a "an"]; l [a "s"; a "expression"]];;
val sexp : Sexp.t = (this (is an) (s expression))
```

In addition, most of the base types in Core support conversion to and
from s-expressions.  For example, we can write:

```ocaml
# Int.sexp_of_t 3;;
- : Sexp.t = 3
# List.sexp_of_t;;
- : ('a -> Sexp.t) -> 'a List.t -> Sexp.t = <fun>
# List.sexp_of_t Int.sexp_of_t [1;2;3];;
- : Sexp.t = (1 2 3)
```

Notice that `List.sexp_of_t` is polymorphic, and takes as its first
argument another conversion function to handle the elements of the
list to be converted.  Core uses this scheme more generally for
defining sexp-converters for polymorphic types.

But what if you want a function to convert some brand new type to an
s-expression?  You can of course write it yourself manually:

```ocaml
# type t = { foo: int; bar: float };;
# let sexp_of_t t =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ l [a "foo"; Int.sexp_of_t t.foo  ];
        l [a "bar"; Float.sexp_of_t t.bar]; ]
  ;;
val sexp_of_t : t -> Core.Std.Sexp.t = <fun>
# sexp_of_t { foo = 3; bar = -5.5 };;
- : Core.Std.Sexp.t = ((foo 3) (bar -5.5))
```

This is somewhat tiresome to write, and it gets more so when you
consider the parser, _i.e._, `t_of_sexp`, which is considerably more
complex.  Writing this kind of parsing and printing code by hand is
mechanical and error prone, not to mention a drag.

Given how mechanical the code is, you could imagine writing a program
that inspected the type definition and auto-generated the conversion
code for you.  That is precisely where syntax extensions come in.
Using `Sexplib` (part of Core) and adding `with sexp` as an annotation to our
type definition, we get the functions we want for free.

```ocaml
# type t = { foo: int; bar: float } with sexp;;
type t = { foo : int; bar : float; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))");;
- : t = {foo = 3; bar = 35.}
```

The `with sexp` is detected by a `Sexplib` grammar extension to the normal
OCaml syntax, and replaced with the extra conversion functions you see
above. You can ignore `t_of_sexp__`, which is a helper function that is
needed in very rare cases.

The syntax extensions in Core almost all have this same basic
structure: they auto-generate code based on type definitions,
implementing functionality that you could in theory have implemented
by hand, but with far less programmer effort.

<note>
<title>The `camlp4` preprocessor and `type_conv`</title>

OCaml doesn't directly support converting static type definitions to
and from other data formats.  Instead, it supplies a powerful syntax
extension mechanism known as `camlp4`.  This lets you extend the
grammar of the language to mark types as requiring special action, and
then mechanically generate boilerplate code over those types (such as
converting to and from other data formats).

Many of the examples in the subsequent chapters depend on `camlp4`,
but the examples all invoke it automatically for you via the `-pp`
flag to the OCaml compiler.  If you're interested in building your own
generators, investigate the `type_conv` library which provides the
basic extension mechanism used by the rest of this chapter.

</note>


## The Sexp format

The textual representation of s-expressions is pretty
straightforward. An s-expression is written down as a nested
parenthetical expression, with whitespace-separated strings as the
atoms.  Quotes are used for atoms that contain parenthesis or spaces
themselves, backslash is the escape character, and semicolons are used
to introduce single-line comments.  Thus, if you create the following
`example.scm` file:

```scheme
;; example.scm

((foo 3.3) ;; Shall I compare thee to a summer's day?
 (bar "this is () an \" atom"))
```

we can load it up and print it back out again:

```ocaml
# Sexp.load_sexp "example.scm";;
- : Core.Std.Sexp.t = ((foo 3.3) (bar "this is () an \" atom"))
```

All in, the s-expression format actually supports three comment
syntaxes:

- `;`, which comments out everything to the end of a line
- `#|` and `|#`, which are delimiters for commenting out a block
- `#;`, which comments out the first complete s-expression that follows.

The following example shows all of these in action.

```scheme
;; comment_heavy_example.scm
((this is included)
 ; (this is commented out
 (this stays)
 #; (all of this is commented
     out (even though it crosses lines.))
  (and #| block delimiters will comment out
    an arbitrary multi-line block))) |#
   now we're done
   ))
```

If we load this into the toplevel, we can see what is excluded.

```ocaml
# Sexp.load_sexp "comment_heavy_example.scm";;
- : Core.Std.Sexp.t = ((this is included) (this stays) (and now we're done))
```

Note that the comments were dropped from the file upon reading.  This
is expected, since there's no place in the `Sexp.t` type to store
comments.

If we introduce an error into our s-expression, by, say, deleting the
open-paren in front of `bar`, we'll get a parse error:

```ocaml
# Exn.handle_uncaught ~exit:false (fun () ->
    ignore (Sexp.load_sexp "foo.scm"));;
  Uncaught exception:

  (Sexplib.Sexp.Parse_error
   ((location parse) (err_msg "unexpected character: ')'") (text_line 4)
    (text_char 29) (global_offset 94) (buf_pos 94)))
```

In the above, we use `Exn.handle_uncaught` to make sure that the
exception gets printed out in full detail.  You should generally wrap
every Core program in this handler to get good error messages for any
unexpected exceptions.

## Sexp converters

The most important functionality provided by Sexplib is the
auto-generation of converters for new types.  We've seen a bit of how
this works already, but let's walk through a complete example.  Here's
the source for the beginning of a library for representing integer
intervals.

```ocaml
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
```

We can now use this module as follows:

```ocaml
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
  |> List.sexp_of_t Int_interval.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
```

But we're still missing something: we haven't created an `mli` signature
for `Int_interval` yet.  Note that we need to explicitly export the
s-expression converters that were created within the ml.  If we don't:

```ocaml
(* file: int_interval.mli *)
(* Module for representing closed integer intervals *)

type t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

then we'll get the following error:

```
File "test_interval.ml", line 15, characters 20-42:
Error: Unbound value Int_interval.sexp_of_t
Command exited with code 2.
```

We could export the types by hand in the signature:

```ocaml
type t
val sexp_of_t : Sexp.t -> t
val t_of_sexp : t -> Sexp.t
```

But Sexplib has a shorthand for this as well, so that we can just
use the same `with` shorthand in the `mli` signature:

```ocaml
type t with sexp
```

at which point `test_interval.ml` will compile again, and if we run
it, we'll get the following output:

```
$ ./test_interval.native
((Range 3 4) Empty (Range 2 3) (Range 1 6))
```

<sidebar> <title>Preserving invariants</title>

One easy mistake to make when dealing with sexp converters is to
ignore the fact that those converters can violate the invariants of
your code.  For example, the `Int_interval` module depends for the
correctness of the `is_empty` check on the fact that for any value
`Range (x,y)`, `y` is greater than or equal to `x`.  The `create`
function preserves this invariant, but the `t_of_sexp` function does
not.

We can fix this problem by overriding the autogenerated function
and writing a custom sexp-converter, but still using the sexp-converter
that we already have:

```ocaml
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
```

This trick of overriding an existing function definition with a new
one is perfectly acceptable in OCaml.  Function definitions are only
recursive if the `rec` keyword is specified, and so in this case the
inner `t_of_sexp` call will go to the earlier auto-generated definition
that resulted from the `type t with sexp` definition.

We call the function `of_sexp_error` to raise an exception because
that improves the error reporting that Sexplib can provide when a
conversion fails.

</sidebar>

## Getting good error messages

There are two steps to deserializing a type from an s-expression:
first, converting the bytes in a file to an s-expression, and the
second, converting that s-expression into the type in question.  One
problem with this is that it can be hard to localize errors to the
right place using this scheme.  Consider the following example:

```ocaml
(* file: read_foo.ml *)

open Core.Std

type t = { a: string; b: int; c: float option } with sexp

let run () =
  let t =
    Sexp.load_sexp "foo.scm"
    |> t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
```

If you were to run this on a malformatted file, say, this one:

```
;; foo.scm
((a not-an-integer)
 (b not-an-integer)
 (c ()))
```

you'll get the following error:

```
read_foo $ ./read_foo.native
Uncaught exception:

  (Sexplib.Conv.Of_sexp_error
   (Failure "int_of_sexp: (Failure int_of_string)") not-an-integer)
```

If all you have is the error message and the string, it's not terribly
informative.  In particular, you know that the parsing error-ed out on
the atom "not-an-integer", but you don't know which one!  In a large
file, this kind of bad error message can be pure misery.

But there's hope!  If we make small change to the `run` function as
follows:

```ocaml
let run () =
  let t = Sexp.load_sexp_conv_exn "foo.scm" t_of_sexp in
  printf "b is: %d\n%!" t.b
```

and run it again, we'll get the following much more helpful error
message:

```ocaml
read_foo $ ./read_foo.native
Uncaught exception:

  (Sexplib.Conv.Of_sexp_error
   (Sexplib.Sexp.Annotated.Conv_exn foo.scm:3:4
    (Failure "int_of_sexp: (Failure int_of_string)"))
   not-an-integer)
```

In the above error, "foo.scm:3:4" tells us that the error occurred on
"foo.scm", line 3, character 4, which is a much better start for
figuring out what has gone wrong.

## Sexp-conversion directives

Sexplib supports a collection of directives for modifying the default
behavior of the auto-generated sexp-converters.  These directives allow
you to customize the way in which types are represented as
s-expressions without having to write a custom parser.

### `sexp-opaque`

The most commonly used directive is `sexp_opaque`, whose purpose is to
mark a given component of a type as being unconvertible.  Anything
marked with `sexp_opaque` will be presented as the atom `<opaque>` by
the to-sexp converter, and will trigger an exception from the
from-sexp converter.  Note that the type of a component marked as
opaque doesn't need to have a sexp-converter defined.  Here, if we
define a type without a sexp-converter, and then try to use it another
type with a sexp-converter, we'll error out:

```ocaml
# type no_converter = int * int;;
type no_converter = int * int
# type t = { a: no_converter; b: string } with sexp;;
Characters 14-26:
  type t = { a: no_converter; b: string } with sexp;;
                ^^^^^^^^^^^^
Error: Unbound value no_converter_of_sexp
```

But with `sexp_opaque`, we won't:

```ocaml
# type t = { a: no_converter sexp_opaque; b: string } with sexp;;
type t = { a : no_converter Core.Std.sexp_opaque; b : string; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
```

And if we now convert a value of this type to an s-expression, we'll
see the contents of field `a` marked as opaque:

```ocaml
# sexp_of_t { a = (3,4); b = "foo" };;
- : Sexp.t = ((a <opaque>) (b foo))
```

### `sexp_option`

Another common directive is `sexp_opaque`, which is used to make an
optional field in a record.  Ordinary optional values are represented
either as `()` for `None`, or as `(x)` for `Some x`.  If you put an
option in a record field, then the record field will always be
required, and its value will be presented in the way an ordinary
optional value would.  For example:

```ocaml
# type t = { a: int option; b: string } with sexp;;
# sexp_of_t { a = None; b = "hello" };;
- : Sexp.t = ((a ()) (b hello))
# sexp_of_t { a = Some 3; b = "hello" };;
- : Sexp.t = ((a (3)) (b hello))
```

But what if we want a field to be optional, _i.e._, we want to allow
it to be omitted from the record entirely?  In that case, we can mark
it with `sexp_option`:

```ocaml
# type t = { a: int sexp_option; b: string } with sexp;;
# sexp_of_t { a = Some 3; b = "hello" };;
- : Sexp.t = ((a 3) (b hello))
# sexp_of_t { a = None; b = "hello" };;
- : Sexp.t = ((b hello))
```

### `sexp_list`

One problem with the auto-generated sexp-converters is that they can
have more parentheses than one would ideally like.  Consider, for
example, the following variant type:

```ocaml
# type compatible_versions = | Specific of string list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"]);;
- : Sexp.t = (Specific (3.12.0 3.12.1 3.13.0))
```

You might prefer to make the syntax a bit less parenthesis-laden by
dropping the parentheses around the list.  `sexp_list` gives us this
alternate syntax:

```ocaml
# type compatible_versions = | Specific of string sexp_list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"]);;
- : Sexp.t = (Specific 3.12.0 3.12.1 3.13.0)
```
