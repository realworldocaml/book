# Syntax Extensions

_(yminsky: still very very rough)_

This chapter convers several extensions to OCaml's syntax that are
distributed with Core.  Before diving into the details of the syntax
extensions, let's take a small detour that will explain the motivation
behind creating them in the first place.

## Serialization with s-expressions

Serialization, _i.e._ reading and writing program data to a sequence
of bytes, is an important and common programming task.  To this end,
Core comes with good support for _s-expressions_, which are a
convenient general-purpose serialization format.  The type of an
s-expression is as follows:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Sexp : sig
  type t = Atom of string | List of t list
end
~~~~~~~~~~~~~~~~~~~~~~~~

An s-expression is in essence a nested parenthetical list whose atomic
values are strings.  The `Sexp` module comes with functionality for
parsing and printing s-expressions.

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let sexp = Sexp.of_string "(This (is an) (s expression))";;
val sexp : Sexp.t =
Sexp.List
 [Sexp.Atom "This";
  Sexp.List [Sexp.Atom "is"; Sexp.Atom "an"];
  Sexp.List [Sexp.Atom "s"; Sexp.Atom "expression"]]
# Sexp.to_string_hum (Sexp.List [sexp;sexp]);;
- : string = "((This (is an) (s expression)) (This (is an) (s expression)))"
~~~~~~~~~~~~~~~~~~~~~~~~

In addition, most of the base types in Core support conversion to and
from s-expressions.  For example, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int.sexp_of_t 3
- : Sexp.t = Sexp.Atom "3"
# List.sexp_of_t;;
- : ('a -> Sexp.t) -> 'a List.t -> Sexp.t = <fun>
# Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t [1;2;3]);;
- : string = "(1 2 3)"
~~~~~~~~~~~~~~~~~~~~~~~~

Notice that `List.sexp_of_t` is polymorphic, and takes as its first
argument another conversion function to handle the elements of the
list to be converted.  Core uses this scheme more generally for
defining sexp-converters for polymorphic types.

But what if you want a function to convert some brand new type to an
s-expression?  You can of course write it yourself:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float };;
type t = { foo : int; bar : float; }
# let sexp_of_t t =
    Sexp.List [ Sexp.List [Sexp.Atom "foo"; Int.sexp_of_t t.foo];
                Sexp.List [Sexp.Atom "bar"; Float.sexp_of_t t.bar]; ]
  ;;
# Sexp.to_string_hum (sexp_of_t { foo = 3; bar = -5.5 });;
- : string = "((foo 3) (bar -5.5))"
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
# Sexp.load_sexp "foo.scm" |! Sexp.to_string_hum |! print_endline ;;
((foo 3.3) (bar "this is () an \" atom"))
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the comments were dropped from the file upon reading.  This
is expected, since there's no place in the `Sexp.t` type to store
comments.

If we introduce an error into our s-expression, by, say, deleting the
open-paren in front of `bar`, we'll get a parse error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Exn.handle_uncaught ~exit:false (fun () ->
    Sexp.load_sexp "foo.scm" |! Sexp.to_string_hum |! print_endline);;
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
right place using this scheme.  Consider the following example type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: read_foo.ml *)

open Core.Std

type u = Foo | Bar with sexp
type t = { a: u; b: int; c: float option }
with sexp

let run () =
  let t =
    Sexp.load_sexp "foo.scm"
    |! t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
~~~~~~~~~~~~~~~~~~~~~~~~~~~



### Sexp-conversion directives

Sexplib supports a collection of directives for modifying the default
behavior of the autogenerated sexp-converters.  These directives allow
you to customize the way in which types are represented as
s-expressions without having to write a custom parser.  We describe
these directives below.

#### `sexp-opaque`

The most commonly used directive is `sexp_opaque`, whose purpose is to
mark a given component of a type as being unconvertable.  Anything
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
# { a = (3,4); b = "foo" } |! sexp_of_t |! Sexp.to_string_hum;;
- : string = "((a <opaque>) (b foo))"
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
# { a = None; b = "hello" } |! sexp_of_t |! Sexp.to_string_hum;;
- : string = "((a ()) (b hello))"
# { a = Some 3; b = "hello" } |! sexp_of_t |! Sexp.to_string_hum;;
- : string = "((a (3)) (b hello))"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But what if we want a field to be optional, _i.e._, we want to allow
it to be omitted from the record entirely?  In that case, we can mark
it with `sexp_option`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type t = { a: int sexp_option; b: string } with sexp;;
# { a = Some 3; b = "hello" } |! sexp_of_t |! Sexp.to_string_hum;;
- : string = "((a 3) (b hello))"
# { a = None; b = "hello" } |! sexp_of_t |! Sexp.to_string_hum;;
- : string = "((b hello))"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### `sexp_list`

One problem with the autogenerated sexp-converters is that they can
have more parens than one would ideally like.  Consider, for example,
the following variant type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type compatible_versions = | Specific of string list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"])
  |! Sexp.to_string_hum;;
- : string = "(Specific (3.12.0 3.12.1 3.13.0))"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might prefer to make the syntax a bit less parenthesis-laden by
dropping the parens around the list.  `sexp_list` gives us this
alternate syntax:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type compatible_versions = | Specific of string sexp_list
                             | All
  with sexp;;
# sexp_of_compatible_versions (Specific ["3.12.0"; "3.12.1"; "3.13.0"])
  |! Sexp.to_string_hum;;
- : string = "(Specific 3.12.0 3.12.1 3.13.0)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~
