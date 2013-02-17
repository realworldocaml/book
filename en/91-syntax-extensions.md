# Syntax Extensions

_(yminsky: still very very rough)_

This chapter covers several extensions to OCaml's syntax that are
distributed with Core.  Before diving into the details of the syntax
extensions, let's take a small detour that will explain the motivation
behind creating them in the first place.

## Serialization with s-expressions

Serialization, _i.e._ reading and writing program data to a sequence
of bytes, is an important and common programming task.  To this end,
Core comes with good support for _s-expressions_, which are a
convenient general-purpose serialization format.  The type of an
s-expression is as follows:

```ocaml
module Sexp : sig
  type t = Atom of string | List of t list
end
```

An s-expression is in essence a nested parenthetical list whose atomic
values are strings.  The `Sexp` module comes with functionality for
parsing and printing s-expressions.

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
s-expression?  You can of course write it yourself:

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
Using Sexplib and adding `with sexp` as an annotation to our type
definition, we get the functions we want for free.

```ocaml
# type t = { foo: int; bar: float } with sexp;;
type t = { foo : int; bar : float; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))");;
- : t = {foo = 3; bar = 35.}
```

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

```
;; foo.scm

((foo 3.3) ;; Shall I compare thee  to a summer's dream?
 (bar "this is () an \" atom"))
```

we can load it up and print it back out again:

```ocaml
# Sexp.load_sexp "foo.scm";;
- : Sexp.t = ((foo 3.3) (bar "this is () an \" atom"))
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

(In the above, we use `Exn.handle_uncaught` to make sure that the
exception gets printed out in full detail.)

### Sexp converters

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
  |! List.sexp_of_t Int_interval.sexp_of_t
  |! Sexp.to_string_hum
  |! print_endline
```

But we're still missing something: we haven't created an `mli` for
`Int_interval` yet.  Note that we need to explicitly export the
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

We could export the types by hand:

```ocaml
type t
val sexp_of_t : Sexp.t -> t
val t_of_sexp : t -> Sexp.t
```

But Sexplib has a shorthand for this as well, so that we can instead
write simply:

```ocaml
type t with sexp
```

at which point `test_interval.ml` will compile again, and if we run
it, we'll get the following output:

```
$ ./test_interval.native
((Range 3 4) Empty (Range 2 3) (Range 1 6))
```

<note> <title>Preserving invariants</title>

One easy mistake to make when dealing with sexp converters is to
ignore the fact that those converters can violate the invariants of
your code.  For example, the `Int_interval` module depends for the
correctness of the `is_empty` check on the fact that for any value
`Range (x,y)`, `y` is greater than or equal to `x`.  The `create`
function preserves this invariant, but the `t_of_sexp` function does
not.

We can fix this problem by writing a custom sexp-converter, in this
case, using the sexp-converter that we already have:

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

We call the function `of_sexp_error` to raise an exception because
that improves the error reporting that Sexplib can provide when a
conversion fails.

</note>

### Getting good error messages

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
    |! t_of_sexp
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

#### `sexp_option`

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

#### `sexp_list`

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
unsightly in the toplevel because of all the definitions that are
generated.  We'll elide those definitions here, but you can see it for
yourself in the toplevel.)

Here's a small complete example of a program that can read and write
values using bin-io.  Here, the serialization is of types that might
be used as part of a message-queue, where each message has a topic,
some content, and a source, which is in turn a hostname and a port.

```ocaml
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
```

## Fieldslib

One common idiom when using records is to provide field accessor
functions for a particular record.

```ocaml
type t = { topic: string;
           content: string;
           source: Source.t;
         }

let topic   t = t.topic
let content t = t.content
let source  t = t.source
```

Similarly, sometimes you simultaneously want an accessor to a field of
a record and a textual representation of the name of that field.  This
might come up if you were validating a field and needed the string
representation to generate an error message, or if you wanted to
scaffold a form in a GUI automatically based on the fields of a
record.  Fieldslib provides a module `Field` for this purpose.  Here's
some code for creating `Field.t`'s for all the fields of our type `t`.

```ocaml
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
```

