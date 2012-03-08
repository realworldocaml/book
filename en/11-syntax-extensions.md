# Syntax Extensions

This chapter is going to discuss several _syntax extensions_, _i.e._,
extensions to OCaml's syntax, that are distributed with Core.  But
before diving into the details of how these syntax extensions work,
let's take a detour that will explain the motivation behind creating
these extensions in the first place.

## A detour: Serialization

Programmers spend a lot of time creating machine and human readable
formats for serializing and deserializing program data.  One
convenient general-purpose serialization format that is well supported
in Core is the _s-expression_, which has the following type:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Sexp : sig
  type t = Atom of string | List of t list
end
~~~~~~~~~~~~~~~~~~~~~~~~

Thus, an s-expression is a nested parenthetical lists whose atomic
values are strings.  Core has a lot of s-expression related
functionality, such as functions for parsing and printing
s-expressions.

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

S-expressions are a convenient data serialization format, and most of
the base types in Core support conversion to and from s-expressions.
For example, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int.sexp_of_t 3
- : Sexp.t = Sexp.Atom "3"
# List.sexp_of_t;;
- : ('a -> Sexp.t) -> 'a List.t -> Sexp.t = <fun>
# Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t [1;2;3]);;
- : string = "(1 2 3)"
~~~~~~~~~~~~~~~~~~~~~~~~

Notice that `List.sexp_of_t` is polymorphic, and takes as its first
argument another conversion function to handle the elements of
whatever list it is to convert.  Core uses these scheme more generally
for defining sexp-converters for polymorphic types.

But what if you want a function to convert some brand new type to an
s-expression?  You can of course write it yourself:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float };;
type t = { foo : int; bar : float; }
# let sexp_of_t t =
    Sexp.List [ Sexp.List [Sexp.Atom "foo"; Int.sexp_of_t t.foo];
                Sexp.List [Sexp.Atom "bar"; Float.sexp_of_t t.bar]; ]
  ;; 
# Sexp.to_string (sexp_of_t { foo = 3; bar = -5.5 });;
- : string = "((foo 3)(bar -5.5))"
~~~~~~~~~~~~~~~~~~~~~~~~

This is somewhat tiresome to write, and you can imagine it gets more
so when you also write the `t_of_sexp` function, which is the more
complicated function to write.  Writing this kind of code by hand for
many large and complex types is a mechanical process that is made
error prone by the sheer drudgery involved.

Given how mechanical the code is, you could imagine writing a program
that inspected the type definition and auto-generated the conversion
code for you.  That is precisely where syntax extensions come in.  If
you turn on the `sexplib` syntax extension in Core, then we can add
the `with sexp` annotation to our type definition, and sexplib will do
the rest.

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float } with sexp;;
type t = { foo : int; bar : float; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))");;
- : t = {foo = 3; bar = 35.}
~~~~~~~~~~~~~~~~~~~~~~~~

(You can ignore the value `t_of_sexp__`, which is a helper function
that is needed in very rare cases.)

The syntax-extensions in Core that we're going to discuss have this
same basic structure: they all involve creating new functionality
based on the definition of a type.  In the remainder of this chapter,
we'll go into each of these syntax extensions in more detail and
discuss what they do and how to use them effectively.

It's worth noting that all of these syntax extensions are optional.
You can use Core without them with no real penalty.  But they're also
highly valuable, and you would do well to add them to your OCaml
toolkit.

## Sexplib

Sexplib is a package for dealing with s-expressions that was developed
for and is tightly integrated with Core, although it can be used
independently from Core as well.  Sexplib itself really has two parts:
a library for dealing with s-expressions, which is mostly found in the
module `Sexp` in Core, and the syntax extension, which automates the
creation of converters to and from the s-expression type itself.

Sexplib's format for s-expressions is pretty straightforward.  A valid
s-expression needs to be a balanced parenthetical statements, and by
default atoms are separted by spaces.  You can use quotes for atoms
that contain parenthesis or spaces themselves, and backslash is an
escape character.  A semicolon is used to introduce comments.  Thus,
if you create the following file:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; foo.scm

((foo 3.3) ;; Shall I compare thee  to a summer foo?
 (bar "this is () an \" atom"))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

we can load it up and print it back out again:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Sexp.load_sexp "foo.scm" |! Sexp.to_string_hum |! print_endline ;;
((foo 3.3) (bar "this is () an \" atom"))
- : unit = ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the comments were dropped from the file upon reading.  This
is because there's no place in the `Sexp.t` type to store comments.

If we introduce an error in our s-expression, say, by deleting the
open-paren in front of `bar`, we'll get a syntax error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Exn.handle_uncaught ~exit:false (fun () -> 
    Sexp.load_sexp "foo.scm" |! Sexp.to_string_hum |! print_endline);;
  Uncaught exception:
  
  (Sexplib.Sexp.Parse_error
   ((location parse) (err_msg "unexpected character: ')'") (text_line 4)
    (text_char 29) (global_offset 94) (buf_pos 94)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~


~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Sexp.load_sexp "foo.scm" |! Sexp.to_string_hum |! print_endline ;;
Exception: Pre_sexp.Parse_error _.
~~~~~~~~~~~~~~~~~~~~~~~~~~~
This exception is rather uninformative, but we can get a more detailed
message if we run this bit of code in a different way:


