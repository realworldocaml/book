# Syntax Extensions

This chapter is going to discuss several _syntax extensions_, _i.e._,
extensions to OCaml's syntax, that are distributed with Core.  Before
diving into the details of how to use these extensions, let's take a
detour that will explain the motivation behind creating them in the
first place.

## A detour: serialization with s-expressions

Serialization, _i.e._ reading and writing program data to a sequence
of bytes, is an important and common programming task.  To this end,
Core comes with good support for _s-expressions_, which are a
convenient general-purpose serialization format with the following
type:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Sexp : sig
  type t = Atom of string | List of t list
end
~~~~~~~~~~~~~~~~~~~~~~~~

Thus, an s-expression is a nested parenthetical lists whose atomic
values are strings.  Using the `Sexp` module, you can easily parse and
print s-expressions:

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

This is somewhat tiresome to write, and it gets more so when you
consider `t_of_sexp`, which is consierably more complex.  Writing this
kind of code by hand is a mechanical process that is made error prone
by the sheer drudgery involved.

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

The syntax-extensions in Core that we're going to discuss all have
this same basic structure: they are type-driven processes for
auto-generating standardized functionality for newly declared types.

In the remainder of this chapter, we'll go into each of these syntax
extensions in more detail and discuss what they do and how to use them
effectively.

## Sexplib

Sexplib is a package for dealing with s-expressions, and it has two
parts: a library for dealing with s-expressions, which is found in the
`Sexp` module in Core, and the syntax extension, which automates the
creation of converters to and from the s-expression type itself.

Sexplib's format for s-expressions is pretty straightforward.  An
s-expression is written down as a nested parenthetical expression,
with whitespace-separated strings as the atoms.  Quotes are used for
atoms that contain parenthesis or spaces themselves, backslash is the
escape character, and semicolons are used to introduce comments.
Thus, if you create the following file:

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



