# Syntax Extensions

This chapter is going to discuss several language extensions for OCaml
that are distributed with Core as syntax extensions.  But before
diving into the details of how OCaml syntax extensions work, let's
talk a detour that will explain the motivation behind building them.

## A detour: serialization

Serialization is an extremely common programming task.  You will often
want to write out datatype that your program generates in some machine
and human readable form.  One convenient serialization format that is
well supported in Core is the _s-expression_, which has the following
type:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Sexp : sig
  type t = Atom of string | List of t list
end
~~~~~~~~~~~~~~~~~~~~~~~~

S-expressions are basically nested parenthetical lists, where the
atomic values are strings.  OCaml has functions for parsing and
writing s-expressions:

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
# Int.sexp_of_t
- : Sexp.t = Sexp.Atom "3"
# List.sexp_of_t;;
- : ('a -> Sexp.t) -> 'a List.t -> Sexp.t = <fun>
# Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t [1;2;3]);;
- : string = "(1 2 3)"
~~~~~~~~~~~~~~~~~~~~~~~~

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

And you can imagine similar serialization code for other data types
that you might invent.  It goes without saying that writing this kind
of code by hand for many large and complex types is a mechanical
process that is made error prone by the sheer drudgery involved.

Given how very mechanical the code is, what you really want is
something that will automatically write this code for you.  And that
is where syntax extensions can come in.  If you turn on the `sexplib`
syntax extension in Core, then you can change your defintion of `t` as
follows, to get the s-expression converters for `t` for free:

~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# type t = { foo: int; bar: float };;
# type t = { foo: int; bar: float } with sexp;;
type t = { foo : int; bar : float; }
val t_of_sexp__ : Sexplib.Sexp.t -> t = <fun>
val t_of_sexp : Sexplib.Sexp.t -> t = <fun>
val sexp_of_t : t -> Sexplib.Sexp.t = <fun>
# t_of_sexp (Sexp.of_string "((bar 35) (foo 3))");;
- : t = {foo = 3; bar = 35.}
~~~~~~~~~~~~~~~~~~~~~~~~


