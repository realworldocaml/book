# Preprocessing with ppx

One powerful feature in OCaml is a facility to extend the standard language via
_extension points_.  These represent placeholders in the OCaml syntax tree and are
ignored by the standard compiler tooling, beyond being delimited and stored in
the abstract syntax tree alongside the normal parsed source code. They are
intended to be expanded by external tools that select extension nodes that can
interpret them.  The external tools can choose to generate further OCaml code
by transforming the input syntax tree, thus forming the basis of an extensible
preprocessor for the language.

There are two primary forms of extension points in OCaml: _attributes_ and
_extension nodes_.  Let's first run through some examples of what they look
like, and then see how to use them in your own code.

## Extension Attributes

Attributes supply additional information that is attached to a node in the OCaml
syntax tree, and subsequently interpreted and expanded by external tools. 

The basic form of an attribute is the `[@ ... ]` syntax.  The number of `@` symbols
defines which part of the syntax tree the attribute is bound to:

- a single `[@` binds to expressions and individual type definitions.
- a double `[@@` binds to blocks of code, such as module definitions, type declarations or class fields.
- a triple `[@@@` appears as a standalone entry in a module implementation or
 signature, and are not tied to any specific source code node.

The OCaml compiler has some useful builtin attributes that we can use to
illustrate their use without requiring any external tools.  Let's first look
at the use of the standalone attribute `@@@warning` to toggle an OCaml
compiler warning.


```ocaml env=main
# module Abc = struct

  [@@@warning "+10"]
  let a = Sys.get_argv (); ()

  [@@@warning "-10"]
  let b = Sys.get_argv (); ()
  end
Line 4, characters 11-26:
Warning 10: this expression should have type unit.
module Abc : sig val a : unit val b : unit end
```

The warning number in our example is taken from the
[compiler manual page](https://caml.inria.fr/pub/docs/manual-ocaml/native.html).
In this case, warning 10 emits a message if the expression in a sequence
doesn't have type `unit`.  The `@@@warning` nodes in the module implementation
cause the compiler to change its behaviour within the scope of that structure only.

An annotation can also be more narrowly attached to a block of code.  For example,
a module implementation can be annotated with `@@deprecated` to indicate that it
should not be used in new code:

```ocaml env=main
# module Planets = struct
    let earth = true
    let pluto = true
  end [@@deprecated "Sorry, Pluto is no longer a planet. Use the Planets2016 module instead."]
module Planets : sig val earth : bool val pluto : bool end
# module Planets2016 = struct
    let earth = true
    let pluto = false
  end
module Planets2016 : sig val earth : bool val pluto : bool end
```

In this example, the `@@deprecated` annotation is only attached to the
`Planets` module, and the human-readable argument string redirects developers
to the newer code.  Now if we try to use the value that has been marked as
deprecated, the compiler will issue a warning.

```ocaml env=main
# let is_pluto_a_planet = Planets.pluto
Line 1, characters 25-38:
Alert deprecated: module Planets
Sorry, Pluto is no longer a planet. Use the Planets2016 module instead.
val is_pluto_a_planet : bool = true
# let is_pluto_a_planet = Planets2016.pluto
val is_pluto_a_planet : bool = false
```

Finally, an attribute can also be attached to an individual expression.  In the
next example, the `@warn_on_literal_pattern` attribute indicates that the
argument to the type constructor should not be pattern matched upon with a
constant literal.

```ocaml env=main
# type program_result =
  | Error of string [@warn_on_literal_pattern]
  | Exit_code of int
type program_result = Error of string | Exit_code of int
# let exit_with = function
  | Error "It blew up" -> 1
  | Exit_code code -> code
  | Error _ -> 100
Line 2, characters 11-23:
Warning 52: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 9.5)
val exit_with : program_result -> int = <fun>
```

#### Commonly used extension attributes

We have already used extension points in [Data Serialization With S
Expressions](data-serialization.html#data-serialization-with-s-expressions){data-type=xref}
to generate boilerplate code for handling s-expressions.

TODO

### Extension Nodes

While extension points are useful for annotating existing source
code, we also need a mechanism to store generic placeholders
within the OCaml AST for code generation.  OCaml provides this
facility via the *extension node* syntax.

The general syntax for an extension node is `[%id expr]`, where
`id` is an identifier for a particular extension node rewriter
and `expr` is the payload for the rewriter to parse.

## Using ppx extensions

### ppx in the compiler


## Building your first ppx extension

### The AST Mapper and hello world

- replace `[%calc 1+2]` with a constant integer

### ppxlib

Why use ppxlib over direct use of AST-mapper


