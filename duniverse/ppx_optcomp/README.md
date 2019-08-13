ppx_optcomp - Optional compilation for OCaml
============================================

ppx\_optcomp stands for Optional Compilation. It is a tool used to
handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...

The syntax is based on OCaml item extension nodes, with keywords similar to cpp.

```ocaml
[%%if ocaml_version < (4, 02, 0)]
let x = 1
[%%else]
let y = 2
[%%endif]
```

Syntax
------

ppx\_optcomp is implemented using ppx_driver and operates on ocaml AST.
This means that whole file needs to be grammatically correct ocaml.

The general syntax is:

```
[%%keyword expression]
```

Most of the statements are only supported on the toplevel. See
[grammar description](http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec248)
for detailed information where ```[%% ]``` directives may be placed.

Note in particular that the item extensions cannot be placed inside an
expression, and this would result in syntax error.
```ocaml
(* SYNTAX ERROR: let x = [%%if defined(abc) ] 1 [%%else] 2 [%%endif] *)
```

Additional syntax is provided for optional type variant declaration, as in
```ocaml
type t =
| FOO
| BAR [@if ocaml_version < (4, 02, 0)]
```

Directives
----------

### Defining variables

- `[%%define` _identifier_ _expression_`]`
- `[%%undef` _identifier_`]`

We also allow: `[%%define` _identifier_`]`. This will define
_identifier_ to `()`.  The undefined identifiers are not valid in
subsequent expressions, but for expression `defined(`_identifier_`)`,
which evaluates to false.

The scope of identifiers follows the same scoping rules as OCaml
variables. For instance:

```ocaml
(* [x] is undefined *)
[%%define x 0]
(* [x] is bound to [0] *)
module A = struct
  (* [x] is bound to [0] *)
  [%%define x 42]
  (* [x] is bound to [42] *)
end
(* [x] is bound to [0] *)
```

### Conditionals

The following directives are available for conditional compilations:

- `[%%if` _expression_`]`
- `[%%elif` _expression_`]`
- `[%%else]`
- `[%%endif]`
- `[@if` _expression_`]`

In all cases _expression_ must be an expression that evaluates to a
boolean value. Ppx\_optcomp will fail if it is not the case.

Pseudo-function `defined(`_identifier_`)` may be then used in
expressions to check whether a given identifier has been defined.
Note that identifiers that were not defined or undefined beforehand
are assumed to be a typo, and therefore are rejected, with a notable
exception of
```
[%%ifndef FOO]
[%%define FOO]
```
which is allowed even if `FOO` was not seen before.

The last form may be used only in type-variant definitions and pattern
matching, following constructors which are to be optional.  If you
need a few constructors under the same condition, you need to copy the
directive multiple times, sorry.
```
type t =
| A of int
| B of int * int [@if ocaml >= 4.04]
...

match (v: t) with
| A x -> something x
| B (y,z) [@if ocaml >= 4.04] -> something' y z
```

### Warnings and errors

`[%%warning _string_]` will cause the pre-processor to print a
message on stderr.

`[%%error _string_]` will cause the pre-processor to fail with the
following error message.

Note that in both cases _expression_ can be an arbitrary expression.

### Imports

Ppx\_optcomp allows one to import another file using:

`[%%import` _filename_`]`

where _filename_ is a string constant. Filenames to import are
resolved as follow:

- if _filename_ is relative, i.e. doesn't start with `/`, it is
  considered as relative to the directory of the file being parsed
- if _filename_ is absolute, i.e. starts with `/`, it is used as is

Only optcomp directives are allowed in the imported files. The
intended use is including some configuration variables at the
beginning of a file:

```ocaml
[%%import "config.mlh"]
```

If imported file's extension is `.h`, an alternate C-like syntax is
expected in the file.  This is to allow importing both from C and
OCaml single configuration file like:
```
#ifndef CONFIG_H
#define CONFIG_H

#define FOO
#undef BAR
#define BAZ 3*3 + 3

#endif
```


Expressions and patterns
------------------------

ppx\_optcomp supports a subset of OCaml expressions and patterns:

- literals: integers, characters and strings
- tuples
- `true` and `false`
- let-bindings
- pattern matching

And it provides the following functions:

- comparison operators: `=`, `<`, ...
- boolean operators: `||`, `&&`, `not`, ...
- arithmetic operators: `+`, `-`, `*`, `/`
- `min` and `max`
- `fst` and `snd`
- conversion functions: `to_int`, `to_string`, `to_char`, `to_bool`
- `defined`, `not_defined`: check whether a variable is defined
- `show`: act as identity, but pretty-print a value to stderr
