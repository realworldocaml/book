ppx_sexp_message - Easy construction of s-expressions
=====================================================

Overview
--------

The aim of ppx\_sexp\_message is to ease the creation of s-expressions
in OCaml.  This is mainly motivated by writing error and debugging
messages, where one needs to construct a s-expression based on various
element of the context such as function arguments.  For instance:

```ocaml
open Core

let rename ~src ~dst =
  try Unix.rename ~src:tmpfile ~dst
  with Unix.Unix_Error (error, _, _) ->
    raise_s
      [%message
        "Error while renaming file"
          ~source:(tmpfile : string)
          ~dest:  (dst     : string)
                  (error   : Unix.Error.t)
      ]
```

would produce the following s-expression:

```scheme
("Error while renaming file"
 (source tmp/XYZ)
 (dest   blah)
 (error  ENOENT))
```

Syntax
------

Ppx\_sexp\_message expands the `[%message ...]` extension point into
an expression that evaluates to an s-expression. The grammar of the
payload is a small DSL that specifies what the generated s-expression
looks like.

### Basic syntax

Ppx\_sexp\_message recognizes the form `[%message expr1 expr2 ...]`,
and maps it pointwise to an s-expression that looks like
`((<optional_tag_1> <value_1>) ... (<optional_tag_n> <value_n>))`.
A single expression also works (but it can't syntactically be an application).

Every `expr` is mapped to an an optional tag and value as follows.

The tag is determined by the following rules:

- no tag if the labelled expression has the label `_`
- the tag is the label when the label is not `_`
- when the expression is not labelled
    - the tag is `expr` if the expression has the form `(expr : typ)`
    - otherwise there is no tag

Here are examples of each of these rules:

- `[%message "error" ~_:(msg : string)]` becomes `(error "value of msg")`
- `[%message "error" ~tag:(msg : string)]` becomes `(error (tag "value of msg"))`
- `[%message "error" (msg : string)]` becomes `(error (msg "value of msg"))`
- `[%message "error" "value of msg"]` becomes `(error "value of msg")`

Having the tag derived from the expression is by far the most common
case, since it is convenient to not have to come up with a descriptive
name. This is especially valuable for debugging messages.

The rest of this section describe how each expression is converted
to a value in the s-expression.

#### Conversion of expressions

Literals of base types (constant strings, integers, floats, ...) are
converted to their natural sexp representation.

When an expression is annotated with a type, the type is used to
convert the value exactly like `ppx_sexp_conv` does.

Otherwise, expressions are assumed to be valid ocaml expressions of
type string and the resulting string ends up directly in the
s-expression.

For instance:

- `"foo"` becomes `foo`
- `(Map.keys m : string list)` becomes `("Map.keys m" (".bashrc" ".emacs"))`
- `(sprintf "expected %s of type" ast_type)` becomes `"expected a pattern of type"`

#### Optionally displayed expression

When an expression is annotated with a type, if the type has the
syntax `a sexp_option` for some `a`, then:

- when the value is `None`, the tag and expression are omitted
- when the value is  `Some x`, then the tag and `x` are displayed

If the type annotation has the attribute `[@sexp.omit_nil]`, then when
the expression is converted into an sexp, if that sexp is `()`, then
both the tag and the expression are omitted.

One can also use `[%message.omit_nil exprs]`, which is a variation of
`[%message exprs]` which behaves as if all `option` types were
`sexp_option` and all other expressions were annotated with
`[@sexp.omit_nil]`.

#### Special case of the empty string

An exception to the previous rules is the treatment of the empty
string. An empty string will be treated as if it did not appear in the
source code. It it useful to work around syntactic limitations:

- the inability to put a label on the first expression
- the inability to tell the difference between an application and a
single expression which is an application

For instance:

```ocaml
`[%message "" ~problem:(error : Error.t)]
```

```ocaml
`[%message "" (sprintf "invalid %s" name)]
```

#### Misc

For convenience and continuity of the syntax `[%message]` becomes
`()`.

Difference with ppx\_sexp\_value
--------------------------------

Ppx\_sexp\_message is similar to
[ppx_sexp_value](https://github.com/janestreet/ppx_sexp_value) in the
sense that it makes the creation of s-expression nicer. The main
difference is that ppx\_sexp\_value is a more general rewriter that
build a s-expression based closely on what the user wrote. On the
other hand ppx\_sexp\_message tries to focus on having a DSL that is
as light as possible for building error messages.
