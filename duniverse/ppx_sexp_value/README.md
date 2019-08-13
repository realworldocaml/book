ppx_sexp_value
==============

A ppx rewriter that simplifies building s-expressions from ocaml values.

Basic use
---------

The building block of this preprocessor is the extension:

```ocaml
[%sexp expr]
```

in expressions. It evaluates to an s-expression. This is done by
recursing down the expression and converting all data constructors
into s-expressions. If an expression with a type annotation is found,
then the type is used to convert to s-expression. Expressions that are
neither data constructors nor annotated with a type will be rejected.

For instance:

```ocaml
[%sexp { a = "hello" ; b = (Time.now () : Time.t) } ]
```

will be preprocessed into:

```ocaml
List [List [Atom "a"; Atom "hello"];
      List [Atom "b"; [%sexp_of: Time.t] (Time.now ())];
     ]
```

This does not require a record with fields a and b to exist (and if
one does exist, its sexp_of function will be ignored unless a type
annotation is added around the record).
One can annotate a record field with the type `sexp_option` or with
`[@sexp.omit_nil]` to achieve the same result as when using
sexplib. They both have the same behavior inside tuples.

Variant, polymorphic variants, tuples and lists are supported as
well.  Variants are analogous to records in that a type containing the
variant does not have to exist unless a type annotation is added to
the variant.

Expressions with their evaluations
----------------------------------

It is sometimes convenient to include the expression itself in the
s-expression. This is especially true for debugging, as it avoids
having to think of a label for a value; one can simply use the
expression itself.

Ppx\_sexp\_value allows this by reserving the `~~` operator. Inside
`[%sexp]`, `~~<expr>` is the same as `("<expr>", <expr>)`, where the
type annotation in `<expr>` is stripped off in the s-expression
(`~~<expr>` isn't allowed if `<expr>` is a data constructor).

For instance:

```ocaml
[%sexp (~~(x : int), ~~(y + z : int), "literal") ]
```

will be preprocessed into:

```ocaml
List [List [Atom "x";     [%sexp_of: int] x];
      List [Atom "y + z"; [%sexp_of: int] (y + z)];
      [%sexp_of: string] "literal";
     ]
```

Recommended use for errors
--------------------------

This extension is primarily intended to build better errors, by making
building errors easier and more readable, particularly when using
records to add context:

```ocaml
try Unix.rename ~src:tmpfile ~dst
with exn ->
  raise_s
    [%sexp
     "Error while renaming file",
      { source = (tmpfile : string)
      ; dest   = (dst     : string)
      ; exn    = (exn     : exn   )
      }
    ]
```
