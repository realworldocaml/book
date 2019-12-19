ppx_js_style - Enforce Jane Street coding styles
================================================

`ppx_js_style` is an identity ppx rewriter that enforces Jane Street coding
styles.

# Coding rules

The following rules are enforced by `ppx_js_style`:

## `-dated-deprecation`

`[@@deprecated]` attributes must contain the date of deprecation, using the
format `"[since MM-YYYY] ..."`

N.B. this check, on by default at Jane Street, but off by default externally. It
can also be disabled with the flag `-no-dated-deprecation`.

## `-allow-unannotated-ignores`

Ignored expressions must come with a type annotation, such as:
```
ignore (expr : typ)
let (_ : type) = expr
```
Note that aliases need not be annotated:
```
let _ = Foo.bar in
```

This check is enabled by default, and can be disabled by passing the flag.

## `-check-doc-comments`

Comments in mli must either be documentation comments or explicitly "ignored":
```
(** documentation comment *)
(*_ ignored comment *)
```
Normal `(* comment *)` comments are disallowed.

This flag additionally enables warning 50, which checks the placement of
documentation comments.

Finally, doc comments are checked to be syntactically valid.

## `-compat-32`

Checks that calling ocamlc on the input would produce bytecode that works on 32
bits architectures (including `js_of_ocaml`), ie that all constant are
representable on 32 bits architectures. Compared to the compiler flag by the
same name, it allows to perform this check without building any bytecode.

## `-dont-check-underscored-literal`

This check is enabled by default, and can be disabled by passing the flag.

Check for misleading usage of underscores in number literals. Currrently it
means that underscores must be:

* in positions that are multiple of 3 for numbers in decimal notation.
* in positions that are multiple of 2 for numbers in hexadecimal, octal or
binary notation.

Two consecutive underscores in a number disables the check for that number. This
is useful to turn off the check for integers that are really fixed point
decimals, like `1_000__0000` to represent `1000.0` with four implied decimal
places.

## Binding operators

Since 4.08 one can define
[binding operators](http://caml.inria.fr/pub/docs/manual-ocaml/manual046.html).
Which provide some of the features of `ppx_let`, but not all of them.
For that reason, we have made the choice to keep using `ppx_let` internally and
to forbid the use of binding operators, to keep a consistent style across our
codebase.

So uses of these operators are rejected by default at Jane Street, but allowed
externally.
This behavior can be changed with the flags:
- `-allow-let-operators`
- `-forbid-let-operators`

# Setting the flags in a jbuild file

These flags are typically passed to the ppx via the ppx_jane pps. If a developer
wanted to allow unannotated ignores, they might add the following `preprocess`
clause to their jbuild file:

```
(executables
 ((names (example_application))
  (libraries ())
  (preprocess (pps (ppx_jane -allow-unannotated-ignores)))
  ))
```
