ppx_custom_printf
=================

Extensions to printf-style format-strings for user-defined string conversion.

`ppx_custom_printf` is a ppx rewriter that allows the use of
user-defined string conversion functions in format strings (that is,
strings passed to printf, sprintf, etc.).

No new syntax is introduced.  Instead a previously ill-typed use of
the `!` operator is re-purposed.

Basic Usage
-----------

The basic usage is as follows:

```ocaml
printf !"The time is %{Time} and the timezone is %{Time.Zone}."
  time zone
```

The ppx rewriter will turn the `!`-string into a format of type
`(Time.t -> Time.Zone.t -> unit, unit, string) format`. This is done
by embedding the `Time.to_string` and `Time.Zone.to_string` functions
into the format, using the low-level format mechanism of the stdlib.

In general, specifiers like `%{<Module-path>}` produce a call to
`Module-path.to_string`.  The module path can even be empty, in which
case the generated code calls `to_string`.

Note that you have to prepend the format string with a `!`, so that
the ppx rewriter knows to operate on it.

Sexps
-----

The syntax `%{sexp:<type>}` is also supported.  For example:

```ocaml
printf !"The time is %{sexp:Time.t}." time
```

The `time` argument will be turned into a string using:

```ocaml
fun x -> Sexplib.Sexp.to_string_hum ([%sexp_of: Time.t] x)
```

This supports arbitrary type expressions.

You can use `Sexplib.Sexp.to_string_mach` instead of
`Sexplib.Sexp.to_string_hum` by using `%{sexp#mach:<type>}`

Using functions other than `M.to_string`
----------------------------------------

The format specifier `%{<Module-path>.<lowercase_identifier>}`
corresponds to that function.  So, for example:

```ocaml
printf !"The date is %{Core.Date.to_string_iso8601_basic}" date
```

will turn `date` to a string using the following code:

```ocaml
fun x -> Core.Date.to_string_iso8601_basic x
```

Further, the format specifier
`%{<Module-path>#<lowercase_identifier>}` corresponds to the function
`<Module_path>.to_string_<lowercase_identifier>`.  So, for example:

```ocaml
printf !"The date is %{Core.Date#american}" date
```

will turn `date` to a string using:

```ocaml
fun x -> Core.Date.to_string_american x
```

Subformats disallowed
---------------------

In a regular format string, you can use format specifiers of the form
`%{<spec>%}` and `%(<spec>%)` where `<spec>` is another format
specifier.

Using these specifiers is disallowed in format strings that are
processed with custom-printf.
