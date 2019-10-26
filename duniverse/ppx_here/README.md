ppx_here
========

A ppx rewriter that defines an extension node whose value is its source position.

Syntax
------

`ppx_here` rewrites the extension `[%here]` in expressions, by
replacing it by a value of type `Source_code_position.t`
(i.e. `Lexing.position`) corresponding to the current position. It
respects line number directives.

For instance:

```ocaml
let _ =
  print_endline [%here].Lexing.pos_fname
```

becomes:

```ocaml
let _ =
  print_endline
    {
      Lexing.pos_fname = ppx/ppx_here/test/test.ml";
      pos_lnum = 2;
      pos_cnum = 26;
      pos_bol = 8;
    }.Lexing.pos_fname
```

Usage
-----

This is normally used so exceptions can contain better positions. An example is
`Core_kernel.Std.Option.value_exn`, which takes an optional position so that if you have a
stack trace, you can get still the origin of the exception.

It can also be used in cases where stack traces are useless (for instance in monads with a
complicated control flow).

Command line flag
-----------------

If the flag `-dirname <dir>` is given, relative filenames are made
relative to `<dir>`. `<dir>` can be a relative path.

`<dir>` can be chosen as the path from the root of the repository to
the directory of the source, to make filenames unique within the
repository (which avoids ambiguities as there can be many files called
`server.ml`, `common.ml` or `config.ml`).

