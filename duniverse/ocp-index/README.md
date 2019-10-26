ocp-index is a light-weight tool and library providing easy access to the information contained in your OCaml cmi/cmt/cmti files.
It can be used to provide features like library interface browsing, auto-completion, show-type and goto-source.

This source tree builds:
- `ocp-index.lib`, the engine as a library
- the `ocp-index` command-line tool
- the `ocp-grep` command-line tool, that can search code for a given qualified
  OCaml ident (including uses after `open`s, etc.)
- the `ocp-browser` tool, which is a browser for installed OCaml APIs on the
  terminal

The first tree are part of opam package `ocp-index`, and the latter is in its own `ocp-browser` package.

ocp-index is part of TypeRex, developed and maintained by OCamlPro. Documentation to install and use this tool is available on http://www.typerex.org/ocp-index.html

It is released under LGPL v3 with linking exception.
