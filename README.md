These scripts form the compilation pipeline for Real World OCaml.  They aren't
really designed to be useful outside of the book toolchain, but the plan is to
factor out the useful bits eventually.

The following binaries work against the
[examples](http://github.com/realworldocaml/examples) repository:

* `rwo-run-toplevel`: Runs a toplevel script in a non-interactive
   toplevel, and outputs the result as HTML and Markdown.
* `rwo-syntax-highlight`: Called from the `code` Makefile to convert
   a code fragment into HTML and Markdown, either via direct parsing
   using [COW](http://github.com/mirage/ocaml-cow) or
   [Pygments](http://pygments.org).

The following binaries are used by the internal book toolchain to
turn it into an O'Reilly PDF and the online website.  They're definitely
not for external consumption until the full toolchain is published.

* `rwo-html-code-highlight`: Substitute code fragments in DocBook XHTML
   chunked output with the right HTML ones from the code repository.

