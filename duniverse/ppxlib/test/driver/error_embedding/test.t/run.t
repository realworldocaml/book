Keep the error output short in order to avoid different error output between
different compiler versions in the subsequent tests

  $ export OCAML_ERROR_STYLE=short

With the `-embed-errors` options, if a PPX raises, the first such exception
is caught and prepended to the last valid AST

  $ echo "let _ = [%raise]" > impl.ml
  $ ../raiser.exe -embed-errors impl.ml
  [%%ocaml.error "Raising inside the rewriter"]
  let _ = [%raise ]

The same is true when using the `-as-ppx` mode (note that the error is reported
by ocaml itself)

  $ ocaml -ppx '../raiser.exe -as-ppx' impl.ml
  File "./impl.ml", line 1, characters 8-16:
  Error: Raising inside the rewriter
  [2]

Also exceptions raised in a preprocessor get embedded into an AST(while the
error from the preprocessor's stderr also gets reported on the driver's stderr)

  $ touch file.ml
  $ ../raiser.exe -embed-errors -pp ../pp.exe file.ml | sed "s/> '.*'/> tmpfile/"
  Fatal error: exception Raising inside the preprocessor
  [%%ocaml.error
    "Error while running external preprocessor\nCommand line: ../pp.exe 'file.ml' > tmpfile\n"]

Also `unknown version` errors are embedded into an AST when using the
main standalone

  $ ../raiser.exe -embed-errors -intf unknown_version_binary_ast
  [%%ocaml.error
    "File is a binary ast for an unknown version of OCaml with magic number 'Caml1999N012'"]

... but the `-as-ppx` standalone raises them

  $ ../raiser.exe -as-ppx unknown_version_binary_ast output
  File "unknown_version_binary_ast", line 1:
  Error: The input is a binary ast for an unknown version of OCaml with magic number 'Caml1999N012'
  [1]

Similar for 'input doesn't exist' errors: they get embedded by the main standalone...

  $ ../raiser.exe -embed-errors -impl non_existing_file
  [%%ocaml.error "I/O error: non_existing_file: No such file or directory"]

... but not by the `-as-ppx` standalone

  $ ../raiser.exe -as-ppx non_existing_file output
  File "non_existing_file", line 1:
  Error: I/O error: non_existing_file: No such file or directory
  [1]
