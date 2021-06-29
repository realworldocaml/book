Keep the error output short in order to avoid different error output between different compiler versions in the subsequent tests

  $ export OCAML_ERROR_STYLE=short

The rewriter gets applied when using `--as-ppx`

  $ echo "let _ = [%print_hi]" > impl.ml
  $ ocaml -ppx 'print_stuff --as-ppx' impl.ml
  hi

If a non-compatible file gets fed, the file name is reported correctly

  $ touch no_binary_ast.ml
  $ print_stuff --as-ppx no_binary_ast.ml some_output
  File "no_binary_ast.ml", line 1:
  Error: Expected a binary AST as input
  [1]

The ocaml.ppx.context attribute gets parsed correctly; in particular, the tool name gets set correctly

  $ echo "let _ = [%print_tool_name]" > impl.ml
  $ ocaml -ppx 'print_stuff --as-ppx' impl.ml
  ocaml

The driver's `shared_args` arguments are taken into account. For example, `-loc-filename`

  $ echo "let _ = [%print_fname]" > impl.ml
  $ ocaml -ppx 'print_stuff --as-ppx -loc-filename new_fn.ml' impl.ml
  new_fn.ml

or `dont-apply`

  $ echo "let _ = [%print_hi]" > impl.ml
  $ ocaml -ppx 'print_stuff --as-ppx -dont-apply test' impl.ml
  File "./impl.ml", line 1, characters 10-18:
  Error: Uninterpreted extension 'print_hi'.
  [2]
