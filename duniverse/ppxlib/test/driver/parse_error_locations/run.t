Keep the error output short in order to avoid different error output between
different compiler versions in the subsequent test

  $ export OCAML_ERROR_STYLE=short

Syntax errors in files parsed by ppxlib are reported correctly

  $ cat > test.ml << EOF
  > let x = 5
  > let let
  > EOF
  $ ./identity_standalone.exe -impl test.ml
  File "test.ml", line 2, characters 4-7:
  Error: Syntax error
  [1]
