Keep the error output short in order to avoid different error output between
different compiler versions in the subsequent tests

  $ export OCAML_ERROR_STYLE=short

The registered rewriters get applied when using `run_as_ppx_rewriter` as entry point

  $ cat > file.ml << EOF
  > let () = [%print_hi]
  > let () = [%print_bye]
  > EOF
  $ ocaml -ppx '../print_greetings.exe' file.ml
  hi
  bye

The driver's `shared_args` are taken into account, such as `-apply`...

  $ ocaml -ppx '../print_greetings.exe -apply print_hi' file.ml
  hi
  File "./file.ml", line 2, characters 11-20:
  Error: Uninterpreted extension 'print_bye'.
  [2]

... and `-check`

  $ echo "[@@@attr non_registered_attr]" > attribute_file.ml
  $ ocaml -ppx '../print_greetings.exe -check' attribute_file.ml
  File "./attribute_file.ml", line 1, characters 4-8:
  Error: Attribute `attr' was not used
  [2]


If a non-compatible file gets fed, the file name is reported correctly

  $ touch no_binary_ast.ml
  $ ../print_greetings.exe no_binary_ast.ml some_output
  File "no_binary_ast.ml", line 1:
  Error: Expected a binary AST as input
  [1]

The only possible usage is [extra_args] <infile> <outfile>...

  $ ../print_greetings.exe some_input
  Usage: print_greetings.exe [extra_args] <infile> <outfile>
  [2]

...in particular the order between the flags and the input/output matters.

  $ touch some_output
  $ ../print_greetings.exe some_input some_output -check
  ../print_greetings.exe: anonymous arguments not accepted.
  print_greetings.exe [extra_args] <infile> <outfile>
    -loc-filename <string>      File name to use in locations
    -reserve-namespace <string> Mark the given namespace as reserved
    -no-check                   Disable checks (unsafe)
    -check                      Enable checks
    -no-check-on-extensions     Disable checks on extension point only
    -check-on-extensions        Enable checks on extension point only
    -no-locations-check         Disable locations check only
    -locations-check            Enable locations check only
    -apply <names>              Apply these transformations in order (comma-separated list)
    -dont-apply <names>         Exclude these transformations
    -no-merge                   Do not merge context free transformations (better for debugging rewriters)
    -cookie NAME=EXPR           Set the cookie NAME to EXPR
    --cookie                    Same as -cookie
    -help                       Display this list of options
    --help                      Display this list of options
  [2]

The only exception is consulting help

  $ ../print_greetings.exe -help
  print_greetings.exe [extra_args] <infile> <outfile>
    -loc-filename <string>      File name to use in locations
    -reserve-namespace <string> Mark the given namespace as reserved
    -no-check                   Disable checks (unsafe)
    -check                      Enable checks
    -no-check-on-extensions     Disable checks on extension point only
    -check-on-extensions        Enable checks on extension point only
    -no-locations-check         Disable locations check only
    -locations-check            Enable locations check only
    -apply <names>              Apply these transformations in order (comma-separated list)
    -dont-apply <names>         Exclude these transformations
    -no-merge                   Do not merge context free transformations (better for debugging rewriters)
    -cookie NAME=EXPR           Set the cookie NAME to EXPR
    --cookie                    Same as -cookie
    -help                       Display this list of options
    --help                      Display this list of options

Binary AST's of any by ppxlib supported OCaml version are supported.
The version is preserved.

  $ cat 406_binary_ast | ../print_magic_number.exe
  Magic number: Caml1999N022

  $ ../print_greetings.exe 406_binary_ast /dev/stdout | ../print_magic_number.exe
  Magic number: Caml1999N022
