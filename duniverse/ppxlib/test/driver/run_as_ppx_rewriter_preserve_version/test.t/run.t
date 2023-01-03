Binary AST's of any by ppxlib supported OCaml version are supported.
The version is preserved.

  $ cat 406_binary_ast | ../print_magic_number.exe
  Magic number: Caml1999N022

  $ ../identity_standalone.exe 406_binary_ast /dev/stdout | ../print_magic_number.exe
  Magic number: Caml1999N022
