  $ ocamlopt -runtime-variant d -verbose -o hello.native hello.ml
  + clang -arch x86_64 -Wno-trigraphs -c -o 'hello.o' '/var/folders/9g/7vjfw6kn7k9bs721d_zjzn7h0000gn/T/camlasm9b916c.s'
  + clang -arch x86_64 -Wno-trigraphs -c -o '/var/folders/9g/7vjfw6kn7k9bs721d_zjzn7h0000gn/T/camlstartup8f1c0d.o' '/var/folders/9g/7vjfw6kn7k9bs721d_zjzn7h0000gn/T/camlstartupf69d9a.s'
  + cc -O2 -fno-strict-aliasing -fwrapv -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT -DCAML_NAME_SPACE   -Wl,-no_compact_unwind -o 'hello.native'   '-L/Users/thomas/git/rwo/book/_opam/lib/ocaml'  '/var/folders/9g/7vjfw6kn7k9bs721d_zjzn7h0000gn/T/camlstartup8f1c0d.o' '/Users/thomas/git/rwo/book/_opam/lib/ocaml/std_exit.o' 'hello.o' '/Users/thomas/git/rwo/book/_opam/lib/ocaml/stdlib.a' '/Users/thomas/git/rwo/book/_opam/lib/ocaml/libasmrund.a' 
  $ ./hello.native
  ### OCaml runtime: debug mode ###
  Initial minor heap size: 256k words
  Initial major heap size: 3840k bytes
  Initial space overhead: 80%
  Initial max overhead: 500%
  Initial heap increment: 15%
  Initial allocation policy: 0
  Initial smoothing window: 1
  Hello OCaml World!
