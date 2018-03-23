  $ jbuilder build hello.exe
        ocamlc .hello.eobjs/hello.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /home/yminsky/.opam/fresh-4.06.1/bin/ocamlc.opt -w -40 -cclib -lncurses -g -bin-annot -I .hello.eobjs -I /home/yminsky/.opam/fresh-4.06.1/lib/bytes -I /home/yminsky/.opam/fresh-4.06.1/lib/ctypes -I /home/yminsky/.opam/fresh-4.06.1/lib/integers -I /home/yminsky/.opam/fresh-4.06.1/lib/ocaml/threads -no-alias-deps -o .hello.eobjs/hello.cmo -c -impl hello.ml)
  File "hello.ml", line 1, characters 5-12:
  Error: Unbound module Ncurses
@@ exit 1
