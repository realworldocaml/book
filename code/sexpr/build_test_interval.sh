ocamlbuild -use-ocamlfind -tag thread -syntax camlp4o -pkgs core,sexplib.syntax test_interval.native
./test_interval.native
