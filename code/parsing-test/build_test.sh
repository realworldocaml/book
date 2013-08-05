ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core test.native
./test.native test1.json
