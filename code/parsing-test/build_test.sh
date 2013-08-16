ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native
./test.native test1.json
