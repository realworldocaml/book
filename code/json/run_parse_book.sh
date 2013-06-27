ocamlbuild -use-ocamlfind -pkg core,yojson -tag thread parse_book.native
./parse_book.native
