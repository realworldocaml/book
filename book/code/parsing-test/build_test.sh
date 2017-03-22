ln -sf ../parsing/lexer.mll ../parsing/parser.mly ../parsing/json.ml ./
ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native
./test.native test1.json
