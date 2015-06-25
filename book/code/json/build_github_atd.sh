atdgen -t github.atd
atdgen -j github.atd
ocamlfind ocamlc -package atd -i github_t.mli
