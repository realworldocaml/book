  $ corebuild -no-hygiene -tag principal principal.cmi non_principal.cmi
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules principal.ml > principal.ml.depends
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -principal -thread -package core -ppx 'ppx-jane -as-ppx' -o principal.cmo principal.ml
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules non_principal.ml > non_principal.ml.depends
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -principal -thread -package core -ppx 'ppx-jane -as-ppx' -o non_principal.cmo non_principal.ml
  + ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -principal -thread -package core -ppx 'ppx-jane -as-ppx' -o non_principal.cmo non_principal.ml
  [1mFile "[1mnon_principal.ml", line 6, characters 4-7[0m[0m:
  [1;35mWarning[0m 18: this type-based field disambiguation is not principal.
