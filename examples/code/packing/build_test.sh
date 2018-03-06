  $ corebuild test.inferred.mli test.cmi
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules test.ml > test.ml.depends
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules A.ml > A.ml.depends
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules B.ml > B.ml.depends
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -thread -package core -ppx 'ppx-jane -as-ppx' -o A.cmo A.ml
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -thread -package core -ppx 'ppx-jane -as-ppx' -o B.cmo B.ml
  ocamlfind ocamlc -pack -g -bin-annot A.cmo B.cmo -o X.cmo
  ocamlfind ocamlc -i -thread -short-paths -package core -ppx 'ppx-jane -as-ppx' test.ml > test.inferred.mli
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -thread -package core -ppx 'ppx-jane -as-ppx' -o test.cmo test.ml
  $ cat _build/test.inferred.mli
  val v : string
  val w : int
  $ ocamlobjinfo _build/test.cmi
  File _build/test.cmi
  Unit name: Test
  Interfaces imported:
  	7b1e33d4304b9f8a8e844081c001ef22	Test
  	27a343af5f1904230d1edc24926fde0e	X
  	9b04ecdc97e5102c1d342892ef7ad9a2	Pervasives
  	79ae8c0eb753af6b441fe05456c7970b	CamlinternalFormatBasics
