  $ corebuild counter.inferred.mli
  ocamlfind ocamldep -package core -ppx 'ppx-jane -as-ppx' -modules counter.ml > counter.ml.depends
  ocamlfind ocamlc -i -thread -short-paths -package core -ppx 'ppx-jane -as-ppx' counter.ml > counter.inferred.mli
  $ cat _build/counter.inferred.mli
  val touch :
    (string, int) Base__List.Assoc.t ->
    string -> (string, int) Base__List.Assoc.t
