
(rule
 (alias html)
 (target classes.html)
 (action (run rwo-convert_md  %{dep:classes/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target classes.md)
 (action (run rwo-convert_md  %{dep:classes/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target command-line-parsing.html)
 (action (run rwo-convert_md  %{dep:command-line-parsing/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target command-line-parsing.md)
 (action (run rwo-convert_md  %{dep:command-line-parsing/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target compiler-backend.html)
 (action (run rwo-convert_md  %{dep:compiler-backend/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target compiler-backend.md)
 (action (run rwo-convert_md  %{dep:compiler-backend/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target compiler-frontend.html)
 (action (run rwo-convert_md  %{dep:compiler-frontend/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target compiler-frontend.md)
 (action (run rwo-convert_md  %{dep:compiler-frontend/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target concurrent-programming.html)
 (action (run rwo-convert_md  %{dep:concurrent-programming/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target concurrent-programming.md)
 (action (run rwo-convert_md  %{dep:concurrent-programming/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target data-serialization.html)
 (action (run rwo-convert_md  %{dep:data-serialization/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target data-serialization.md)
 (action (run rwo-convert_md  %{dep:data-serialization/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target error-handling.html)
 (action (run rwo-convert_md  %{dep:error-handling/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target error-handling.md)
 (action (run rwo-convert_md  %{dep:error-handling/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target files-modules-and-programs.html)
 (action (run rwo-convert_md  %{dep:files-modules-and-programs/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target files-modules-and-programs.md)
 (action (run rwo-convert_md  %{dep:files-modules-and-programs/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target first-class-modules.html)
 (action (run rwo-convert_md  %{dep:first-class-modules/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target first-class-modules.md)
 (action (run rwo-convert_md  %{dep:first-class-modules/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target foreign-function-interface.html)
 (action (run rwo-convert_md  %{dep:foreign-function-interface/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target foreign-function-interface.md)
 (action (run rwo-convert_md  %{dep:foreign-function-interface/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target functors.html)
 (action (run rwo-convert_md  %{dep:functors/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target functors.md)
 (action (run rwo-convert_md  %{dep:functors/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target gadts.html)
 (action (run rwo-convert_md  %{dep:gadts/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target gadts.md)
 (action (run rwo-convert_md  %{dep:gadts/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target garbage-collector.html)
 (action (run rwo-convert_md  %{dep:garbage-collector/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target garbage-collector.md)
 (action (run rwo-convert_md  %{dep:garbage-collector/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target guided-tour.html)
 (action (run rwo-convert_md  %{dep:guided-tour/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target guided-tour.md)
 (action (run rwo-convert_md  %{dep:guided-tour/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target imperative-programming.html)
 (action (run rwo-convert_md  %{dep:imperative-programming/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target imperative-programming.md)
 (action (run rwo-convert_md  %{dep:imperative-programming/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target json.html)
 (action (run rwo-convert_md  %{dep:json/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target json.md)
 (action (run rwo-convert_md  %{dep:json/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target lists-and-patterns.html)
 (action (run rwo-convert_md  %{dep:lists-and-patterns/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target lists-and-patterns.md)
 (action (run rwo-convert_md  %{dep:lists-and-patterns/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target maps-and-hashtables.html)
 (action (run rwo-convert_md  %{dep:maps-and-hashtables/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target maps-and-hashtables.md)
 (action (run rwo-convert_md  %{dep:maps-and-hashtables/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target objects.html)
 (action (run rwo-convert_md  %{dep:objects/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target objects.md)
 (action (run rwo-convert_md  %{dep:objects/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target parsing-with-ocamllex-and-menhir.html)
 (action (run rwo-convert_md  %{dep:parsing-with-ocamllex-and-menhir/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target parsing-with-ocamllex-and-menhir.md)
 (action (run rwo-convert_md  %{dep:parsing-with-ocamllex-and-menhir/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target platform.html)
 (action (run rwo-convert_md  %{dep:platform/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target platform.md)
 (action (run rwo-convert_md  %{dep:platform/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target prologue.html)
 (action (run rwo-convert_md  %{dep:prologue/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target prologue.md)
 (action (run rwo-convert_md  %{dep:prologue/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target records.html)
 (action (run rwo-convert_md  %{dep:records/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target records.md)
 (action (run rwo-convert_md  %{dep:records/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target runtime-memory-layout.html)
 (action (run rwo-convert_md  %{dep:runtime-memory-layout/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target runtime-memory-layout.md)
 (action (run rwo-convert_md  %{dep:runtime-memory-layout/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target testing.html)
 (action (run rwo-convert_md  %{dep:testing/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target testing.md)
 (action (run rwo-convert_md  %{dep:testing/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target variables-and-functions.html)
 (action (run rwo-convert_md  %{dep:variables-and-functions/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target variables-and-functions.md)
 (action (run rwo-convert_md  %{dep:variables-and-functions/README.md} -t md -o %{target})))

(rule
 (alias html)
 (target variants.html)
 (action (run rwo-convert_md  %{dep:variants/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target variants.md)
 (action (run rwo-convert_md  %{dep:variants/README.md} -t md -o %{target})))
