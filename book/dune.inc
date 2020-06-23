
(rule
 (alias html)
 (target classes.html)
 (action (run rwo-convert_md  %{dep:classes/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target classes.tex)
 (action (run rwo-convert_md  %{dep:classes/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target command-line-parsing.html)
 (action (run rwo-convert_md  %{dep:command-line-parsing/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target command-line-parsing.tex)
 (action (run rwo-convert_md  %{dep:command-line-parsing/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target compiler-backend.html)
 (action (run rwo-convert_md  %{dep:compiler-backend/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target compiler-backend.tex)
 (action (run rwo-convert_md  %{dep:compiler-backend/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target compiler-frontend.html)
 (action (run rwo-convert_md  %{dep:compiler-frontend/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target compiler-frontend.tex)
 (action (run rwo-convert_md  %{dep:compiler-frontend/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target concurrent-programming.html)
 (action (run rwo-convert_md  %{dep:concurrent-programming/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target concurrent-programming.tex)
 (action (run rwo-convert_md  %{dep:concurrent-programming/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target data-serialization.html)
 (action (run rwo-convert_md  %{dep:data-serialization/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target data-serialization.tex)
 (action (run rwo-convert_md  %{dep:data-serialization/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target error-handling.html)
 (action (run rwo-convert_md  %{dep:error-handling/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target error-handling.tex)
 (action (run rwo-convert_md  %{dep:error-handling/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target files-modules-and-programs.html)
 (action (run rwo-convert_md  %{dep:files-modules-and-programs/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target files-modules-and-programs.tex)
 (action (run rwo-convert_md  %{dep:files-modules-and-programs/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target first-class-modules.html)
 (action (run rwo-convert_md  %{dep:first-class-modules/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target first-class-modules.tex)
 (action (run rwo-convert_md  %{dep:first-class-modules/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target foreign-function-interface.html)
 (action (run rwo-convert_md  %{dep:foreign-function-interface/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target foreign-function-interface.tex)
 (action (run rwo-convert_md  %{dep:foreign-function-interface/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target functors.html)
 (action (run rwo-convert_md  %{dep:functors/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target functors.tex)
 (action (run rwo-convert_md  %{dep:functors/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target garbage-collector.html)
 (action (run rwo-convert_md  %{dep:garbage-collector/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target garbage-collector.tex)
 (action (run rwo-convert_md  %{dep:garbage-collector/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target guided-tour.html)
 (action (run rwo-convert_md  %{dep:guided-tour/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target guided-tour.tex)
 (action (run rwo-convert_md  %{dep:guided-tour/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target imperative-programming.html)
 (action (run rwo-convert_md  %{dep:imperative-programming/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target imperative-programming.tex)
 (action (run rwo-convert_md  %{dep:imperative-programming/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target json.html)
 (action (run rwo-convert_md  %{dep:json/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target json.tex)
 (action (run rwo-convert_md  %{dep:json/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target lists-and-patterns.html)
 (action (run rwo-convert_md  %{dep:lists-and-patterns/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target lists-and-patterns.tex)
 (action (run rwo-convert_md  %{dep:lists-and-patterns/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target maps-and-hashtables.html)
 (action (run rwo-convert_md  %{dep:maps-and-hashtables/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target maps-and-hashtables.tex)
 (action (run rwo-convert_md  %{dep:maps-and-hashtables/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target objects.html)
 (action (run rwo-convert_md  %{dep:objects/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target objects.tex)
 (action (run rwo-convert_md  %{dep:objects/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target parsing-with-ocamllex-and-menhir.html)
 (action (run rwo-convert_md  %{dep:parsing-with-ocamllex-and-menhir/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target parsing-with-ocamllex-and-menhir.tex)
 (action (run rwo-convert_md  %{dep:parsing-with-ocamllex-and-menhir/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target ppx.html)
 (action (run rwo-convert_md  %{dep:ppx/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target ppx.tex)
 (action (run rwo-convert_md  %{dep:ppx/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target prologue.html)
 (action (run rwo-convert_md  %{dep:prologue/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target prologue.tex)
 (action (run rwo-convert_md  %{dep:prologue/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target records.html)
 (action (run rwo-convert_md  %{dep:records/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target records.tex)
 (action (run rwo-convert_md  %{dep:records/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target runtime-memory-layout.html)
 (action (run rwo-convert_md  %{dep:runtime-memory-layout/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target runtime-memory-layout.tex)
 (action (run rwo-convert_md  %{dep:runtime-memory-layout/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target testing.html)
 (action (run rwo-convert_md  %{dep:testing/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target testing.tex)
 (action (run rwo-convert_md  %{dep:testing/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target variables-and-functions.html)
 (action (run rwo-convert_md  %{dep:variables-and-functions/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target variables-and-functions.tex)
 (action (run rwo-convert_md  %{dep:variables-and-functions/README.md} -t latex -o %{target})))

(rule
 (alias html)
 (target variants.html)
 (action (run rwo-convert_md  %{dep:variants/README.md} -t html -o %{target})))

(rule
 (alias latex)
 (target variants.tex)
 (action (run rwo-convert_md  %{dep:variants/README.md} -t latex -o %{target})))
