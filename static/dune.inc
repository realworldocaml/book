
(rule
 (alias site)
 (target classes.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/classes.html})))

(rule
 (alias site)
 (target command-line-parsing.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/command-line-parsing.html})))

(rule
 (alias site)
 (target compiler-backend.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/compiler-backend.html})))

(rule
 (alias site)
 (target compiler-frontend.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/compiler-frontend.html})))

(rule
 (alias site)
 (target concurrent-programming.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/concurrent-programming.html})))

(rule
 (alias site)
 (target data-serialization.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/data-serialization.html})))

(rule
 (alias site)
 (target error-handling.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/error-handling.html})))

(rule
 (alias site)
 (target files-modules-and-programs.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/files-modules-and-programs.html})))

(rule
 (alias site)
 (target first-class-modules.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/first-class-modules.html})))

(rule
 (alias site)
 (target foreign-function-interface.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/foreign-function-interface.html})))

(rule
 (alias site)
 (target functors.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/functors.html})))

(rule
 (alias site)
 (target garbage-collector.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/garbage-collector.html})))

(rule
 (alias site)
 (target guided-tour.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/guided-tour.html})))

(rule
 (alias site)
 (target imperative-programming.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/imperative-programming.html})))

(rule
 (alias site)
 (target json.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/json.html})))

(rule
 (alias site)
 (target lists-and-patterns.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/lists-and-patterns.html})))

(rule
 (alias site)
 (target maps-and-hashtables.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/maps-and-hashtables.html})))

(rule
 (alias site)
 (target objects.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/objects.html})))

(rule
 (alias site)
 (target parsing-with-ocamllex-and-menhir.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/parsing-with-ocamllex-and-menhir.html})))

(rule
 (alias site)
 (target ppx.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/ppx.html})))

(rule
 (alias site)
 (target prologue.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/prologue.html})))

(rule
 (alias site)
 (target records.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/records.html})))

(rule
 (alias site)
 (target runtime-memory-layout.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/runtime-memory-layout.html})))

(rule
 (alias site)
 (target variables-and-functions.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/variables-and-functions.html})))

(rule
 (alias site)
 (target variants.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/variants.html})))
