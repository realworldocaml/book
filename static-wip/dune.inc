
(rule
 (alias site-wip)
 (target classes.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/classes.html})))

(rule
 (alias site-wip)
 (target command-line-parsing.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/command-line-parsing.html})))

(rule
 (alias site-wip)
 (target compiler-backend.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/compiler-backend.html})))

(rule
 (alias site-wip)
 (target compiler-frontend.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/compiler-frontend.html})))

(rule
 (alias site-wip)
 (target concurrent-programming.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/concurrent-programming.html})))

(rule
 (alias site-wip)
 (target data-serialization.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/data-serialization.html})))

(rule
 (alias site-wip)
 (target error-handling.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/error-handling.html})))

(rule
 (alias site-wip)
 (target files-modules-and-programs.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/files-modules-and-programs.html})))

(rule
 (alias site-wip)
 (target first-class-modules.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/first-class-modules.html})))

(rule
 (alias site-wip)
 (target foreign-function-interface.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/foreign-function-interface.html})))

(rule
 (alias site-wip)
 (target functors.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/functors.html})))

(rule
 (alias site-wip)
 (target garbage-collector.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/garbage-collector.html})))

(rule
 (alias site-wip)
 (target guided-tour.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/guided-tour.html})))

(rule
 (alias site-wip)
 (target imperative-programming.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/imperative-programming.html})))

(rule
 (alias site-wip)
 (target json.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/json.html})))

(rule
 (alias site-wip)
 (target lists-and-patterns.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/lists-and-patterns.html})))

(rule
 (alias site-wip)
 (target maps-and-hashtables.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/maps-and-hashtables.html})))

(rule
 (alias site-wip)
 (target objects.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/objects.html})))

(rule
 (alias site-wip)
 (target parsing-with-ocamllex-and-menhir.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/parsing-with-ocamllex-and-menhir.html})))

(rule
 (alias site-wip)
 (target ppx.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/ppx.html})))

(rule
 (alias site-wip)
 (target prologue.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/prologue.html})))

(rule
 (alias site-wip)
 (target records.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/records.html})))

(rule
 (alias site-wip)
 (target runtime-memory-layout.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/runtime-memory-layout.html})))

(rule
 (alias site-wip)
 (target testing.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/testing.html})))

(rule
 (alias site-wip)
 (target variables-and-functions.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/variables-and-functions.html})))

(rule
 (alias site-wip)
 (target variants.html)
 (deps (alias ../book/html) ../book/toc.scm)
 (action (run rwo-build build chapter -o . -repo-root .. %{dep:../book/variants.html})))
