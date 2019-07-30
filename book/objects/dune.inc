(rule
 (target findlib.conf)
 (action (write-file %{target} "path=\"%{project_root}/_build/install/%{context_name}/lib\"")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package ppx_jane)
         (:y2 ../../examples/code/objects/is_barbell.ml)
         (:y1 ../../examples/code/objects/narrowing.ml)
         (:y0 ../../examples/code/objects/subtyping.ml)
         prelude.ml
         subtyping.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml --direction=to-ml %{x}))
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected)
           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package ppx_jane)
         (:y2 ../../examples/code/objects/is_barbell.ml)
         (:y1 ../../examples/code/objects/narrowing.ml)
         (:y0 ../../examples/code/objects/subtyping.ml)
         prelude.ml
         subtyping.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml --direction=to-ml --non-deterministic %{x}))
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected)
           (diff? %{x} %{x}.corrected))))
