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
         (:y3 ../../examples/code/classes-async/multiple_inheritance.ml)
         (:y2 ../../examples/code/classes-async/multiple_inheritance_wrong.ml)
         (:y1 ../../examples/code/classes-async/shapes/shapes.ml)
         (:y0 ../../examples/code/classes-async/verbose_shapes.ml)
         (source_tree ../../examples/code/classes-async/shapes)
         prelude.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml %{x}))
           (diff? %{y3} %{y3}.corrected)
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected)
           (diff? %{x} %{x}.corrected))))
