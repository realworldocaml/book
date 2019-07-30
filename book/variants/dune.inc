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
         (:y3 ../../examples/code/variants-termcol-annotated/terminal_color.ml)
         (:y2 ../../examples/code/variants-termcol-fixed/terminal_color.ml)
         (:y1 ../../examples/code/variants-termcol/terminal_color.ml)
         (:y0 ../../examples/code/variants-termcol/terminal_color.mli)
         (source_tree ../../examples/code/variants-termcol-annotated)
         catch_all.ml
         logger.ml
         prelude.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --prelude=catch_all:catch_all.ml --prelude=logger:logger.ml --direction=to-ml %{x}))
           (diff? %{y3} %{y3}.corrected)
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected)
           (diff? %{x} %{x}.corrected))))
