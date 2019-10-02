(alias
 (name runtest)
 (deps
  (:x pp.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x ellipsis.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x envs.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x empty_line.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x spaces.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x errors.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x errors.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md --non-deterministic %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x heredoc.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x mlt.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x semisemi.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x exit.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x padding.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x multilines.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x lines.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x lwt.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x non-det.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x non-det.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md --non-deterministic %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x code.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x set_environment_variable.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x unset_environment_variable.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x prelude.md))
 (action
  (progn
   (run ocaml-mdx test --prelude-str "#require \"lwt\"" --prelude-str
     "toto:let x = \"42\"" --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x prelude_file.md)
  prelude.ml)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x dune_rules.md)
  (:y1 dune_rules_1.ml)
  (:y0 dune_rules_2.ml)
  (source_tree foo))
 (action
  (progn
   (run ocaml-mdx test --direction=to-ml %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
(alias
 (name runtest)
 (deps
  (:x require/require-package.md)
  (package example_lib))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x cram.t))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md --syntax=cram %{x})
   (diff? %{x} %{x}.corrected))))
(alias
 (name runtest)
 (deps
  (:x section.md))
 (action
  (progn
   (run ocaml-mdx test --direction=to-md --section "Testing" %{x})
   (diff? %{x} %{x}.corrected))))
