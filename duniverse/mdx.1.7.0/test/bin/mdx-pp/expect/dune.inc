
(rule
 (target section-option.actual)
 (deps (package mdx) (source_tree section-option))
 (action
  (with-stdout-to %{target}
   (chdir section-option
    (run ocaml-mdx pp --section Valuable test-case.md)))))

(rule
 (alias runtest)
 (action (diff section-option/test_case.ml.expected section-option.actual)))

(rule
 (target spaces.actual)
 (deps (package mdx) (source_tree spaces))
 (action
  (with-stdout-to %{target}
   (chdir spaces
    (run ocaml-mdx pp test-case.md)))))

(rule
 (alias runtest)
 (action (diff spaces/test_case.ml.expected spaces.actual)))

(rule
 (target toplevel.actual)
 (deps (package mdx) (source_tree toplevel))
 (action
  (with-stdout-to %{target}
   (chdir toplevel
    (run ocaml-mdx pp test-case.md)))))

(rule
 (alias runtest)
 (action (diff toplevel/test_case.ml.expected toplevel.actual)))
