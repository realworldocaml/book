
(rule
 (target both-prelude.actual)
 (deps (package mdx) (source_tree both-prelude))
 (action
  (with-outputs-to %{target}
   (chdir both-prelude
    (system "! ocaml-mdx test --prelude=a --prelude-str=b test-case.md")))))

(rule
 (alias runtest)
 (action (diff both-prelude/test-case.md.expected both-prelude.actual)))

(rule
 (target in-toplevel.actual)
 (deps (package mdx) (source_tree in-toplevel))
 (action
  (with-outputs-to %{target}
   (chdir in-toplevel
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff in-toplevel/test-case.md.expected in-toplevel.actual)))

(rule
 (target invalid-label.actual)
 (deps (package mdx) (source_tree invalid-label))
 (action
  (with-outputs-to %{target}
   (chdir invalid-label
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff invalid-label/test-case.md.expected invalid-label.actual)))

(rule
 (target invalid-label-value.actual)
 (deps (package mdx) (source_tree invalid-label-value))
 (action
  (with-outputs-to %{target}
   (chdir invalid-label-value
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff invalid-label-value/test-case.md.expected invalid-label-value.actual)))

(rule
 (target mixed-label-syntax.actual)
 (deps (package mdx) (source_tree mixed-label-syntax))
 (action
  (with-outputs-to %{target}
   (chdir mixed-label-syntax
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff mixed-label-syntax/test-case.md.expected mixed-label-syntax.actual)))

(rule
 (target ml-file-not-found.actual)
 (deps (package mdx) (source_tree ml-file-not-found))
 (action
  (with-outputs-to %{target}
   (chdir ml-file-not-found
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff ml-file-not-found/test-case.md.expected ml-file-not-found.actual)))

(rule
 (target part-not-ended.actual)
 (deps (package mdx) (source_tree part-not-ended))
 (action
  (with-outputs-to %{target}
   (chdir part-not-ended
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff part-not-ended/test-case.md.expected part-not-ended.actual)))

(rule
 (target part-not-found.actual)
 (deps (package mdx) (source_tree part-not-found))
 (action
  (with-outputs-to %{target}
   (chdir part-not-found
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff part-not-found/test-case.md.expected part-not-found.actual)))

(rule
 (target part-not-opened.actual)
 (deps (package mdx) (source_tree part-not-opened))
 (action
  (with-outputs-to %{target}
   (chdir part-not-opened
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff part-not-opened/test-case.md.expected part-not-opened.actual)))

(rule
 (target part-unsupported.actual)
 (deps (package mdx) (source_tree part-unsupported))
 (action
  (with-outputs-to %{target}
   (chdir part-unsupported
    (system "! ocaml-mdx test test-case.md")))))

(rule
 (alias runtest)
 (action (diff part-unsupported/test-case.md.expected part-unsupported.actual)))
