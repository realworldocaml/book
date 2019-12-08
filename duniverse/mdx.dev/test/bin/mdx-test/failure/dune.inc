
(rule
 (target both-prelude.actual)
 (deps (package mdx) (source_tree both-prelude))
 (action
  (with-outputs-to %{target}
   (chdir both-prelude
    (system "! ocaml-mdx test --prelude=a --prelude-str=b test-case.md")))))

(alias
 (name runtest)
 (action (diff both-prelude/test-case.md.expected both-prelude.actual)))

(rule
 (target in-toplevel.actual)
 (deps (package mdx) (source_tree in-toplevel))
 (action
  (with-outputs-to %{target}
   (chdir in-toplevel
    (system "! ocaml-mdx test test-case.md")))))

(alias
 (name runtest)
 (action (diff in-toplevel/test-case.md.expected in-toplevel.actual)))

(rule
 (target ml-file-not-found.actual)
 (deps (package mdx) (source_tree ml-file-not-found))
 (action
  (with-outputs-to %{target}
   (chdir ml-file-not-found
    (system "! ocaml-mdx test test-case.md")))))

(alias
 (name runtest)
 (action (diff ml-file-not-found/test-case.md.expected ml-file-not-found.actual)))

(rule
 (target part-not-found.actual)
 (deps (package mdx) (source_tree part-not-found))
 (action
  (with-outputs-to %{target}
   (chdir part-not-found
    (system "! ocaml-mdx test --direction=to-md test-case.md")))))

(alias
 (name runtest)
 (action (diff part-not-found/test-case.md.expected part-not-found.actual)))
