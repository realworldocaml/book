
(rule
 (target bash-fence.actual)
 (deps (package mdx) (source_tree bash-fence))
 (action
  (with-stdout-to %{target}
   (chdir bash-fence
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff bash-fence/test-case.md.expected bash-fence.actual)))

(rule
 (target casual-file-inc.actual)
 (deps (package mdx) (source_tree casual-file-inc))
 (action
  (with-stdout-to %{target}
   (chdir casual-file-inc
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff casual-file-inc/test-case.md.expected casual-file-inc.actual)))

(rule
 (target code.actual)
 (deps (package mdx) (source_tree code))
 (action
  (with-stdout-to %{target}
   (chdir code
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff code/test-case.md code.actual)))

(rule
 (target cram.actual)
 (deps (package mdx) (source_tree cram))
 (action
  (with-stdout-to %{target}
   (chdir cram
    (run ocaml-mdx test --output - --syntax=cram test-case.t)))))

(rule
 (alias runtest)
 (action (diff cram/test-case.t cram.actual)))

(rule
 (target ellipsis.actual)
 (deps (package mdx) (source_tree ellipsis))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff ellipsis/test-case.md ellipsis.actual)))

(rule
 (target ellipsis-updates.actual)
 (deps (package mdx) (source_tree ellipsis-updates))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis-updates
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff ellipsis-updates/test-case.md.expected ellipsis-updates.actual)))

(rule
 (target empty.actual)
 (deps (package mdx) (source_tree empty))
 (action
  (with-stdout-to %{target}
   (chdir empty
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff empty/test-case.md empty.actual)))

(rule
 (target empty-line.actual)
 (deps (package mdx) (source_tree empty-line))
 (action
  (with-stdout-to %{target}
   (chdir empty-line
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff empty-line/test-case.md empty-line.actual)))

(rule
 (target empty-lines.actual)
 (deps (package mdx) (source_tree empty-lines))
 (action
  (with-stdout-to %{target}
   (chdir empty-lines
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff empty-lines/test-case.md.expected empty-lines.actual)))

(rule
 (target env-and-prelude.actual)
 (deps (package mdx) (source_tree env-and-prelude))
 (action
  (with-stdout-to %{target}
   (chdir env-and-prelude
    (run ocaml-mdx test --output - --prelude all.ml --prelude " :default.ml" --prelude a:a.ml --prelude b:b.ml test-case.md)))))

(rule
 (alias runtest)
 (action (diff env-and-prelude/test-case.md env-and-prelude.actual)))

(rule
 (target environment-variable.actual)
 (deps (package mdx) (source_tree environment-variable))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff environment-variable/test-case.md environment-variable.actual)))

(rule
 (target environment-variable-set.actual)
 (deps (package mdx) (source_tree environment-variable-set))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable-set
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff environment-variable-set/test-case.md environment-variable-set.actual)))

(rule
 (target environment-variable-unset.actual)
 (deps (package mdx) (source_tree environment-variable-unset))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable-unset
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff environment-variable-unset/test-case.md environment-variable-unset.actual)))

(rule
 (target envs.actual)
 (deps (package mdx) (source_tree envs))
 (action
  (with-stdout-to %{target}
   (chdir envs
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff envs/test-case.md envs.actual)))

(rule
 (target errors.actual)
 (deps (package mdx) (source_tree errors))
 (action
  (with-stdout-to %{target}
   (chdir errors
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff errors/test-case.md errors.actual)))

(rule
 (target exit.actual)
 (deps (package mdx) (source_tree exit))
 (action
  (with-stdout-to %{target}
   (chdir exit
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff exit/test-case.md exit.actual)))

(rule
 (target heredoc.actual)
 (deps (package mdx) (source_tree heredoc))
 (action
  (with-stdout-to %{target}
   (chdir heredoc
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff heredoc/test-case.md heredoc.actual)))

(rule
 (target labels-syntax.actual)
 (deps (package mdx) (source_tree labels-syntax))
 (action
  (with-stdout-to %{target}
   (chdir labels-syntax
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff labels-syntax/test-case.md labels-syntax.actual)))

(rule
 (target lines.actual)
 (deps (package mdx) (source_tree lines))
 (action
  (with-stdout-to %{target}
   (chdir lines
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff lines/test-case.md lines.actual)))

(rule
 (target lwt.actual)
 (deps (package mdx) (source_tree lwt))
 (action
  (with-stdout-to %{target}
   (chdir lwt
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff lwt/test-case.md lwt.actual)))

(rule
 (target mlt.actual)
 (deps (package mdx) (source_tree mlt))
 (action
  (with-stdout-to %{target}
   (chdir mlt
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff mlt/test-case.md mlt.actual)))

(rule
 (target multilines.actual)
 (deps (package mdx) (source_tree multilines))
 (action
  (with-stdout-to %{target}
   (chdir multilines
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff multilines/test-case.md multilines.actual)))

(rule
 (target non-det.actual)
 (deps (package mdx) (source_tree non-det))
 (action
  (with-stdout-to %{target}
   (chdir non-det
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff non-det/test-case.md non-det.actual)))

(rule
 (target non-det-default-preserved.actual)
 (deps (package mdx) (source_tree non-det-default-preserved))
 (action
  (with-stdout-to %{target}
   (chdir non-det-default-preserved
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff non-det-default-preserved/test-case.md non-det-default-preserved.actual)))

(rule
 (target ocaml-408-syntax.actual)
 (deps (package mdx) (source_tree ocaml-408-syntax))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-408-syntax
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff ocaml-408-syntax/test-case.md ocaml-408-syntax.actual)))

(rule
 (target ocaml-errors.actual)
 (deps (package mdx) (source_tree ocaml-errors))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-errors
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff ocaml-errors/test-case.md.expected ocaml-errors.actual)))

(rule
 (target ocaml-errors-ellipsis.actual)
 (deps (package mdx) (source_tree ocaml-errors-ellipsis))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-errors-ellipsis
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff ocaml-errors-ellipsis/test-case.md ocaml-errors-ellipsis.actual)))

(rule
 (target padding.actual)
 (deps (package mdx) (source_tree padding))
 (action
  (with-stdout-to %{target}
   (chdir padding
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff padding/test-case.md padding.actual)))

(rule
 (target parts-begin-end.actual)
 (deps (package mdx) (source_tree parts-begin-end))
 (action
  (with-stdout-to %{target}
   (chdir parts-begin-end
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff parts-begin-end/test-case.md.expected parts-begin-end.actual)))

(rule
 (target prelude.actual)
 (deps (package mdx) (source_tree prelude))
 (action
  (with-stdout-to %{target}
   (chdir prelude
    (run ocaml-mdx test --output - --prelude-str "#require \"lwt\"" --prelude-str "toto:let x = \"42\"" test-case.md)))))

(rule
 (alias runtest)
 (action (diff prelude/test-case.md prelude.actual)))

(rule
 (target prelude-file.actual)
 (deps (package mdx) (source_tree prelude-file))
 (action
  (with-stdout-to %{target}
   (chdir prelude-file
    (run ocaml-mdx test --output - --prelude prelude.ml test-case.md)))))

(rule
 (alias runtest)
 (action (diff prelude-file/test-case.md prelude-file.actual)))

(rule
 (target root-option.actual)
 (deps (package mdx) (source_tree root-option))
 (action
  (with-stdout-to %{target}
   (chdir root-option
    (run ocaml-mdx test --output - --root=somedir test-case.md)))))

(rule
 (alias runtest)
 (action (diff root-option/test-case.md.expected root-option.actual)))

(rule
 (target section.actual)
 (deps (package mdx) (source_tree section))
 (action
  (with-stdout-to %{target}
   (chdir section
    (run ocaml-mdx test --output - -s Testing test-case.md)))))

(rule
 (alias runtest)
 (action (diff section/test-case.md.expected section.actual)))

(rule
 (target semisemi.actual)
 (deps (package mdx) (source_tree semisemi))
 (action
  (with-stdout-to %{target}
   (chdir semisemi
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff semisemi/test-case.md semisemi.actual)))

(rule
 (target shell-file-inc.actual)
 (deps (package mdx) (source_tree shell-file-inc))
 (action
  (with-stdout-to %{target}
   (chdir shell-file-inc
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff shell-file-inc/test-case.md.expected shell-file-inc.actual)))

(rule
 (target simple-mli.actual)
 (deps (package mdx) (source_tree simple-mli))
 (action
  (with-stdout-to %{target}
   (chdir simple-mli
    (run ocaml-mdx test --output - --syntax=mli test-case.mli)))))

(rule
 (alias runtest)
 (action (diff simple-mli/test-case.mli simple-mli.actual)))

(rule
 (target spaces.actual)
 (deps (package mdx) (source_tree spaces))
 (action
  (with-stdout-to %{target}
   (chdir spaces
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff spaces/test-case.md spaces.actual)))

(rule
 (target sync-from-subdir.actual)
 (deps (package mdx) (source_tree sync-from-subdir))
 (action
  (with-stdout-to %{target}
   (chdir sync-from-subdir
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff sync-from-subdir/test-case.md.expected sync-from-subdir.actual)))

(rule
 (target sync-to-md.actual)
 (deps (package mdx) (source_tree sync-to-md))
 (action
  (with-stdout-to %{target}
   (chdir sync-to-md
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff sync-to-md/test-case.md.expected sync-to-md.actual)))

(rule
 (target tabs.actual)
 (deps (package mdx) (source_tree tabs))
 (action
  (with-stdout-to %{target}
   (chdir tabs
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff tabs/test-case.md.expected tabs.actual)))

(rule
 (target toploop-getvalue.actual)
 (deps (package mdx) (source_tree toploop-getvalue))
 (action
  (with-stdout-to %{target}
   (chdir toploop-getvalue
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff toploop-getvalue/test-case.md toploop-getvalue.actual)))

(rule
 (target trailing-whitespaces.actual)
 (deps (package mdx) (source_tree trailing-whitespaces))
 (action
  (with-stdout-to %{target}
   (chdir trailing-whitespaces
    (run ocaml-mdx test --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff trailing-whitespaces/test-case.md.expected trailing-whitespaces.actual)))
