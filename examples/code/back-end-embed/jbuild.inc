(jbuild_version 1)

(alias ((name sexp) (deps (embed_me2.ml.sexp))))

(rule (
  (targets (embed_me2.ml.sexp))
  (deps    (embed_me2.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (embed_out.c.sexp))))

(rule (
  (targets (embed_out.c.sexp))
  (deps    (embed_out.c))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_embed_native.rawsh.sexp))))

(rule (
  (targets (build_embed_native.rawsh.sexp))
  (deps    (build_embed_native.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (link_custom.rawsh.sexp))))

(rule (
  (targets (link_custom.rawsh.sexp))
  (deps    (link_custom.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (main.c.sexp))))

(rule (
  (targets (main.c.sexp))
  (deps    (main.c))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_embed.sh.sexp))))

(rule (
  (targets (build_embed.sh.sexp))
  (deps    (build_embed.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_embed_c.sh.sexp))))

(rule (
  (targets (build_embed_c.sh.sexp))
  (deps    (build_embed_c.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_debug_hello.sh.sexp))))

(rule (
  (targets (run_debug_hello.sh.sexp))
  (deps    (run_debug_hello.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (hello.ml.sexp))))

(rule (
  (targets (hello.ml.sexp))
  (deps    (hello.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_embed_binary.rawsh.sexp))))

(rule (
  (targets (build_embed_binary.rawsh.sexp))
  (deps    (build_embed_binary.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (link_dllib.rawsh.sexp))))

(rule (
  (targets (link_dllib.rawsh.sexp))
  (deps    (link_dllib.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (embed_me1.ml.sexp))))

(rule (
  (targets (embed_me1.ml.sexp))
  (deps    (embed_me1.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

