Out-edge instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = assert (bool_of_string "true")
  > EOF
  let _ =
    ___bisect_post_visit___ 1
      (assert (___bisect_post_visit___ 0 (bool_of_string "true")))


Not instrumented for assert false.

  $ bash ../test.sh <<'EOF'
  > let _ = assert false
  > EOF
  let _ = assert false
