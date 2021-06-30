Sys.opaque_identity instrumentation is suppressed.

  $ bash ../test.sh <<'EOF'
  > let _ = Sys.opaque_identity (print_endline "foo")
  > EOF
  let _ = Sys.opaque_identity (___bisect_post_visit___ 0 (print_endline "foo"))
