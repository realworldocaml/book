Post-instrumented when they are not in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = print_endline "foo"
  > EOF
  let _ = ___bisect_post_visit___ 0 (print_endline "foo")


Not instrumented when in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> print_endline "foo"
  > EOF
  let _ =
   fun () ->
    ___bisect_visit___ 0;
    print_endline "foo"


Arguments instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > let _ = String.trim (String.trim "foo")
  > EOF
  let _ =
    ___bisect_post_visit___ 1
      (String.trim (___bisect_post_visit___ 0 (String.trim "foo")))


Function position instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > let _ = (List.map ignore) []
  > EOF
  let _ =
    ___bisect_post_visit___ 0 ((___bisect_post_visit___ 0 (List.map ignore)) [])


Multiple arguments don't produce nested instrumentation.

  $ bash ../test.sh <<'EOF'
  > let _ = List.map ignore []
  > EOF
  let _ = ___bisect_post_visit___ 0 (List.map ignore [])


Labels preserved.

  $ bash ../test.sh <<'EOF'
  > let _ = ListLabels.iter ~f:ignore []
  > EOF
  let _ = ___bisect_post_visit___ 0 (ListLabels.iter ~f:ignore [])


Instrumentation suppressed if all arguments labeled.

  $ bash ../test.sh <<'EOF'
  > [@@@ocaml.warning "-5"]
  > let _ = ListLabels.iter ~f:ignore
  > EOF
  [@@@ocaml.warning "-5"]
  
  let _ = ListLabels.iter ~f:ignore
