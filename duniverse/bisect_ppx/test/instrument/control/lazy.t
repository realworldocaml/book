Thunk body is instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = lazy ()
  > EOF
  let _ =
    lazy
      (___bisect_visit___ 0;
       ())


Recursive instrumentation of subexpression.

  $ bash ../test.sh <<'EOF'
  > let _ = lazy (lazy ())
  > EOF
  let _ =
    lazy
      (___bisect_visit___ 1;
       lazy
         (___bisect_visit___ 0;
          ()))


Subexpression in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = lazy (print_endline "foo")
  > EOF
  let _ =
    lazy
      (___bisect_visit___ 0;
       print_endline "foo")
