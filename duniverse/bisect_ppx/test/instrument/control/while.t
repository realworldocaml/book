Loop body is instrumented. Condition is not instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = while true do () done
  > EOF
  let _ =
    while true do
      ___bisect_visit___ 0;
      ()
    done


Recursive instrumentation of subexpressions.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   while
  >     (while true do () done); true
  >   do
  >     while true do () done
  >   done
  > EOF
  let _ =
    while
      while true do
        ___bisect_visit___ 2;
        ()
      done;
      true
    do
      ___bisect_visit___ 1;
      while true do
        ___bisect_visit___ 0;
        ()
      done
    done


Subexpressions not in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = while bool_of_string "true" do print_endline "foo" done
  > EOF
  let _ =
    while ___bisect_post_visit___ 2 (bool_of_string "true") do
      ___bisect_visit___ 1;
      ___bisect_post_visit___ 0 (print_endline "foo")
    done
