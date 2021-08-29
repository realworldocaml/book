Pipe is given special treatment, to instrument it intuitively as an application
of a function to an argument, rather than a function to two arguments.

  $ bash ../test.sh <<'EOF'
  > let _ = "" |> String.trim
  > EOF
  let _ = ___bisect_post_visit___ 0 ("" |> String.trim)


Subexpressions instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > let _ = (String.trim "") |> (fun s -> String.trim s)
  > EOF
  let _ =
    ___bisect_post_visit___ 2
      ( ___bisect_post_visit___ 1 (String.trim "") |> fun s ->
        ___bisect_visit___ 0;
        String.trim s )


Instrumentation suppressed in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> "" |> String.trim
  > EOF
  let _ =
   fun () ->
    ___bisect_visit___ 0;
    "" |> String.trim


Right argument is not in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = [] |> List.mem 0
  > EOF
  let _ = ___bisect_post_visit___ 0 ([] |> List.mem 0)
