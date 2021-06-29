Instrumentation of cases.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match true with
  >   | true -> ()
  >   | false -> ()
  > EOF
  let _ =
    match true with
    | true ->
        ___bisect_visit___ 0;
        ()
    | false ->
        ___bisect_visit___ 1;
        ()


Recursive instrumentation of cases.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match
  >     match () with
  >     | () -> ()
  >   with
  >   | () ->
  >     match () with
  >     | () -> ()
  > EOF
  let _ =
    match
      match () with
      | () ->
          ___bisect_visit___ 2;
          ()
    with
    | () -> (
        ___bisect_visit___ 1;
        match () with
        | () ->
            ___bisect_visit___ 0;
            ())


Expressions in selector don't need their out-edge instrumented. Expressions in
cases are in tail position iff the match expression is in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match print_endline "foo" with () -> print_endline "bar"
  > let _ = fun () ->
  >   match print_endline "foo" with () -> print_endline "bar"
  > EOF
  let _ =
    match print_endline "foo" with
    | () ->
        ___bisect_visit___ 1;
        ___bisect_post_visit___ 0 (print_endline "bar")
  
  let _ =
   fun () ->
    ___bisect_visit___ 3;
    match print_endline "foo" with
    | () ->
        ___bisect_visit___ 2;
        print_endline "bar"
