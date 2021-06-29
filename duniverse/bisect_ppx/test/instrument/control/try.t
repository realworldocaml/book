Instrumentation of cases. No instrumentation of main subexpression.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   try ()
  >   with
  >   | Exit -> ()
  >   | Failure _ -> ()
  > EOF
  let _ =
    try () with
    | Exit ->
        ___bisect_visit___ 0;
        ()
    | Failure _ ->
        ___bisect_visit___ 1;
        ()


Recursive instrumentation of subexpressions.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   try
  >     try () with _ -> ()
  >   with _ ->
  >     try () with _ -> ()
  > EOF
  let _ =
    try
      try ()
      with _ ->
        ___bisect_visit___ 2;
        ()
    with _ -> (
      ___bisect_visit___ 1;
      try ()
      with _ ->
        ___bisect_visit___ 0;
        ())


Main subexpression is not in tail position. Handler is in tail position iff the
whole expression is in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   try print_endline "foo" with _ -> print_endline "bar"
  > let _ = fun () ->
  >   try print_endline "foo" with _ -> print_endline "bar"
  > EOF
  let _ =
    try ___bisect_post_visit___ 2 (print_endline "foo")
    with _ ->
      ___bisect_visit___ 1;
      ___bisect_post_visit___ 0 (print_endline "bar")
  
  let _ =
   fun () ->
    ___bisect_visit___ 5;
    try ___bisect_post_visit___ 4 (print_endline "foo")
    with _ ->
      ___bisect_visit___ 3;
      print_endline "bar"


Or-pattern.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   try ()
  >   with Exit | End_of_file -> ()
  > EOF
  let _ =
    try ()
    with (Exit | End_of_file) as ___bisect_matched_value___ ->
      (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
         ___bisect_matched_value___
       with
      | Exit ->
          ___bisect_visit___ 0;
          ()
      | End_of_file ->
          ___bisect_visit___ 1;
          ()
      | _ -> ());
      ()
