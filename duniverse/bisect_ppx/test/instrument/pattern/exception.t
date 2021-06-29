Exception or-patterns.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | () -> ()
  >   | exception (Exit | Failure _) -> ()
  > EOF
  let _ =
    match () with
    | exception ((Exit | Failure _) as ___bisect_matched_value___) ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | Exit ->
            ___bisect_visit___ 1;
            ()
        | Failure _ ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ()
    | () ->
        ___bisect_visit___ 0;
        ()


Mixed value-exception cases are partitioned. Order is preserved. Case are
factored out into functions, whose parameters are the bound variables of the
patterns.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match Exit with
  >   | Exit as x | exception (Exit as x) -> ignore x; print_endline "foo"
  >   | End_of_file as y | exception (End_of_file | Failure _ as y) ->
  >     ignore y; print_endline "bar"
  >   | _ -> print_endline "baz"
  > EOF
  let _ =
    let ___bisect_case_0___ x () =
      ignore x;
      ___bisect_post_visit___ 0 (print_endline "foo")
    and ___bisect_case_1___ y () =
      ignore y;
      ___bisect_post_visit___ 1 (print_endline "bar")
    in
    match Exit with
    | exception (Exit as x) ->
        ___bisect_visit___ 4;
        ___bisect_case_0___ x ()
    | exception ((End_of_file | Failure _) as y as ___bisect_matched_value___) ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | End_of_file as y ->
            ___bisect_visit___ 6;
            ()
        | Failure _ as y ->
            ___bisect_visit___ 7;
            ()
        | _ -> ());
        ___bisect_case_1___ y ()
    | Exit as x ->
        ___bisect_visit___ 3;
        ___bisect_case_0___ x ()
    | End_of_file as y ->
        ___bisect_visit___ 5;
        ___bisect_case_1___ y ()
    | _ ->
        ___bisect_visit___ 8;
        ___bisect_post_visit___ 2 (print_endline "baz")
