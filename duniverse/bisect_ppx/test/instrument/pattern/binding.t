Bindings made under or-patterns remain consistent after instrumentation.

  $ (bash ../test.sh | tail -n +4) <<'EOF'
  > let _ =
  >   match `A with
  >   | (`A as x) | (`B as x) -> print_endline "foo"; x
  > EOF
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A as x ->
            ___bisect_visit___ 1;
            ()
        | `B as x ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo");
        x

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A () with
  >   | `A x | `B x -> print_endline "foo"; x
  > EOF
  let _ =
    match `A () with
    | (`A x | `B x) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A x ->
            ___bisect_visit___ 1;
            ()
        | `B x ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo");
        x
