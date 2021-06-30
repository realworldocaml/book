If there is a pattern guard, pattern instrumentation is placed on it instead.
The nested expression gets a fresh instrumentation point, being the out-edge of
the guard, rather than the pattern.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A `B with
  >   | `A (`B | `C) when print_endline "foo"; true -> ()
  >   | _ -> ()
  > EOF
  let _ =
    match `A `B with
    | `A (`B | `C) as ___bisect_matched_value___
      when (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
              ___bisect_matched_value___
            with
           | `A `B ->
               ___bisect_visit___ 1;
               ()
           | `A `C ->
               ___bisect_visit___ 2;
               ()
           | _ -> ());
           ___bisect_post_visit___ 0 (print_endline "foo");
           true ->
        ___bisect_visit___ 3;
        ()
    | _ ->
        ___bisect_visit___ 4;
        ()

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | () -> ()
  >   | exception (Exit | Failure _) when print_endline "foo"; true -> ()
  > EOF
  let _ =
    match () with
    | exception ((Exit | Failure _) as ___bisect_matched_value___)
      when (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
              ___bisect_matched_value___
            with
           | Exit ->
               ___bisect_visit___ 2;
               ()
           | Failure _ ->
               ___bisect_visit___ 3;
               ()
           | _ -> ());
           ___bisect_post_visit___ 0 (print_endline "foo");
           true ->
        ___bisect_visit___ 4;
        ()
    | () ->
        ___bisect_visit___ 1;
        ()
