Alias.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A with
  >   | `A | `B as _x -> print_endline "foo"
  > EOF
  let _ =
    match `A with
    | (`A | `B) as _x as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A as _x ->
            ___bisect_visit___ 1;
            ()
        | `B as _x ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")


Constructor.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match Some `A with
  >   | Some (`A | `B) -> print_endline "foo"
  >   | None -> ()
  > EOF
  let _ =
    match Some `A with
    | Some (`A | `B) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | Some `A ->
            ___bisect_visit___ 1;
            ()
        | Some `B ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")
    | None ->
        ___bisect_visit___ 3;
        ()


Polymorphic variant constructor.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A `B with
  >   | `A (`B | `C) -> print_endline "foo"
  > EOF
  let _ =
    match `A `B with
    | `A (`B | `C) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A `B ->
            ___bisect_visit___ 1;
            ()
        | `A `C ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")


Type constraint.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A with
  >   | (`A | `B : _) -> print_endline "foo"
  > EOF
  let _ =
    match `A with
    | (`A | `B : _) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | (`A : _) ->
            ___bisect_visit___ 1;
            ()
        | (`B : _) ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")


Lazy.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match lazy `A with
  >   | lazy (`A | `B) -> print_endline "foo"
  > EOF
  let _ =
    match
      lazy
        (___bisect_visit___ 3;
         `A)
    with
    | (lazy (`A | `B)) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | (lazy `A) ->
            ___bisect_visit___ 1;
            ()
        | (lazy `B) ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")
