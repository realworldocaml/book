Exception patterns under or-pattern.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | () -> ()
  >   | exception Exit | exception Failure _ -> ()
  > EOF
  let _ =
    match () with
    | (exception (Exit as ___bisect_matched_value___))
    | (exception (Failure _ as ___bisect_matched_value___)) ->
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


Exception pattern under type constraint.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | () -> ()
  >   | (exception (Exit | Failure _) : unit) -> ()
  > EOF
  let _ =
    match () with
    | ((exception ((Exit | Failure _) as ___bisect_matched_value___)) : unit) ->
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
