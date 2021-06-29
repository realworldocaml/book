Instrumentation of cases.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   function
  >   | 0 -> ()
  >   | _ -> ()
  > EOF
  let _ = function
    | 0 ->
        ___bisect_visit___ 0;
        ()
    | _ ->
        ___bisect_visit___ 1;
        ()


Recursive instrumentation of cases.

  $ bash ../test.sh <<'EOF'
  > let _ = function () -> function () -> ()
  > EOF
  let _ = function
    | () -> (
        ___bisect_visit___ 1;
        function
        | () ->
            ___bisect_visit___ 0;
            ())


Instrumentation suppressed "between arguments."

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> function () -> ()
  > EOF
  let _ =
   fun () -> function
    | () ->
        ___bisect_visit___ 0;
        ()


Expressions in cases are in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = function () -> print_endline "foo"
  > EOF
  let _ = function
    | () ->
        ___bisect_visit___ 0;
        print_endline "foo"


Or-pattern.

  $ bash ../test.sh <<'EOF'
  > let _ = function None | Some _ -> print_endline "foo"
  > EOF
  let _ =
   fun ___bisect_matched_value___ ->
    match ___bisect_matched_value___ with
    | None | Some _ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | None ->
            ___bisect_visit___ 0;
            ()
        | Some _ ->
            ___bisect_visit___ 1;
            ()
        | _ -> ());
        print_endline "foo"


Or-pattern with polymorphic variants.

  $ bash ../test.sh <<'EOF'
  > let _ = function `A | `B -> print_endline "foo"
  > EOF
  let _ = function
    | (`A | `B) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A ->
            ___bisect_visit___ 0;
            ()
        | `B ->
            ___bisect_visit___ 1;
            ()
        | _ -> ());
        print_endline "foo"
