Tuple.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match (`A, `C) with
  >   | ((`A | `B), (`C | `D)) -> print_endline "foo"
  > EOF
  let _ =
    match (`A, `C) with
    | ((`A | `B), (`C | `D)) as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | `A, `C ->
            ___bisect_visit___ 2;
            ___bisect_visit___ 1;
            ()
        | `A, `D ->
            ___bisect_visit___ 3;
            ___bisect_visit___ 1;
            ()
        | `B, `C ->
            ___bisect_visit___ 2;
            ___bisect_visit___ 4;
            ()
        | `B, `D ->
            ___bisect_visit___ 3;
            ___bisect_visit___ 4;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")


Record.

  $ bash ../test.sh <<'EOF'
  > type t = {a : bool; b : bool}
  > let _ =
  >   match {a = true; b = false} with
  >   | {a = true | false; b = true | false} -> print_endline "foo"
  > EOF
  type t = { a : bool; b : bool }
  
  let _ =
    match { a = true; b = false } with
    | ___bisect_matched_value___ -> (
        match ___bisect_matched_value___ with
        | { a = true | false; b = true | false } ->
            (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
               ___bisect_matched_value___
             with
            | { a = true; b = true } ->
                ___bisect_visit___ 2;
                ___bisect_visit___ 1;
                ()
            | { a = true; b = false } ->
                ___bisect_visit___ 3;
                ___bisect_visit___ 1;
                ()
            | { a = false; b = true } ->
                ___bisect_visit___ 2;
                ___bisect_visit___ 4;
                ()
            | { a = false; b = false } ->
                ___bisect_visit___ 3;
                ___bisect_visit___ 4;
                ()
            | _ -> ());
            ___bisect_post_visit___ 0 (print_endline "foo"))


Array.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match [|`A; `C|] with
  >   | [|`A | `B; `C | `D|] -> print_endline "foo"
  >   | _ -> ()
  > EOF
  let _ =
    match [| `A; `C |] with
    | [| `A | `B; `C | `D |] as ___bisect_matched_value___ ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | [| `A; `C |] ->
            ___bisect_visit___ 2;
            ___bisect_visit___ 1;
            ()
        | [| `A; `D |] ->
            ___bisect_visit___ 3;
            ___bisect_visit___ 1;
            ()
        | [| `B; `C |] ->
            ___bisect_visit___ 2;
            ___bisect_visit___ 4;
            ()
        | [| `B; `D |] ->
            ___bisect_visit___ 3;
            ___bisect_visit___ 4;
            ()
        | _ -> ());
        ___bisect_post_visit___ 0 (print_endline "foo")
    | _ ->
        ___bisect_visit___ 5;
        ()
