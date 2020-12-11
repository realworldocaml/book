GADT. See https://github.com/aantron/bisect_ppx/issues/325.

  $ bash ../test.sh <<'EOF'
  > type _ t = A : unit t | B : bool t
  > let f : type a. a t -> unit = fun x ->
  >   match x with
  >   | A | B -> ()
  > EOF
  type _ t = A : unit t | B : bool t
  
  let f : type a. a t -> unit =
   fun x ->
    ___bisect_visit___ 2;
    match x with
    | ___bisect_matched_value___ -> (
        match ___bisect_matched_value___ with
        | A | B ->
            (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
               ___bisect_matched_value___
             with
            | A ->
                ___bisect_visit___ 0;
                ()
            | B ->
                ___bisect_visit___ 1;
                ()
            | _ -> ());
            ())


With function.

  $ bash ../test.sh <<'EOF'
  > type _ t = A : unit t | B : bool t
  > let f : type a. a t -> unit = function
  >   | A | B -> ()
  > EOF
  type _ t = A : unit t | B : bool t
  
  let f : type a. a t -> unit =
   fun ___bisect_matched_value___ ->
    match ___bisect_matched_value___ with
    | A | B ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | A ->
            ___bisect_visit___ 0;
            ()
        | B ->
            ___bisect_visit___ 1;
            ()
        | _ -> ());
        ()
