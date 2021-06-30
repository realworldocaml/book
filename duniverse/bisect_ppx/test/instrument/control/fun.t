Instrumentation of internal entry point.

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> ()
  > EOF
  let _ =
   fun () ->
    ___bisect_visit___ 0;
    ()


Preservation of labeled arguments and their patterns.

  $ bash ../test.sh <<'EOF'
  > let _ = fun ~l:_ -> ()
  > EOF
  let _ =
   fun ~l:_ ->
    ___bisect_visit___ 0;
    ()


Preservation of optional labeled arguments.

  $ bash ../test.sh <<'EOF'
  > let _ = (fun ?l:_ -> ()) [@ocaml.warning "-16"]
  > EOF
  let _ =
   fun [@ocaml.warning "-16"] ?l:_ ->
    ___bisect_visit___ 0;
    ()


Preservation of default values. Instrumentation of entry into default values.
Recursive instrumentation of default values.

  $ bash ../test.sh <<'EOF'
  > let _ = fun ?(l = fun () -> ()) -> l
  > EOF
  let _ =
   fun ?(l =
         ___bisect_visit___ 1;
         fun () ->
           ___bisect_visit___ 0;
           ()) ->
    ___bisect_visit___ 2;
    l


Recursive instrumentation of main subexpression. Instrumentation suppressed on
"between arguments."

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> fun () -> ()
  > EOF
  let _ =
   fun () () ->
    ___bisect_visit___ 0;
    ()


Instrumentation placed correctly if immediate child is a "return type"
constraint.

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> (() : unit)
  > EOF
  let _ =
   fun () : unit ->
    ___bisect_visit___ 0;
    ()


Gentle handling of optional argument elimination. See
https://github.com/aantron/bisect_ppx/issues/319.

  $ bash ../test.sh <<'EOF'
  > let f () ?x () =
  >   x
  > 
  > let () =
  >   ignore (List.map (f ()) [])
  > EOF
  let f () ?x () =
    ___bisect_visit___ 0;
    x
  
  let () =
    ignore
      (___bisect_post_visit___ 2 (List.map (___bisect_post_visit___ 1 (f ())) []))


Expressions in default value are not in tail position; expressions in main
subexpression are.

  $ bash ../test.sh <<'EOF'
  > [@@@ocaml.warning "-27"]
  > let _ =
  >   fun ?(l = print_endline "foo") () -> print_endline "bar"
  > EOF
  [@@@ocaml.warning "-27"]
  
  let _ =
   fun ?(l =
         ___bisect_visit___ 1;
         ___bisect_post_visit___ 0 (print_endline "foo")) () ->
    ___bisect_visit___ 2;
    print_endline "bar"
