Trivial.

  $ bash ../test.sh <<'EOF'
  > class foo =
  > object
  > end
  > EOF
  class foo = object end


Parameters are preserved.

  $ bash ../test.sh <<'EOF'
  > class foo_1 () =
  > object
  > end
  > class foo_2 ~l:_ =
  > object
  > end
  > class foo_3 ?l:_ () =
  > object
  > end
  > EOF
  class foo_1 () = object end
  
  class foo_2 ~l:_ = object end
  
  class foo_3 ?l:_ () = object end


Default values are instrumented, and instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > [@@@ocaml.warning "-27"]
  > class foo ?(l = fun () -> ()) () =
  > object
  > end
  > EOF
  [@@@ocaml.warning "-27"]
  
  class foo
    ?(l =
      ___bisect_visit___ 1;
      fun () ->
        ___bisect_visit___ 0;
        ()) () = object end


Nested expressions and initializers instrumented.

  $ bash ../test.sh <<'EOF'
  > class foo =
  >   let () = print_endline "bar" in
  >   object
  >     initializer print_endline "baz"
  >   end
  > EOF
  class foo =
    let () = ___bisect_post_visit___ 0 (print_endline "bar") in
    object
      initializer
      ___bisect_visit___ 2;
      ___bisect_post_visit___ 1 (print_endline "baz")
    end
