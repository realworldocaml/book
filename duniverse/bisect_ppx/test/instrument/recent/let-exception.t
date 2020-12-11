  $ bash ../test.sh <<'EOF'
  > [@@@ocaml.warning "-38"]
  > let _ =
  >   let exception E in print_endline "foo"
  > let _ = fun () ->
  >   let exception E in print_endline "foo"
  > EOF
  [@@@ocaml.warning "-38"]
  
  let _ =
    let exception E in
    ___bisect_post_visit___ 0 (print_endline "foo")
  
  let _ =
   fun () ->
    ___bisect_visit___ 1;
    let exception E in
    print_endline "foo"
