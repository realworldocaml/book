Subexpressions instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > let (let*) x f = f x
  > let (and*) x y = (x, y)
  > let return x = x
  > let _ =
  >   let* () = print_endline "foo"
  >   and* () = print_endline "bar" in
  >   let* () = print_endline "baz" in
  >   return ()
  > EOF
  let ( let* ) x f =
    ___bisect_visit___ 0;
    f x
  
  let ( and* ) x y =
    ___bisect_visit___ 1;
    (x, y)
  
  let return x =
    ___bisect_visit___ 2;
    x
  
  let _ =
    let* () = ___bisect_post_visit___ 7 (print_endline "foo")
    and* () = ___bisect_post_visit___ 6 (print_endline "bar") in
    ___bisect_visit___ 5;
    let* () = ___bisect_post_visit___ 4 (print_endline "baz") in
    ___bisect_visit___ 3;
    return ()
