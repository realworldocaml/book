Pseudo-entry point of newtype is not instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = fun (type _t) -> ()
  > EOF
  let _ = fun (type _t) -> ()


Recursive instrumentation of subexpression.

  $ bash ../test.sh <<'EOF'
  > let _ = fun (type _t) -> fun x -> x
  > EOF
  let _ =
    fun (type _t) x ->
     ___bisect_visit___ 0;
     x


Subexpression in tail position iff whole expression is in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   fun (type _t) -> print_endline "foo"
  > let _ = fun () ->
  >   fun (type _t) -> print_endline "foo"
  > EOF
  let _ = fun (type _t) -> ___bisect_post_visit___ 0 (print_endline "foo")
  
  let _ =
   fun () ->
    ___bisect_visit___ 1;
    fun (type _t) -> print_endline "foo"
