New instrumented.

  $ bash ../test.sh << 'EOF'
  > class foo = object end
  > let _ = new foo
  > EOF
  class foo = object end
  
  let _ = ___bisect_post_visit___ 0 (new foo)


Not instrumented in tail position.

  $ bash ../test.sh << 'EOF'
  > class foo = object end
  > let _ = fun () -> new foo
  > EOF
  class foo = object end
  
  let _ =
   fun () ->
    ___bisect_visit___ 0;
    new foo


Not instrumented inside a surrounding application expression.

  $ bash ../test.sh << 'EOF'
  > class foo () = object end
  > let _ = new foo ()
  > EOF
  class foo () = object end
  
  let _ = ___bisect_post_visit___ 0 (new foo ())
