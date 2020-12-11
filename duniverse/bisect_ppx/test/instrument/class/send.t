Send instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = (object method foo = () end)#foo
  > EOF
  let _ =
    ___bisect_post_visit___ 1
      (object
         method foo =
           ___bisect_visit___ 0;
           ()
      end)
        #foo


Not instrumented in tail position.

  $ bash ../test.sh <<'EOF'
  > let _ = fun () -> (object method foo = () end)#foo
  > EOF
  let _ =
   fun () ->
    ___bisect_visit___ 1;
    (object
       method foo =
         ___bisect_visit___ 0;
         ()
    end)
      #foo


Not instrumented inside a surrounding application expression.

  $ bash ../test.sh << 'EOF'
  > let _ = (object method foo () = () end)#foo ()
  > EOF
  let _ =
    ___bisect_post_visit___ 1
      ((object
          method foo () =
            ___bisect_visit___ 0;
            ()
       end)
         #foo
         ())
