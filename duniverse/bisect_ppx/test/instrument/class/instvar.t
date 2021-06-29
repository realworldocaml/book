Pexp_setinstvar traversed.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     val mutable x = ()
  >     method foo = x <- (print_endline "foo")
  >   end
  > EOF
  let _ =
    object
      val mutable x = ()
  
      method foo =
        ___bisect_visit___ 1;
        x <- ___bisect_post_visit___ 0 (print_endline "foo")
    end


Pexp_override traversed.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     val x = ()
  >     method foo = {< x = print_endline "foo" >}
  >   end
  let _ =
    object
      val x = ()
  
      method foo =
        ___bisect_visit___ 1;
        {<x = ___bisect_post_visit___ 0 (print_endline "foo")>}
    end
