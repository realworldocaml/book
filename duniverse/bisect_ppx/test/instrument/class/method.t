Method "entry point" instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     method foo = ()
  >   end
  > EOF
  let _ =
    object
      method foo =
        ___bisect_visit___ 0;
        ()
    end


Instrumentation is inserted into nested abstractions.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     method foo () () = ()
  >     method bar = function () -> ()
  >   end
  > EOF
  let _ =
    object
      method foo () () =
        ___bisect_visit___ 0;
        ()
  
      method bar =
        function
        | () ->
            ___bisect_visit___ 1;
            ()
    end


Subexpressions instrumented recursively.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     val foo = print_endline "foo"
  >     method bar = print_endline "bar"
  >   end
  > EOF
  let _ =
    object
      val foo = ___bisect_post_visit___ 0 (print_endline "foo")
  
      method bar =
        ___bisect_visit___ 1;
        print_endline "bar"
    end


Virtual method preserved.

  $ bash ../test.sh <<'EOF'
  > class virtual foo =
  > object
  >   method virtual bar : unit
  > end
  > EOF
  class virtual foo =
    object
      method virtual bar : unit
    end


Polymorphic type annotations preserved.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   object
  >     method foo : 'a. unit = ()
  >     method bar : 'a. 'a -> unit = fun _ -> ()
  >   end
  > EOF
  let _ =
    object
      method foo : 'a. unit =
        ___bisect_visit___ 0;
        ()
  
      method bar : 'a. 'a -> unit =
        fun _ ->
          ___bisect_visit___ 1;
          ()
    end
