Instrumentation is suppressed for all of the following function names.
Subexpressions are still instrumented.

  $ bash ../test.sh <<'EOF'
  > let _ = not (List.mem () [])
  > EOF
  let _ = not (___bisect_post_visit___ 0 (List.mem () []))


  $ bash ../test.sh <<'EOF'
  > let _ = (print_endline "foo") = (print_endline "bar")
  > let _ = (=) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") <> (print_endline "bar")
  > let _ = (<>) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") < (print_endline "bar")
  > let _ = (<) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") <= (print_endline "bar")
  > let _ = (<=) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") > (print_endline "bar")
  > let _ = (>) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") >= (print_endline "bar")
  > let _ = (>=) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") == (print_endline "bar")
  > let _ = (==) (print_endline "foo") (print_endline "bar")
  > let _ = (print_endline "foo") != (print_endline "bar")
  > let _ = (!=) (print_endline "foo") (print_endline "bar")
  > EOF
  let _ =
    ___bisect_post_visit___ 0 (print_endline "foo")
    = ___bisect_post_visit___ 1 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 2 (print_endline "foo")
    = ___bisect_post_visit___ 3 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 4 (print_endline "foo")
    <> ___bisect_post_visit___ 5 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 6 (print_endline "foo")
    <> ___bisect_post_visit___ 7 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 8 (print_endline "foo")
    < ___bisect_post_visit___ 9 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 10 (print_endline "foo")
    < ___bisect_post_visit___ 11 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 12 (print_endline "foo")
    <= ___bisect_post_visit___ 13 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 14 (print_endline "foo")
    <= ___bisect_post_visit___ 15 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 16 (print_endline "foo")
    > ___bisect_post_visit___ 17 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 18 (print_endline "foo")
    > ___bisect_post_visit___ 19 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 20 (print_endline "foo")
    >= ___bisect_post_visit___ 21 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 22 (print_endline "foo")
    >= ___bisect_post_visit___ 23 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 24 (print_endline "foo")
    == ___bisect_post_visit___ 25 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 26 (print_endline "foo")
    == ___bisect_post_visit___ 27 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 28 (print_endline "foo")
    != ___bisect_post_visit___ 29 (print_endline "bar")
  
  let _ =
    ___bisect_post_visit___ 30 (print_endline "foo")
    != ___bisect_post_visit___ 31 (print_endline "bar")


  $ bash ../test.sh <<'EOF'
  > let _ = ref (print_endline "foo")
  > let _ = !(ref (print_endline "foo"))
  > let _ = ref (print_endline "foo") := (print_endline "bar")
  > let _ = (:=) (ref (print_endline "foo")) (print_endline "bar")
  > EOF
  let _ = ref (___bisect_post_visit___ 0 (print_endline "foo"))
  
  let _ = !(ref (___bisect_post_visit___ 1 (print_endline "foo")))
  
  let _ =
    ref (___bisect_post_visit___ 2 (print_endline "foo"))
    := ___bisect_post_visit___ 3 (print_endline "bar")
  
  let _ =
    ref (___bisect_post_visit___ 4 (print_endline "foo"))
    := ___bisect_post_visit___ 5 (print_endline "bar")


  $ bash ../test.sh <<'EOF'
  > let _ = (List.rev []) @ (List.rev [])
  > let _ = (@) (List.rev []) (List.rev [])
  > let _ = (String.trim "") ^ (String.trim "")
  > let _ = (^) (String.trim "") (String.trim "")
  > EOF
  let _ =
    ___bisect_post_visit___ 0 (List.rev [])
    @ ___bisect_post_visit___ 1 (List.rev [])
  
  let _ =
    ___bisect_post_visit___ 2 (List.rev [])
    @ ___bisect_post_visit___ 3 (List.rev [])
  
  let _ =
    ___bisect_post_visit___ 4 (String.trim "")
    ^ ___bisect_post_visit___ 5 (String.trim "")
  
  let _ =
    ___bisect_post_visit___ 6 (String.trim "")
    ^ ___bisect_post_visit___ 7 (String.trim "")

  $ bash ../test.sh <<'EOF'
  > let _ = (List.length []) + (List.length [])
  > let _ = (+) (List.length []) (List.length [])
  > let _ = (List.length []) - (List.length [])
  > let _ = (-) (List.length []) (List.length [])
  > let _ = (List.length []) * (List.length [])
  > let _ = ( * ) (List.length []) (List.length [])
  > let _ = (List.length []) / (List.length [])
  > let _ = (/) (List.length []) (List.length [])
  > let _ = (List.length []) mod (List.length [])
  > let _ = (mod) (List.length []) (List.length [])
  > let _ = (float_of_int 0) +. (float_of_int 0)
  > let _ = (+.) (float_of_int 0) (float_of_int 0)
  > let _ = (float_of_int 0) +. (float_of_int 0)
  > let _ = (-.) (float_of_int 0) (float_of_int 0)
  > let _ = (float_of_int 0) *. (float_of_int 0)
  > let _ = ( *. ) (float_of_int 0) (float_of_int 0)
  > let _ = (float_of_int 0) /. (float_of_int 0)
  > let _ = (/.) (float_of_int 0) (float_of_int 0)
  > EOF
  let _ =
    ___bisect_post_visit___ 0 (List.length [])
    + ___bisect_post_visit___ 1 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 2 (List.length [])
    + ___bisect_post_visit___ 3 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 4 (List.length [])
    - ___bisect_post_visit___ 5 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 6 (List.length [])
    - ___bisect_post_visit___ 7 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 8 (List.length [])
    * ___bisect_post_visit___ 9 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 10 (List.length [])
    * ___bisect_post_visit___ 11 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 12 (List.length [])
    / ___bisect_post_visit___ 13 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 14 (List.length [])
    / ___bisect_post_visit___ 15 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 16 (List.length [])
    mod ___bisect_post_visit___ 17 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 18 (List.length [])
    mod ___bisect_post_visit___ 19 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 20 (float_of_int 0)
    +. ___bisect_post_visit___ 21 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 22 (float_of_int 0)
    +. ___bisect_post_visit___ 23 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 24 (float_of_int 0)
    +. ___bisect_post_visit___ 25 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 26 (float_of_int 0)
    -. ___bisect_post_visit___ 27 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 28 (float_of_int 0)
    *. ___bisect_post_visit___ 29 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 30 (float_of_int 0)
    *. ___bisect_post_visit___ 31 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 32 (float_of_int 0)
    /. ___bisect_post_visit___ 33 (float_of_int 0)
  
  let _ =
    ___bisect_post_visit___ 34 (float_of_int 0)
    /. ___bisect_post_visit___ 35 (float_of_int 0)


  $ bash ../test.sh <<'EOF'
  > let _ = (List.length []) land (List.length [])
  > let _ = (land) (List.length []) (List.length [])
  > let _ = (List.length []) lor (List.length [])
  > let _ = (lor) (List.length []) (List.length [])
  > let _ = (List.length []) lxor (List.length [])
  > let _ = (lxor) (List.length []) (List.length [])
  > let _ = (List.length []) lsl (List.length [])
  > let _ = (lsl) (List.length []) (List.length [])
  > let _ = (List.length []) lsr (List.length [])
  > let _ = (lsr) (List.length []) (List.length [])
  > let _ = (List.length []) asr (List.length [])
  > let _ = (asr) (List.length []) (List.length [])
  > EOF
  let _ =
    ___bisect_post_visit___ 0 (List.length [])
    land ___bisect_post_visit___ 1 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 2 (List.length [])
    land ___bisect_post_visit___ 3 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 4 (List.length [])
    lor ___bisect_post_visit___ 5 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 6 (List.length [])
    lor ___bisect_post_visit___ 7 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 8 (List.length [])
    lxor ___bisect_post_visit___ 9 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 10 (List.length [])
    lxor ___bisect_post_visit___ 11 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 12 (List.length [])
    lsl ___bisect_post_visit___ 13 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 14 (List.length [])
    lsl ___bisect_post_visit___ 15 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 16 (List.length [])
    lsr ___bisect_post_visit___ 17 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 18 (List.length [])
    lsr ___bisect_post_visit___ 19 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 20 (List.length [])
    asr ___bisect_post_visit___ 21 (List.length [])
  
  let _ =
    ___bisect_post_visit___ 22 (List.length [])
    asr ___bisect_post_visit___ 23 (List.length [])


  $ bash ../test.sh <<'EOF'
  > let _ = raise (print_endline "foo"; Exit)
  > let _ = raise_notrace (print_endline "foo"; Exit)
  > let _ = failwith (print_endline "foo"; "bar")
  > let _ = ignore (print_endline "foo")
  > let _ = Obj.magic (print_endline "foo")
  > EOF
  let _ =
    raise
      (___bisect_post_visit___ 0 (print_endline "foo");
       Exit)
  
  let _ =
    raise_notrace
      (___bisect_post_visit___ 1 (print_endline "foo");
       Exit)
  
  let _ =
    failwith
      (___bisect_post_visit___ 2 (print_endline "foo");
       "bar")
  
  let _ = ignore (___bisect_post_visit___ 3 (print_endline "foo"))
  
  let _ = Obj.magic (___bisect_post_visit___ 4 (print_endline "foo"))
