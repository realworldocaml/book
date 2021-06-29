No instrumentation is inserted into expressions that are (syntactic) values.


  $ bash test.sh <<'EOF'
  > let _ = ignore
  > EOF
  let _ = ignore


  $ bash test.sh <<'EOF'
  > let _ = 0
  > EOF
  let _ = 0


  $ bash test.sh <<'EOF'
  > let _ = let x = 0 in x
  > let _ = let _x = print_endline "foo" in print_endline "bar"
  > let _ = fun () -> let _x = print_endline "foo" in print_endline "bar"
  > EOF
  let _ =
    let x = 0 in
    x
  
  let _ =
    let _x = ___bisect_post_visit___ 1 (print_endline "foo") in
    ___bisect_post_visit___ 0 (print_endline "bar")
  
  let _ =
   fun () ->
    ___bisect_visit___ 3;
    let _x = ___bisect_post_visit___ 2 (print_endline "foo") in
    print_endline "bar"


  $ bash test.sh <<'EOF'
  > let _ = let x = 0 and _y = 1 in x
  > EOF
  let _ =
    let x = 0 and _y = 1 in
    x


  $ bash test.sh <<'EOF'
  > let _ = (let rec x = 0 and _y = 1 in x) [@ocaml.warning "-39"]
  > EOF
  let _ =
    (let rec x = 0 and _y = 1 in
     x)
    [@ocaml.warning "-39"]


  $ bash test.sh <<'EOF'
  > let _ = (0, 1)
  > let _ = (print_endline "foo", print_endline "bar")
  > EOF
  let _ = (0, 1)
  
  let _ =
    ( ___bisect_post_visit___ 0 (print_endline "foo"),
      ___bisect_post_visit___ 1 (print_endline "bar") )


  $ bash test.sh <<'EOF'
  > let _ = Exit
  > EOF
  let _ = Exit


  $ bash test.sh <<'EOF'
  > let _ = Failure "foo"
  > let _ = Failure (String.concat "" [])
  > EOF
  let _ = Failure "foo"
  
  let _ = Failure (___bisect_post_visit___ 0 (String.concat "" []))


  $ bash test.sh <<'EOF'
  > let _ = `Foo
  > EOF
  let _ = `Foo


  $ bash test.sh <<'EOF'
  > let _ = `Foo "bar"
  > let _ = `Foo (print_endline "foo")
  > EOF
  let _ = `Foo "bar"
  
  let _ = `Foo (___bisect_post_visit___ 0 (print_endline "foo"))


  $ bash test.sh <<'EOF'
  > let _ = {contents = 0}
  > let _ = {contents = print_endline "foo"}
  > EOF
  let _ = { contents = 0 }
  
  let _ = { contents = ___bisect_post_visit___ 0 (print_endline "foo") }


  $ bash test.sh <<'EOF'
  > [@@@ocaml.warning "-23"]
  > let _ = {{contents = 0} with contents = 1}
  > let _ = {{contents = ()} with contents = print_endline "foo"}
  > EOF
  [@@@ocaml.warning "-23"]
  
  let _ = { { contents = 0 } with contents = 1 }
  
  let _ =
    {
      { contents = () } with
      contents = ___bisect_post_visit___ 0 (print_endline "foo");
    }


  $ bash test.sh <<'EOF'
  > let _ = {contents = 0}.contents
  > EOF
  let _ = { contents = 0 }.contents


  $ bash test.sh <<'EOF'
  > let _ = {contents = 0}.contents <- 1
  > let _ = {contents = ()}.contents <- print_endline "foo"
  > EOF
  let _ = { contents = 0 }.contents <- 1
  
  let _ =
    { contents = () }.contents <- ___bisect_post_visit___ 0 (print_endline "foo")


  $ bash test.sh <<'EOF'
  > let _ = [|0; 1|]
  > let _ = [|print_endline "foo"; print_endline "bar"|]
  > EOF
  let _ = [| 0; 1 |]
  
  let _ =
    [|
      ___bisect_post_visit___ 0 (print_endline "foo");
      ___bisect_post_visit___ 1 (print_endline "bar");
    |]


  $ bash test.sh <<'EOF'
  > let _ = (); 0
  > let _ = print_endline "foo"; print_endline "bar"
  > let _ = fun () -> print_endline "foo"; print_endline "bar"
  > EOF
  let _ =
    ();
    0
  
  let _ =
    ___bisect_post_visit___ 1 (print_endline "foo");
    ___bisect_post_visit___ 0 (print_endline "bar")
  
  let _ =
   fun () ->
    ___bisect_visit___ 3;
    ___bisect_post_visit___ 2 (print_endline "foo");
    print_endline "bar"


  $ bash test.sh <<'EOF'
  > let _ = (0 : int)
  > let _ = (print_endline "foo" : unit)
  > let _ = fun () -> (print_endline "foo" : unit)
  > EOF
  let _ = (0 : int)
  
  let _ = (___bisect_post_visit___ 0 (print_endline "foo") : unit)
  
  let _ =
   fun () : unit ->
    ___bisect_visit___ 1;
    print_endline "foo"


  $ bash test.sh <<'EOF'
  > let _ = (`Foo :> [ `Foo | `Bar ])
  > let f () = `Foo
  > let _ = (f () :> [ `Foo | `Bar ])
  > let _ = fun () -> (f () :> [ `Foo | `Bar ])
  > EOF
  let _ = (`Foo :> [ `Foo | `Bar ])
  
  let f () =
    ___bisect_visit___ 0;
    `Foo
  
  let _ = (___bisect_post_visit___ 1 (f ()) :> [ `Foo | `Bar ])
  
  let _ = fun () -> (f () :> [ `Foo | `Bar ])


  $ bash test.sh <<'EOF'
  > let _ = let module Foo = struct end in 0
  > let _ =
  >   let module Foo = struct let () = print_endline "foo" end in
  >   print_endline "bar"
  > let _ = fun () ->
  >   let module Foo = struct let () = print_endline "foo" end in
  >   print_endline "bar"
  > EOF
  let _ =
    let module Foo = struct end in
    0
  
  let _ =
    let module Foo = struct
      let () = ___bisect_post_visit___ 1 (print_endline "foo")
    end in
    ___bisect_post_visit___ 0 (print_endline "bar")
  
  let _ =
   fun () ->
    ___bisect_visit___ 3;
    let module Foo = struct
      let () = ___bisect_post_visit___ 2 (print_endline "foo")
    end in
    print_endline "bar"


  $ bash test.sh <<'EOF'
  > module type X = sig val x : unit end
  > let _ = (module struct let x = () end : X)
  > let _ = (module struct let x = print_endline "foo" end : X)
  > EOF
  module type X = sig
    val x : unit
  end
  
  let _ =
    (module struct
      let x = ()
    end : X)
  
  let _ =
    (module struct
      let x = ___bisect_post_visit___ 0 (print_endline "foo")
    end : X)


  $ bash test.sh <<'EOF'
  > [@@@ocaml.warning "-33"]
  > let _ = let open List in ignore
  > let _ = let open List in print_endline "foo"
  > let _ = fun () -> let open List in print_endline "foo"
  > EOF
  [@@@ocaml.warning "-33"]
  
  let _ =
    let open List in
    ignore
  
  let _ =
    let open List in
    ___bisect_post_visit___ 0 (print_endline "foo")
  
  let _ =
   fun () ->
    ___bisect_visit___ 1;
    let open List in
    print_endline "foo"
