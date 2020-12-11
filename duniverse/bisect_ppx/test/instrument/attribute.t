Attributes can suppress instrumentation in an expression subtree.

  $ bash test.sh <<'EOF'
  > let _ =
  >   if true then
  >     ((fun () -> print_endline "foo") [@coverage off])
  >   else
  >     ignore
  > EOF
  let _ =
    if true then fun [@coverage off] () -> print_endline "foo"
    else (
      ___bisect_visit___ 0;
      ignore)


Suppression works even across a transition out of the expression language.

  $ bash test.sh <<'EOF'
  > let _ =
  >   (let module Foo = struct let _bar = fun () -> () end in
  >   ()) [@coverage off]
  > EOF
  let _ =
    (let module Foo = struct
       let _bar () = ()
     end in
    ())
    [@coverage off]


Attributes can suppress instrumentation of a structure item.

  $ bash test.sh <<'EOF'
  > let f () = ()
  >   [@@coverage off]
  > EOF
  let f () = () [@@coverage off]


Attributes can suppress instrumentation of a range of structure items.

  $ bash test.sh <<'EOF'
  > [@@@coverage off]
  > let f () = ()
  > [@@@coverage on]
  > let g () = ()
  > EOF
  [@@@coverage off]
  
  let f () = ()
  
  [@@@coverage on]
  
  let g () =
    ___bisect_visit___ 0;
    ()


Attributes can suppress coverage in a file.

  $ bash test.sh <<'EOF'
  > [@@@coverage exclude_file]
  > let f () = ()
  > EOF


Non-coverage attributes are preserved uninstrumented.

  $ bash test.sh <<'EOF'
  > [@@@foo print_endline "bar"]
  > 
  > let _ = ()
  >   [@@foo print_endline "bar"]
  > 
  > let _ = () [@foo print_endline "bar"]
  > EOF
  [@@@foo print_endline "bar"]
  
  let _ = () [@@foo print_endline "bar"]
  
  let _ = () [@foo print_endline "bar"]
