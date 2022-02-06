Let-bindings with [@@react.component] are not instrumented at their top level.

  $ bash ../test.sh <<'EOF'
  > module React = struct let forwardRef f = f () end
  > 
  > let make1 = fun () -> ignore ignore
  >   [@@react.component]
  > 
  > let make2 () = ignore ignore
  >   [@@react.component]
  > 
  > let make3 = React.forwardRef (fun r -> ignore r)
  >   [@@react.component]
  > EOF
  module React = struct
    let forwardRef f =
      ___bisect_visit___ 0;
      f ()
  end
  
  let make1 () =
    ___bisect_visit___ 1;
    ignore ignore
    [@@react.component]
  
  let make2 () =
    ___bisect_visit___ 2;
    ignore ignore
    [@@react.component]
  
  let make3 =
    React.forwardRef (fun r ->
        ___bisect_visit___ 3;
        ignore r)
    [@@react.component]
