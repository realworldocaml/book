Wildcard.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | _ -> ()
  > EOF
  let _ =
    match () with
    | _ ->
        ___bisect_visit___ 0;
        ()


Variable.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | x -> x
  > EOF
  let _ =
    match () with
    | x ->
        ___bisect_visit___ 0;
        x


Nullary constructor.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match () with
  >   | () -> ()
  > EOF
  let _ =
    match () with
    | () ->
        ___bisect_visit___ 0;
        ()


Constant.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match 0 with
  >   | 0 -> ()
  >   | _ -> ()
  > EOF
  let _ =
    match 0 with
    | 0 ->
        ___bisect_visit___ 0;
        ()
    | _ ->
        ___bisect_visit___ 1;
        ()


Interval.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match 'a' with
  >   | 'a'..'z' -> ()
  >   | _ -> ()
  > EOF
  let _ =
    match 'a' with
    | 'a' .. 'z' ->
        ___bisect_visit___ 0;
        ()
    | _ ->
        ___bisect_visit___ 1;
        ()


Nullary polymorphic variand.

  $ bash ../test.sh <<'EOF'
  > let _ =
  >   match `A with
  >   | `A -> ()
  > EOF
  let _ =
    match `A with
    | `A ->
        ___bisect_visit___ 0;
        ()


Polymorphic variant type name.

  $ bash ../test.sh <<'EOF'
  > type t = [ `A ]
  > let _ =
  >   match `A with
  >   | #t -> ()
  > EOF
  type t = [ `A ]
  
  let _ =
    match `A with
    | #t ->
        ___bisect_visit___ 0;
        ()


Module.

  $ bash ../test.sh <<'EOF'
  > module type L = module type of List
  > let _ =
  >   match (module List : L) with
  >   | (module L) -> ()
  > EOF
  module type L = module type of List
  
  let _ =
    match (module List : L) with
    | (module L) ->
        ___bisect_visit___ 0;
        ()
