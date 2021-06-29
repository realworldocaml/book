Or-pattern instrumentation does not prevent row type generalization.

  $ bash ../test.sh <<'EOF'
  > type t = [ `A | `B ]
  > module M :
  > sig
  >   val f : [< t ] -> unit
  > end =
  > struct
  >   let f = function
  >     | `A | `B -> ()
  > end
  > EOF
  type t = [ `A | `B ]
  
  module M : sig
    val f : [< t ] -> unit
  end = struct
    let f = function
      | (`A | `B) as ___bisect_matched_value___ ->
          (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
             ___bisect_matched_value___
           with
          | `A ->
              ___bisect_visit___ 0;
              ()
          | `B ->
              ___bisect_visit___ 1;
              ()
          | _ -> ());
          ()
  end
