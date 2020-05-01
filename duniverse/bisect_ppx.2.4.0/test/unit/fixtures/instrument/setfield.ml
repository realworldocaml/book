type foo = {
  mutable bar : unit;
}

let baz =
  {bar = ()}

(* Basic. *)
let () =
  baz.bar <- print_endline "foo"

(* Record subexpression. *)
let helper () =
  baz

let () =
  (helper ()).bar <- print_endline "foo"
