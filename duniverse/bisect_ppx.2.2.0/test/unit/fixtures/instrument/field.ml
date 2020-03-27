type foo = {
  bar : unit;
}

let baz =
  {bar = ()}

(* Basic. *)
let () =
  baz.bar

(* Record subexpression. *)
let helper () =
  baz

let () =
  (helper ()).bar
