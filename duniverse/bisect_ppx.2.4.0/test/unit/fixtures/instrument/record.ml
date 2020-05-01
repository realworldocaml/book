type foo = {
  bar : unit;
  baz : unit;
}

(* Basic. *)
let initial = {
  bar = print_endline "foo";
  baz = ();
}

(* Record subexpression. *)
let helper () =
  initial

let final = {(helper ()) with
  bar = print_endline "bar";
}
