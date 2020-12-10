(* Basic. *)
let _ =
  object
    val mutable foo = ()

    method bar =
      {<foo = print_endline "foo";>}
  end
