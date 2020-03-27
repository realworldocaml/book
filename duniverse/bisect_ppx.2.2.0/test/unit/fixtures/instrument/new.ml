class foo =
  object
  end

class bar = fun () () ->
  object
  end

(* Basic. *)
let _ =
  new foo

(* With arguments. *)
let _ =
  new bar () ()

(* In tail position. *)
let f () =
  new foo

(* In tail position with arguments. *)
let f () =
  new bar () ()
