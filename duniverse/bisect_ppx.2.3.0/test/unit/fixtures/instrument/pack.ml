module type EMPTY =
  sig
  end

(* Basic. *)
let _ =
  (module struct end : EMPTY)
