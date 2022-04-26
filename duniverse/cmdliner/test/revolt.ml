(* Example from the documentation, this code is in public domain. *)

let revolt () = print_endline "Revolt!"

open Cmdliner

let revolt_t = Term.(const revolt $ const ())
let cmd = Cmd.v (Cmd.info "revolt") revolt_t
let () = exit (Cmd.eval cmd)
