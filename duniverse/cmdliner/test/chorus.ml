(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command *)

let chorus count msg = for i = 1 to count do print_endline msg done

(* Command line interface *)

open Cmdliner

let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let msg =
  let env =
    let doc = "Overrides the default message to print." in
    Cmd.Env.info "CHORUS_MSG" ~doc
  in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

let chorus_t = Term.(const chorus $ count $ msg)

let cmd =
  let doc = "Print a customizable message repeatedly" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bugs@example.org>." ]
  in
  let info = Cmd.info "chorus" ~version:"v1.1.1" ~doc ~man in
  Cmd.v info chorus_t

let main () = exit (Cmd.eval cmd)
let () = main ()
