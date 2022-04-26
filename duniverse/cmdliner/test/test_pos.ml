open Cmdliner

let pos l t r =
  print_endline (String.concat "\n" (l @ ["--"; t; "--"] @ r))

let test_pos =
  let l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
  let t = Arg.(value & pos 2 string "undefined" & info [] ~docv:"TWO") in
  let r = Arg.(value & pos_right 2 string [] & info [] ~docv:"RIGHT") in
  let info = Cmd.info "test_pos" ~doc:"Test pos arguments" in
  Cmd.v info Term.(const pos $ l $ t $ r)

let () = exit (Cmd.eval test_pos)
