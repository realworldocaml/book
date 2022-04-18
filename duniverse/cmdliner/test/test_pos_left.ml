open Cmdliner

let pos l =
  print_endline (String.concat "\n" l)

let test_pos_left =
  let l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
  let info = Cmd.info "test_pos" ~doc:"Test pos left" in
  Cmd.v info Term.(const pos $ l)

let () = exit (Cmd.eval test_pos_left)
