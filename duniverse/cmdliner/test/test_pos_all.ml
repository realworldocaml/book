open Cmdliner

let pos_all all = print_endline (String.concat "\n" all)

let test_pos_all =
  let docv = "THEARG" in
  let all = Arg.(value & pos_all string [] & info [] ~docv) in
  let info = Cmd.info "test_pos_all" ~doc:"Test pos all" in
  Cmd.v info Term.(const pos_all $ all)

let () = exit (Cmd.eval test_pos_all)
