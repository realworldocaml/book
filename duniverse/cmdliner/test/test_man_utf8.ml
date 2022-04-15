open Cmdliner

let nop () = print_endline "It's the manual that is of interest."

let test_pos =
  let doc = "UTF-8 test: \u{1F42B} íöüóőúűéáăîâșț ÍÜÓŐÚŰÉÁĂÎÂȘȚ 雙峰駱駝" in
  Cmd.v (Cmd.info "test_pos" ~doc) Term.(const nop $ const ())

let () = exit (Cmd.eval test_pos)
