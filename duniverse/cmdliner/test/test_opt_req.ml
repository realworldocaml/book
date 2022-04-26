open Cmdliner

let opt o = print_endline o

let test_opt =
  let req =
    Arg.(required & opt (some string) None & info ["r"; "req"] ~docv:"ARG")
  in
  let info =
    let doc = "Test optional required arguments (don't do this)" in
    Cmd.info "test_opt_req"~doc
  in
  Cmd.v info Term.(const opt $ req)

let () = exit (Cmd.eval test_opt)
