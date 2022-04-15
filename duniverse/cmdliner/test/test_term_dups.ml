open Cmdliner

let dups p p_dup o o_dup =
  let b = string_of_bool in
  print_endline (String.concat "\n" [p; p_dup; b o; b o_dup;])

let test_pos =
  let p =
    let doc = "First pos argument should show up only once in the docs" in
    Arg.(value & pos 0 string "undefined" & info [] ~doc ~docv:"POS")
  in
  let o =
    let doc = "This should show up only once in the docs" in
    Arg.(value & flag & info ["f"; "flag"] ~doc)
  in
  let info = Cmd.info "test_term_dups" ~doc:"Test multiple term usage" in
  Cmd.v info Term.(const dups $ p $ p $ o $ o)

let () = exit (Cmd.eval test_pos)
