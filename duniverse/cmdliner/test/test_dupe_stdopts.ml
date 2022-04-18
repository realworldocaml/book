open Cmdliner

let sub_term = Term.const ()
let main_term =
  let what = Arg.(value & pos 0 (some string) None & info [] ~docv:"WHAT") in
  let doc what = `Help (`Pager, what) in
  Term.(ret (const doc $ what))

let sub_info info = info "sub" ~doc:"description of sub"
let main_info info = info "main" ~doc:"description of main"


module Old = struct
  [@@@alert "-deprecated"]
  let term_info n ~doc = Term.info n ~doc
  let sub_cmd = sub_term, sub_info term_info
  let main_cmd = main_term, main_info term_info
  let main ~argv = Term.exit (Term.eval_choice ~argv main_cmd [sub_cmd])
end

module New = struct
  let cmd_info n ~doc = Cmd.info n ~doc
  let sub_cmd = Cmd.v (sub_info cmd_info) sub_term
  let main_cmd = main_term, main_info cmd_info
  let main = Cmd.group ~default:main_term (main_info cmd_info) [sub_cmd]
  let main ~argv = exit (Cmd.eval ~argv main)
end

let main () =
  let argv, backend = match Array.to_list Sys.argv with
  | exec :: backend :: rest -> Array.of_list (exec :: rest), backend
  | [_] -> prerr_endline "Need to specify a backend: old or new"; exit 1
  | _ -> assert false
  in
  match backend with
  | "old" -> Old.main ~argv
  | "new" -> New.main ~argv
  | b -> prerr_endline ("Unknown backend: " ^ b)

let () = main ()
