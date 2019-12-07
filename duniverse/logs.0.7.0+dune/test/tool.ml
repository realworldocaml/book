(* This code is in the public domain. *)

(* Example setup for a simple command line tool with colorful output. *)

let hello _ msg =
  Logs.app (fun m -> m "%s" msg);
  Logs.info (fun m -> m "End-user information.");
  Logs.debug (fun m -> m "Developer information.");
  Logs.err (fun m -> m "Something bad happened.");
  Logs.warn (fun m -> m "Something bad may happen in the future.");
  ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


(* Command line interface *)

open Cmdliner

let setup_log =
  let env = Arg.env_var "TOOL_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let msg =
  let doc = "The message to output."  in
  Arg.(value & pos 0 string "Hello horrible world!" & info [] ~doc)

let main () =
  match Term.(eval (const hello $ setup_log $ msg, Term.info "tool")) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)

let () = main ()
