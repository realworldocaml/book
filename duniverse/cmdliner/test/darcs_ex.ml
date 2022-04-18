(* Example from the documentation, this code is in public domain. *)

(* Implementations, just print the args. *)

type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb; prehook : string option }

let str = Printf.sprintf
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let opt_str_str = opt_str (fun s -> s)
let verb_str = function
  | Normal -> "normal" | Quiet -> "quiet" | Verbose -> "verbose"

let pr_copts oc copts = Printf.fprintf oc
    "debug = %B\nverbosity = %s\nprehook = %s\n"
    copts.debug (verb_str copts.verb) (opt_str_str copts.prehook)

let initialize copts repodir = Printf.printf
    "%arepodir = %s\n" pr_copts copts repodir

let record copts name email all ask_deps files = Printf.printf
    "%aname = %s\nemail = %s\nall = %B\nask-deps = %B\nfiles = %s\n"
    pr_copts copts (opt_str_str name) (opt_str_str email) all ask_deps
    (String.concat ", " files)

let help copts man_format cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic ->
    let topics = "topics" :: "patterns" :: "environment" :: cmds in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t ->
        let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
        `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

open Cmdliner

(* Help sections common to all commands *)

let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use $(mname) $(i,COMMAND) --help for help on a single command.";`Noblank;
 `P "Use $(mname) $(b,help patterns) for help on patch matching."; `Noblank;
 `P "Use $(mname) $(b,help environment) for help on environment variables.";
 `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";]

(* Options common to all commands *)

let copts debug verb prehook = { debug; verb; prehook }
let copts_t =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  let prehook =
    let doc = "Specify command to run before this $(mname) command." in
    Arg.(value & opt (some string) None & info ["prehook"] ~docs ~doc)
  in
  Term.(const copts $ debug $ verb $ prehook)

(* Commands *)

let sdocs = Manpage.s_common_options

let initialize_cmd =
  let repodir =
    let doc = "Run the program in repository directory $(docv)." in
    Arg.(value & opt file Filename.current_dir_name & info ["repodir"]
           ~docv:"DIR" ~doc)
  in
  let doc = "make the current directory a repository" in
  let man = [
    `S Manpage.s_description;
    `P "Turns the current directory into a Darcs repository. Any
       existing files and subdirectories become …";
    `Blocks help_secs; ]
  in
  let info = Cmd.info "initialize" ~doc ~sdocs ~man in
  Cmd.v info Term.(const initialize $ copts_t $ repodir)

let record_cmd =
  let pname =
    let doc = "Name of the patch." in
    Arg.(value & opt (some string) None & info ["m"; "patch-name"] ~docv:"NAME"
           ~doc)
  in
  let author =
    let doc = "Specifies the author's identity." in
    Arg.(value & opt (some string) None & info ["A"; "author"] ~docv:"EMAIL"
           ~doc)
  in
  let all =
    let doc = "Answer yes to all patches." in
    Arg.(value & flag & info ["a"; "all"] ~doc)
  in
  let ask_deps =
    let doc = "Ask for extra dependencies." in
    Arg.(value & flag & info ["ask-deps"] ~doc)
  in
  let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILE or DIR") in
  let doc = "create a patch from unrecorded changes" in
  let man =
    [`S Manpage.s_description;
     `P "Creates a patch from changes in the working tree. If you specify
         a set of files…";
     `Blocks help_secs; ]
  in
  let info = Cmd.info "record" ~doc ~sdocs ~man in
  Cmd.v info
    Term.(const record $ copts_t $ pname $ author $ all $ ask_deps $ files)

let help_cmd =
  let topic =
    let doc = "The topic to get help on. $(b,topics) lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about darcs and darcs commands" in
  let man =
    [`S Manpage.s_description;
     `P "Prints help about darcs commands and other subjects…";
     `Blocks help_secs; ]
  in
  let info = Cmd.info "help" ~doc ~man in
  Cmd.v info
    Term.(ret (const help $ copts_t $ Arg.man_format $ Term.choice_names $
               topic))

let main_cmd =
  let doc = "a revision control system" in
  let man = help_secs in
  let info = Cmd.info "darcs" ~version:"v1.1.1" ~doc ~sdocs ~man in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  Cmd.group info ~default [initialize_cmd; record_cmd; help_cmd]

let () = exit (Cmd.eval main_cmd)
