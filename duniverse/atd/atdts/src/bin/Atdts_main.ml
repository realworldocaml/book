(*
   Entry point to the atdts command.
*)

open Printf
open Cmdliner

type conf = {
  input_files: string list;
  version: bool;
}

let run conf =
  if conf.version then (
    print_endline Atdts.Version.version;
    exit 0
  )
  else
    conf.input_files
    |> List.iter (fun atd_file ->
      Atdts.Codegen.run_file atd_file
    )

(***************************************************************************)
(* Command-line processing *)
(***************************************************************************)

let error msg =
  eprintf "Error: %s\n%!" msg;
  exit 1

let input_files_term =
  let info =
    Arg.info []  (* list must be empty for anonymous arguments *)
      ~docv:"PATH"
      ~doc:"Input file in the ATD format with the '.atd' extension"
  in
  let default = [] in
  Arg.value (Arg.pos_all Arg.file default info)

let version_term =
  let info =
    Arg.info ["version"]
      ~doc:"Prints the version of atdts and exits"
  in
  Arg.value (Arg.flag info)

let doc =
  "Type-safe JSON serializers for TypeScript"

(*
   The structure of the help page.
*)
let man = [
  (* 'NAME' and 'SYNOPSIS' sections are inserted here by cmdliner. *)

  `S Manpage.s_description;  (* standard 'DESCRIPTION' section *)
  `P "atdts turns a file containing type definitions into TypeScript \
      code that reads, writes, and validates JSON data.";

  (* 'ARGUMENTS' and 'OPTIONS' sections are inserted here by cmdliner. *)

  `S Manpage.s_examples; (* standard 'EXAMPLES' section *)
  `P "The following is a sample ATD file. 'sample.atd' becomes 'sample.ts' \
      with the command 'atdts sample.atd'.";
  `Pre "\
(* Sample ATD file sample.atd *)

type foo = {
  name: string;                 (* required field *)
  ?description: string option;  (* optional field *)
  ~tags: string list;           (* optional with implicit default *)
  ~price <ts default=\"0.99\">: float;  (* explicit default *)
  items: bar list;
}

(* sum type *)
type bar = [
  | Thing <json name=\"thing\"> of int
  | Nothing <json name=\"nothing\">
]
";

  `S Manpage.s_authors;
  `P "Martin Jambon <martin@r2c.dev>";

  `S Manpage.s_bugs;
  `P "Report issues at https://github.com/ahrefs/atd";

  `S Manpage.s_see_also;
  `P "atdgen, atdj, atds, atdpy"
]

let cmdline_term run =
  let combine input_files version =
    run {
      input_files;
      version;
    }
  in
  Term.(const combine
        $ input_files_term
        $ version_term
       )

let parse_command_line_and_run run =
  let info =
    Cmd.info
      ~doc
      ~man
      "atdts"
  in
  Cmd.v info (cmdline_term run) |> Cmd.eval |> exit

let safe_run conf =
  try run conf
  with
  (* for other exceptions, we show a backtrace *)
  | Failure msg -> error msg
  | Atd.Ast.Atd_error msg -> error msg
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "Error: exception %s\n%s%!"
        (Printexc.to_string e)
        trace

let main () =
  Printexc.record_backtrace true;
  let conf = parse_command_line_and_run safe_run in
  safe_run conf

let () = main ()
