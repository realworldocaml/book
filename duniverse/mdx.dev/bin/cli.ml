open Cmdliner
open Result

let named wrapper =
  Term.(app (const wrapper))

let non_deterministic =
  let doc = "Run non-deterministic tests." in
  named (fun x -> `Non_deterministic x)
    Arg.(value & flag & info ["non-deterministic"; "n"] ~doc)

let syntax =
  let parse = function
    | "normal" -> `Ok Mdx.Normal
    | "cram" -> `Ok Mdx.Cram
    | s -> `Error (Format.sprintf "unrecognized syntax %S" s)
  in
  let print fmt syn =
    Format.fprintf fmt "%s"
      (match syn with
       | Mdx.Normal -> "normal"
       | Mdx.Cram -> "cram")
  in
  let syntax = parse, print in
  let doc = "Which syntax to use. Either 'normal' or 'cram'." in
  named (fun x -> `Syntax x)
    Arg.(value & opt (some syntax) None & info ["syntax"] ~doc ~docv:"SYNTAX")

let file_docv = "FILE"

let file =
  let doc = "The file to use." in
  named (fun x -> `File x)
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:file_docv)

let section =
  let doc =
    "Select file sub-sections. Will be interpreted as a Perl regular expression."
  in
  named (fun x -> `Section x)
    Arg.(value & opt (some string) None & info ["section"; "s"] ~doc ~docv:"PAT")

let not_verbose =
  let doc = "Do not show the result of evaluating toplevel phrases." in
  named (fun x -> `Not_verbose x)
    Arg.(value & flag & info ["silent-eval"] ~doc)

let silent =
  let doc = "Do not show any (phrases and findlib directives) results." in
  named (fun x -> `Silent x)
    Arg.(value & flag & info ["silent"] ~doc)

let verbose_findlib =
  let doc =
    "Show the result of evaluating findlib directives in toplevel fragments."
  in
  named (fun x -> `Verbose_findlib x)
    Arg.(value & flag & info ["verbose-findlib"] ~doc)

let prelude =
  let doc =
    "A file to load as prelude. Can be prefixed with $(i,env:) to specify \
     a specific environment to load the prelude in. Multiple prelude files \
     can be provided:they will be evaluated in the order they are provided \
     on the command-line."
  in
  named (fun x -> `Prelude x)
    Arg.(value & opt_all string [] & info ["prelude"] ~doc ~docv:"FILE")

let prelude_str =
  let doc =
    "A string to load as prelude. Can be prefixed with $(i,env:) to specify \
     a specific environment to load the prelude in (the environment name \
     should not contain any spaces. Multiple prelude strings can be provided: \
     they will be evaluated in the order they are provided on the command-line."
  in
  named (fun x -> `Prelude_str x)
    Arg.(value & opt_all string [] & info ["prelude-str"] ~doc ~docv:"STR")

let root =
  let doc = "The directory to run the tests from." in
  named (fun x -> `Root x)
    Arg.(value & opt (some string) None & info ["root"] ~doc ~docv:"DIR")

let direction =
  let doc = "Direction of file synchronization. $(b,to-ml) assumes \
             the .md file is the reference and updates the .ml and \
             the .md files accordingly. $(b,to-md) assumes the .ml \
             file is the reference and updates the .md file \
             accordingly. The default is $(b,to-md)." in
  let opt_names =
    [ "to-md", `To_md
    ; "to-ml", `To_ml ]
  in
  let names = ["direction"] in
  let docv = String.concat "|" (List.map fst opt_names) in
  let docv = "{" ^ docv ^ "}" in
  named (fun x -> `Direction x)
    Arg.(value & opt (enum opt_names) `To_md & info names ~doc ~docv)

let force_output =
  let doc = "Force generation of corrected file (even if there was no diff)" in
  named (fun x -> `Force_output x)
    Arg.(value & flag & info ["force-output"] ~doc)

type output =
  | File of string
  | Stdout

let output_conv =
  let (sparse, sprint) = Arg.string in
  let parse s =
    match sparse s with
    | `Ok "-" -> Ok Stdout
    | `Ok s -> Ok (File s)
    | `Error msg -> Error (`Msg msg)
  in
  let print fmt = function
    | Stdout -> sprint fmt "-"
    | File s -> sprint fmt s
  in
  Arg.conv ~docv:"OUTPUT" (parse, print)

let output =
  let docv = "OUTPUT" in
  let doc =
    Printf.sprintf
      "Specify where to write the command output. $(docv) should be $(b,-) for \
       stdout or a filename. Defaults to $(i,%s).corrected. \
       Note that setting this option implies $(b,--force-output)."
      file_docv
  in
  named (fun x -> `Output x)
    Arg.(value & opt (some output_conv) None & info ~doc ~docv ["o"; "output"])

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup =
  named (fun x -> `Setup x)
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
