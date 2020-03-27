(* CR-someday trefis: the "deps" and "targets" subcommands currently output
   their result on stdout.
   It would make the interaction with jenga nicer if we could specify a file to
   output the result to. *)

open Odoc_odoc
open Cmdliner

let convert_syntax : Odoc_html.Tree.syntax Arg.converter =
  let syntax_parser str =
    match str with
  | "ml" | "ocaml" -> `Ok Odoc_html.Tree.OCaml
  | "re" | "reason" -> `Ok Odoc_html.Tree.Reason
  | s -> `Error (Printf.sprintf "Unknown syntax '%s'" s)
  in
  let syntax_printer fmt syntax =
    Format.pp_print_string fmt (Odoc_html.Tree.string_of_syntax syntax)
  in
  (syntax_parser, syntax_printer)

let convert_directory ?(create=false) () : Fs.Directory.t Arg.converter =
  let (dir_parser, dir_printer) = Arg.string in
  let odoc_dir_parser str =
    let () = if create then Fs.Directory.(mkdir_p (of_string str)) in
    match dir_parser str with
    | `Ok res  -> `Ok (Fs.Directory.of_string res)
    | `Error e -> `Error e
  in
  let odoc_dir_printer fmt dir = dir_printer fmt (Fs.Directory.to_string dir) in
  (odoc_dir_parser, odoc_dir_printer)

(* Very basic validation and normalization for URI paths. *)
let convert_uri : Odoc_html.Tree.uri Arg.converter =
  let parser str =
    if String.length str = 0 then
      `Error "invalid URI"
    else
      (* The URI is absolute if it starts with a scheme or with '/'. *)
      let is_absolute =
        List.exists ["http"; "https"; "file"; "data"; "ftp"]
          ~f:(fun scheme -> Astring.String.is_prefix ~affix:(scheme ^ ":") str)
        || String.get str 0 = '/'
      in
      let last_char = String.get str (String.length str - 1) in
      let str = if last_char <> '/' then str ^ "/" else str in
      `Ok Odoc_html.Tree.(if is_absolute then Absolute str else Relative str)
  in
  let printer ppf = function
    | Odoc_html.Tree.Absolute uri
    | Odoc_html.Tree.Relative uri -> Format.pp_print_string ppf uri
  in
  (parser, printer)

let handle_error = function
  | Result.Ok () -> ()
  | Error (`Cli_error msg) ->
    Printf.eprintf "%s\n%!" msg;
    exit 2
  | Error (`Msg msg) ->
    Printf.eprintf "ERROR: %s\n%!" msg;
    exit 1

let docs = "ARGUMENTS"

let odoc_file_directories =
  let doc =
    "Where to look for required .odoc files. \
     (Can be present several times)."
  in
  Arg.(value & opt_all (convert_directory ()) [] &
    info ~docs ~docv:"DIR" ~doc ["I"])

let hidden =
  let doc =
    "Mark the unit as hidden. \
     (Useful for files included in module packs)."
  in
  Arg.(value & flag & info ~docs ~doc ["hidden"])

let warn_error =
  let doc = "Turn warnings into errors." in
  let env = Arg.env_var "ODOC_WARN_ERROR" ~doc:(doc ^ " See option $(opt).") in
  Arg.(value & flag & info ~docs ~doc ~env ["warn-error"])

let dst ?create () =
  let doc = "Output directory where the HTML tree is expected to be saved." in
  Arg.(required & opt (some (convert_directory ?create ())) None &
       info ~docs ~docv:"DIR" ~doc ["o"; "output-dir"])

module Compile : sig
  val output_file : dst:string option -> input:Fs.file -> Fs.file
  val input : string Term.t
  val dst : string option Term.t
  val cmd : unit Term.t
  val info: Term.info
end = struct
  let has_page_prefix file =
    file
    |> Fs.File.basename
    |> Fs.File.to_string
    |> Astring.String.is_prefix ~affix:"page-"

  let output_file ~dst ~input = match dst with
  | Some file ->
      let output = Fs.File.of_string file in
      if Fs.File.has_ext ".mld" input && not (has_page_prefix output)
      then (
        Printf.eprintf "ERROR: the name of the .odoc file produced from a \
                        .mld must start with 'page-'\n%!";
        exit 1
      );
      output
  | None ->
      let output =
        if Fs.File.has_ext ".mld" input && not (has_page_prefix input)
        then
          let directory = Fs.File.dirname input in
          let name = Fs.File.basename input in
          let name = "page-" ^ Fs.File.to_string name in
          Fs.File.create ~directory ~name
        else input
      in
      Fs.File.(set_ext ".odoc" output)

  let compile hidden directories resolve_fwd_refs dst package_name input
      warn_error =
    let env =
      Env.create ~important_digests:(not resolve_fwd_refs) ~directories
    in
    let input = Fs.File.of_string input in
    let output = output_file ~dst ~input in
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    if Fs.File.has_ext ".cmti" input then
      Compile.cmti ~env ~package:package_name ~hidden ~output ~warn_error input
    else if Fs.File.has_ext ".cmt" input then
      Compile.cmt ~env ~package:package_name ~hidden ~output ~warn_error input
    else if Fs.File.has_ext ".cmi" input then
      Compile.cmi ~env ~package:package_name ~hidden ~output ~warn_error input
    else if Fs.File.has_ext ".mld" input then
      Compile.mld ~env ~package:package_name ~output ~warn_error input
    else
      Error (`Cli_error "Unknown extension, expected one of: cmti, cmt, cmi or mld.\n%!")

  let input =
    let doc = "Input cmti, cmt, cmi or mld file" in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])

  let dst =
    let doc = "Output file path. Non-existing intermediate directories are
                 created. If absent outputs a $(i,BASE).odoc file in the same
                 directory as the input file where $(i,BASE) is the basename
                 of the input file (for mld files the \"page-\" prefix will be
                 added if not already present in the input basename)."
    in
    Arg.(value & opt (some string) None & info ~docs ~docv:"PATH" ~doc ["o"])

  let cmd =
    let pkg =
      let doc = "Package the input is part of" in
      Arg.(required & opt (some string) None &
           info ~docs ~docv:"PKG" ~doc ["package"; "pkg"])
    in
    let resolve_fwd_refs =
      let doc = "Try resolving forward references" in
      Arg.(value & flag & info ~doc ["r";"resolve-fwd-refs"])
    in
    Term.(const handle_error $ (const compile $ hidden $ odoc_file_directories $
          resolve_fwd_refs $ dst $ pkg $ input $ warn_error))

  let info =
    Term.info "compile"
      ~doc:"Compile a cmti, cmt, cmi or mld file to an odoc file."
end

module Support_files_command = struct
  let support_files without_theme output_dir =
    Support_files.write ~without_theme output_dir

  let without_theme =
    let doc = "Don't copy the default theme to output directory." in
    Arg.(value & flag & info ~doc ["without-theme"])

  let cmd =
    Term.(const support_files $ without_theme $ dst ~create:true ())

  let info =
    let doc =
      "Copy the support files (e.g. default theme, JavaScript files) to the \
       output directory."
    in
    Term.info ~doc "support-files"
end

module Css = struct
  let cmd = Support_files_command.cmd

  let info =
    let doc =
      "DEPRECATED: Use `odoc support-files' to copy the CSS file for the \
       default theme."
    in
    Term.info ~doc "css"
end

module Odoc_html : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let html semantic_uris closed_details _hidden directories output_dir index_for
        syntax theme_uri input_file warn_error =
    Odoc_html.Tree.Relative_link.semantic_uris := semantic_uris;
    Odoc_html.Tree.open_details := not closed_details;
    let env = Env.create ~important_digests:false ~directories in
    let file = Fs.File.of_string input_file in
    match index_for with
    | None ->
      Html_page.from_odoc ~env ~syntax ~theme_uri ~output:output_dir file
    | Some pkg_name ->
      Html_page.from_mld ~env ~syntax ~output:output_dir ~package:pkg_name ~warn_error file

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.odoc" [])
    in
    let semantic_uris =
      let doc = "Generate pretty (semantic) links" in
      Arg.(value & flag (info ~doc ["semantic-uris";"pretty-uris"]))
    in
    let closed_details =
      let doc = "If this flag is passed <details> tags (used for includes) will \
                 be closed by default."
      in
      Arg.(value & flag (info ~doc ["closed-details"]))
    in
    let index_for =
      let doc = "DEPRECATED: you should use 'odoc compile' to process .mld \
		 files. When this argument is given, then the input file is \
		 expected to be a .mld file. The output will be a \
                 \"index.html\" file in the output directory. \
                 PKG is using to correctly resolve and link references inside \
		 the input file"
      in
      Arg.(value & opt (some string) None & info ~docv:"PKG" ~doc ["index-for"])
    in
    let theme_uri =
      let doc = "Where to look for theme files (e.g. `URI/odoc.css'). \
                 Relative URIs are resolved using `--output-dir' as a target." in
      let default = Odoc_html.Tree.Relative "./" in
      Arg.(value & opt convert_uri default & info ~docv:"URI" ~doc ["theme-uri"])
    in
    let syntax =
      let doc = "Available options: ml | re" in
      let env = Arg.env_var "ODOC_SYNTAX"
      in
      Arg.(value & opt (pconv convert_syntax) (Odoc_html.Tree.OCaml) @@ info ~docv:"SYNTAX" ~doc ~env ["syntax"])
    in
    Term.(const handle_error $ (const html $ semantic_uris $ closed_details $ hidden $
          odoc_file_directories $ dst ~create:true () $ index_for $ syntax $
          theme_uri $ input $ warn_error))

  let info =
    Term.info ~doc:"Generates an html file from an odoc one" "html"
end

module Html_fragment : sig
  val cmd : unit Term.t
  val info: Term.info
end = struct

  let html_fragment directories xref_base_uri output_file input_file warn_error =
    let env = Env.create ~important_digests:false ~directories in
    let input_file = Fs.File.of_string input_file in
    let output_file = Fs.File.of_string output_file in
    let xref_base_uri =
      if xref_base_uri = "" then xref_base_uri
      else
        let last_char = String.get xref_base_uri (String.length xref_base_uri - 1) in
        if last_char <> '/' then xref_base_uri ^ "/" else xref_base_uri
    in
    Html_fragment.from_mld ~env ~xref_base_uri ~output:output_file ~warn_error input_file

  let cmd =
    let output =
      let doc = "Output HTML fragment file" in
      Arg.(value & opt string "/dev/stdout" &
          info ~docs ~docv:"file.html" ~doc ["o"; "output-file"])
        in
    let input =
      let doc = "Input documentation page file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.mld" [])
    in
    let xref_base_uri =
      let doc = "Base URI used to resolve cross-references. Set this to the \
                 root of the global docset during local development. By default \
                 `.' is used." in
      Arg.(value & opt string "" & info ~docv:"URI" ~doc ["xref-base-uri"])
    in
    Term.(const handle_error $ (const html_fragment $ odoc_file_directories $
          xref_base_uri $ output $ input $ warn_error))

  let info =
    Term.info ~doc:"Generates an html fragment file from an mld one" "html-fragment"
end

module Depends = struct
  module Compile = struct
    let list_dependencies input_file =
      let deps = Depends.for_compile_step (Fs.File.of_string input_file) in
      List.iter ~f:(fun t ->
        Printf.printf "%s %s\n"
          (Depends.Compile.name t)
          (Digest.to_hex @@ Depends.Compile.digest t)
      ) deps;
      flush stdout

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.cm{i,t,ti}" [])
    in
    Term.(const list_dependencies $ input)

  let info =
    Term.info "compile-deps"
      ~doc:"List units (with their digest) which needs to be compiled in order \
            to compile this one. The unit itself and its digest is also \
            reported in the output."
  end

  module Odoc_html = struct
    let list_dependencies input_file =
      let open Or_error in
      Depends.for_html_step (Fs.Directory.of_string input_file) >>= fun depends ->
      List.iter depends
        ~f:(fun (root : Odoc_model.Root.t) ->
          Printf.printf "%s %s %s\n"
            root.package
            (Odoc_model.Root.Odoc_file.name root.file)
            (Digest.to_hex root.digest)
        );
      Ok ()

    let cmd =
      let input =
        let doc = "Input directory" in
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PKG_DIR" [])
      in
      Term.(const handle_error $ (const list_dependencies $ input))

  let info =
    Term.info "html-deps"
      ~doc:"lists the packages which need to be in odoc's load path to process \
            html from the .odoc files in the given directory"
  end
end

module Targets = struct
  module Compile = struct
    let list_targets dst input =
      let input = Fs.File.of_string input in
      let output = Compile.output_file ~dst ~input in
      Printf.printf "%s\n" (Fs.File.to_string output);
      flush stdout

    let cmd =
      Term.(const list_targets $ Compile.dst $ Compile.input)

    let info =
      Term.info "compile-targets" ~doc:"TODO: Fill in."
  end

  module Odoc_html = struct
    let list_targets directories output_dir odoc_file =
      let open Or_error in
      let env = Env.create ~important_digests:false ~directories in
      let odoc_file = Fs.File.of_string odoc_file in
      Targets.of_odoc_file ~env ~output:output_dir odoc_file >>= fun targets ->
      let targets = List.map ~f:Fs.File.to_string targets in
      Printf.printf "%s\n%!" (String.concat ~sep:"\n" targets);
      Ok ()

    let cmd =
      let input =
        let doc = "Input file" in
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.odoc" [])
      in
      Term.(const handle_error $ (const list_targets $ odoc_file_directories $
            dst () $ input))

    let info =
      Term.info "html-targets" ~doc:"TODO: Fill in."
  end

  module Support_files =
  struct
    let list_targets without_theme output_directory =
      Support_files.print_filenames ~without_theme output_directory

    let cmd =
      Term.(const list_targets $ Support_files_command.without_theme $ dst ())

    let info =
      Term.info "support-files-targets"
        ~doc:"Lists the names of the files that 'odoc support-files' outputs."
  end
end

let () =
  Printexc.record_backtrace true;

  let subcommands =
    [ Compile.(cmd, info)
    ; Odoc_html.(cmd, info)
    ; Html_fragment.(cmd, info)
    ; Support_files_command.(cmd, info)
    ; Css.(cmd, info)
    ; Depends.Compile.(cmd, info)
    ; Depends.Odoc_html.(cmd, info)
    ; Targets.Compile.(cmd, info)
    ; Targets.Odoc_html.(cmd, info)
    ; Targets.Support_files.(cmd, info)
    ]
  in
  let default =
    let print_default () =
      let available_subcommands =
        List.map subcommands ~f:(fun (_, info) -> Term.name info)
      in
      Printf.printf "Available subcommands: %s\n\
                     See --help for more information.\n%!"
        (String.concat ~sep:", " available_subcommands)
    in
    Term.(const print_default $ const ()),
    Term.info ~version:"%%VERSION%%" "odoc"
  in
  match Term.eval_choice ~err:Format.err_formatter default subcommands with
  | `Error _ ->
    Format.pp_print_flush Format.err_formatter ();
    exit 2
  | _ -> ()
