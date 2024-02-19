open Stdune
open Import
module Sub_dirs = Dune_engine.Sub_dirs
module Vcs = Dune_engine.Vcs

let is_a_source_file path =
  (match Path.extension path with
  | ".flv"
  | ".gif"
  | ".ico"
  | ".jpeg"
  | ".jpg"
  | ".mov"
  | ".mp3"
  | ".mp4"
  | ".otf"
  | ".pdf"
  | ".png"
  | ".ttf"
  | ".woff" -> false
  | _ -> true)
  && Path.is_file path

let subst_string s path ~map =
  let len = String.length s in
  let longest_var = String.longest (String.Map.keys map) in
  let double_percent_len = String.length "%%" in
  let loc_of_offset ~ofs ~len =
    let rec loop lnum bol i =
      if i = ofs then
        let pos =
          { Lexing.pos_fname = Path.to_string path
          ; pos_cnum = i
          ; pos_lnum = lnum
          ; pos_bol = bol
          }
        in
        { Loc.start = pos; stop = { pos with pos_cnum = pos.pos_cnum + len } }
      else
        match s.[i] with
        | '\n' -> loop (lnum + 1) (i + 1) (i + 1)
        | _ -> loop lnum bol (i + 1)
    in
    loop 1 0 0
  in
  let rec loop i acc =
    if i = len then acc
    else
      match s.[i] with
      | '%' -> after_percent (i + 1) acc
      | _ -> loop (i + 1) acc
  and after_percent i acc =
    if i = len then acc
    else
      match s.[i] with
      | '%' -> after_double_percent ~start:(i - 1) (i + 1) acc
      | _ -> loop (i + 1) acc
  and after_double_percent ~start i acc =
    if i = len then acc
    else
      match s.[i] with
      | '%' -> after_double_percent ~start:(i - 1) (i + 1) acc
      | 'A' .. 'Z' | '_' -> in_var ~start (i + 1) acc
      | _ -> loop (i + 1) acc
  and in_var ~start i acc =
    if i - start > longest_var + double_percent_len then loop i acc
    else if i = len then acc
    else
      match s.[i] with
      | '%' -> end_of_var ~start (i + 1) acc
      | 'A' .. 'Z' | '_' -> in_var ~start (i + 1) acc
      | _ -> loop (i + 1) acc
  and end_of_var ~start i acc =
    if i = len then acc
    else
      match s.[i] with
      | '%' -> (
        let var = String.sub s ~pos:(start + 2) ~len:(i - start - 3) in
        match String.Map.find map var with
        | None -> in_var ~start:(i - 1) (i + 1) acc
        | Some (Ok repl) ->
          let acc = (start, i + 1, repl) :: acc in
          loop (i + 1) acc
        | Some (Error msg) ->
          let loc = loc_of_offset ~ofs:start ~len:(i + 1 - start) in
          User_error.raise ~loc [ Pp.text msg ])
      | _ -> loop (i + 1) acc
  in
  match List.rev (loop 0 []) with
  | [] -> None
  | repls ->
    let result_len =
      List.fold_left repls ~init:(String.length s) ~f:(fun acc (a, b, repl) ->
          acc - (b - a) + String.length repl)
    in
    let buf = Buffer.create result_len in
    let pos =
      List.fold_left repls ~init:0 ~f:(fun pos (a, b, repl) ->
          Buffer.add_substring buf s pos (a - pos);
          Buffer.add_string buf repl;
          b)
    in
    Buffer.add_substring buf s pos (len - pos);
    Some (Buffer.contents buf)

let subst_file path ~map =
  let s = Io.read_file path in
  let s =
    if Path.is_root (Path.parent_exn path) && Package.is_opam_file path then
      "version: \"%%" ^ "VERSION_NUM" ^ "%%\"\n" ^ s
    else s
  in
  match subst_string s ~map path with
  | None -> ()
  | Some s -> Io.write_file path s

(* Extending the Dune_project APIs, but adding capability to modify *)
module Dune_project = struct
  include Dune_project

  type 'a simple_field =
    { loc : Loc.t
    ; loc_of_arg : Loc.t
    ; arg : 'a
    }

  type t =
    { contents : string
    ; name : Package.Name.t simple_field option
    ; version : string simple_field option
    ; project : Dune_project.t
    }

  let filename = Path.in_source Dune_project.filename

  let load ~dir ~files ~infer_from_opam_files =
    let open Memo.O in
    let+ project =
      (* dir_status only affects warning status, but it will not matter here.
         dune subst will fail with a hard error if the name is missing *)
      let dir_status = Sub_dirs.Status.Normal in
      Dune_project.load ~dir ~files ~infer_from_opam_files ~dir_status
    in
    let open Option.O in
    let* project = project in
    let file =
      Dune_project.file project |> Path.Source.to_string |> Path.in_source
    in
    let contents = Io.read_file file in
    let sexp =
      let lb = Lexbuf.from_string contents ~fname:(Path.to_string file) in
      Dune_lang.Parser.parse lb ~mode:Many_as_one
    in
    let parser =
      let open Dune_lang.Decoder in
      let simple_field name arg =
        let+ loc, x = located (field_o name (located arg)) in
        Option.map x ~f:(fun (loc_of_arg, arg) -> { loc; loc_of_arg; arg })
      in
      enter
        (fields
           (let+ name = simple_field "name" Package.Name.decode
            and+ version = simple_field "version" string
            and+ () = junk_everything in
            Some { contents; name; version; project }))
    in
    Dune_lang.Decoder.parse parser Univ_map.empty sexp

  let project t = t.project

  let subst t ~map ~version =
    let s =
      match version with
      | None -> t.contents
      | Some version -> (
        let replace_text start_ofs stop_ofs repl =
          sprintf "%s%s%s"
            (String.sub t.contents ~pos:0 ~len:start_ofs)
            repl
            (String.sub t.contents ~pos:stop_ofs
               ~len:(String.length t.contents - stop_ofs))
        in
        match t.version with
        | Some v ->
          (* There is a [version] field, overwrite its argument *)
          replace_text v.loc_of_arg.start.pos_cnum v.loc_of_arg.stop.pos_cnum
            (Dune_lang.to_string (Dune_lang.atom_or_quoted_string version))
        | None ->
          let version_field =
            Dune_lang.to_string
              (List
                 [ Dune_lang.atom "version"
                 ; Dune_lang.atom_or_quoted_string version
                 ])
            ^ "\n"
          in
          let ofs =
            ref
              (match t.name with
              | Some { loc; _ } ->
                (* There is no [version] field but there is a [name] one, add
                   the version after it *)
                loc.stop.pos_cnum
              | None ->
                (* If all else fails, add the [version] field after the first
                   line of the file *)
                0)
          in
          let len = String.length t.contents in
          while !ofs < len && t.contents.[!ofs] <> '\n' do
            incr ofs
          done;
          if !ofs < len && t.contents.[!ofs] = '\n' then (
            incr ofs;
            replace_text !ofs !ofs version_field)
          else replace_text !ofs !ofs ("\n" ^ version_field))
    in
    let s = Option.value (subst_string s ~map filename) ~default:s in
    if s <> t.contents then Io.write_file filename s
end

let make_watermark_map ~commit ~version ~dune_project ~info =
  let dune_project = Dune_project.project dune_project in
  let version_num =
    let open Option.O in
    let+ version = version in
    Option.value ~default:version (String.drop_prefix version ~prefix:"v")
  in
  let name = Dune_project.name dune_project in
  (* XXX these error messages aren't particularly good as these values do not
     necessarily come from the project file. It's possible for them to be
     defined in the .opam file directly*)
  let make_value name = function
    | None -> Error (sprintf "variable %S not found in dune-project file" name)
    | Some value -> Ok value
  in
  let make_separated name sep = function
    | None -> Error (sprintf "variable %S not found in dune-project file" name)
    | Some value -> Ok (String.concat ~sep value)
  in
  let make_dev_repo_value = function
    | Some (Package.Source_kind.Host h) ->
      Ok (Package.Source_kind.Host.homepage h)
    | Some (Package.Source_kind.Url url) -> Ok url
    | None -> Error (sprintf "variable dev-repo not found in dune-project file")
  in
  let make_version = function
    | Some s -> Ok s
    | None -> Error "repository does not contain any version information"
  in
  String.Map.of_list_exn
    [ ("NAME", Ok (Dune_project.Name.to_string_hum name))
    ; ("VERSION", make_version version)
    ; ("VERSION_NUM", make_version version_num)
    ; ( "VCS_COMMIT_ID"
      , match commit with
        | None -> Error "repository does not contain any commits"
        | Some s -> Ok s )
    ; ( "PKG_MAINTAINER"
      , make_separated "maintainer" ", " @@ Package.Info.maintainers info )
    ; ("PKG_AUTHORS", make_separated "authors" ", " @@ Package.Info.authors info)
    ; ("PKG_HOMEPAGE", make_value "homepage" @@ Package.Info.homepage info)
    ; ("PKG_ISSUES", make_value "bug-reports" @@ Package.Info.bug_reports info)
    ; ("PKG_DOC", make_value "doc" @@ Package.Info.documentation info)
    ; ("PKG_LICENSE", make_separated "license" ", " @@ Package.Info.license info)
    ; ("PKG_REPO", make_dev_repo_value @@ Package.Info.source info)
    ]

let subst vcs =
  let open Memo.O in
  let* (version, commit), files =
    Memo.fork_and_join
      (fun () ->
        Memo.fork_and_join
          (fun () -> Vcs.describe vcs)
          (fun () -> Vcs.commit_id vcs))
      (fun () -> Vcs.files vcs)
  in
  let+ (dune_project : Dune_project.t) =
    (let files =
       (* Filter-out files form sub-directories *)
       List.fold_left files ~init:String.Set.empty ~f:(fun acc fn ->
           if Path.is_root (Path.parent_exn fn) then
             String.Set.add acc (Path.to_string fn)
           else acc)
     in
     Dune_project.load ~dir:Path.Source.root ~files ~infer_from_opam_files:true)
    >>| function
    | Some dune_project -> dune_project
    | None ->
      User_error.raise
        [ Pp.text
            "There is no dune-project file in the current directory, please \
             add one with a (name <name>) field in it."
        ]
        ~hints:
          [ Pp.text "dune subst must be executed from the root of the project."
          ]
  in
  (match Dune_project.subst_config dune_project.project with
  | Dune_engine.Subst_config.Disabled ->
    User_error.raise
      [ Pp.text
          "dune subst has been disabled in this project. Any use of it is \
           forbidden."
      ]
      ~hints:
        [ Pp.text
            "If you wish to re-enable it, change to (subst enabled) in the \
             dune-project file."
        ]
  | Dune_engine.Subst_config.Enabled -> ());
  let info =
    let loc, name =
      match dune_project.name with
      | None ->
        User_error.raise
          [ Pp.textf
              "The project name is not defined, please add a (name <name>) \
               field to your dune-project file."
          ]
      | Some n -> (n.loc_of_arg, n.arg)
    in
    let package_named_after_project =
      let packages = Dune_project.packages dune_project.project in
      Package.Name.Map.find packages name
    in
    let metadata_from_dune_project () =
      Dune_project.info dune_project.project
    in
    let metadata_from_matching_package () =
      match package_named_after_project with
      | Some pkg -> Ok pkg.info
      | None ->
        Error
          (User_error.make ~loc
             [ Pp.textf "Package %s doesn't exist."
                 (Package.Name.to_string name)
             ])
    in
    let version = Dune_project.dune_version dune_project.project in
    let ok_exn = function
      | Ok s -> s
      | Error e -> raise (User_error.E e)
    in
    if version >= (3, 0) then metadata_from_dune_project ()
    else if version >= (2, 8) then
      match metadata_from_matching_package () with
      | Ok p -> p
      | Error _ -> metadata_from_dune_project ()
    else ok_exn (metadata_from_matching_package ())
  in
  let watermarks = make_watermark_map ~commit ~version ~dune_project ~info in
  Dune_project.subst ~map:watermarks ~version dune_project;
  List.iter files ~f:(fun path ->
      if is_a_source_file path && not (Path.equal path Dune_project.filename)
      then subst_file path ~map:watermarks)

let subst () =
  match
    Sys.readdir "." |> Array.to_list |> String.Set.of_list
    |> Vcs.Kind.of_dir_contents
  with
  | None -> Fiber.return ()
  | Some kind -> Memo.run (subst { kind; root = Path.root })

(** A string that is "3.6.1" but not expanded by [dune subst] *)
let literal_version = "%%" ^ "VERSION%%"

let doc = "Substitute watermarks in source files."

let man =
  let var name desc =
    `Blocks [ `Noblank; `P ("- $(b,%%" ^ name ^ "%%), " ^ desc) ]
  in
  let opam field =
    var
      ("PKG_" ^ String.uppercase field)
      ("contents of the $(b," ^ field ^ ":) field from the opam file")
  in
  [ `S "DESCRIPTION"
  ; `P
      {|Substitute $(b,%%ID%%) strings in source files, in a similar fashion to
          what topkg does in the default configuration.|}
  ; `P
      ({|This command is only meant to be called when a user pins a package to
          its development version. Especially it replaces $(b,|}
     ^ literal_version
     ^ {|) strings by the version obtained from the vcs. Currently only git is
            supported and the version is obtained from the output of:|}
      )
  ; `Pre {|  \$ git describe --always --dirty --abbrev=7|}
  ; `P
      {|$(b,dune subst) substitutes the variables that topkg substitutes with
          the default configuration:|}
  ; var "NAME" "the name of the project (from the dune-project file)"
  ; var "VERSION" "output of $(b,git describe --always --dirty --abbrev=7)"
  ; var "VERSION_NUM"
      ("same as $(b," ^ literal_version
     ^ ") but with a potential leading 'v' or 'V' dropped")
  ; var "VCS_COMMIT_ID" "commit hash from the vcs"
  ; opam "maintainer"
  ; opam "authors"
  ; opam "homepage"
  ; opam "issues"
  ; opam "doc"
  ; opam "license"
  ; opam "repo"
  ; `P
      {|In order to call $(b,dune subst) when your package is pinned, add this line
          to the $(b,build:) field of your opam file:|}
  ; `Pre {|  [dune "subst"] {pinned}|}
  ; `P
      {|Note that this command is meant to be called only from opam files and
          behaves a bit differently from other dune commands. In particular it
          doesn't try to detect the root and must be called from the root of
          the project.|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "subst" ~doc ~man

let term =
  let+ () = Common.build_info
  and+ debug_backtraces = Common.debug_backtraces in
  let config : Dune_config.t =
    { Dune_config.default with
      display = { verbosity = Quiet; status_line = false }
    ; concurrency = Fixed 1
    }
  in
  Dune_engine.Clflags.debug_backtraces debug_backtraces;
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir
    (Path.Outside_build_dir.of_string Common.default_build_dir);
  Dune_config.init config;
  Log.init_disabled ();
  Dune_engine.Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    (Dune_config.for_scheduler config None ~insignificant_changes:`React
       ~signal_watcher:`No)
    subst

let command = Cmd.v info term
