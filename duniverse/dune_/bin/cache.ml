open Stdune
open Import

let name = "cache"

(* CR-someday amokhov: Implement other commands supported by Jenga. *)

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dune can share build artifacts between workspaces. Currently, the only
        action supported by this command is `trim`, but we plan to provide more
        functionality soon. |}
  ; `S "ACTIONS"
  ; `P {|$(b,trim) trim the shared cache to free space.|}
  ; `S "EXAMPLES"
  ; `Pre
      {|Trimming the Dune cache to 1 GB.
        
        \$ dune cache trim --trimmed-size=1GB |}
  ; `Pre
      {|Trimming 500 MB from the Dune cache.
        
        \$ dune cache trim --size=500MB |}
  ; `Blocks Common.help_secs
  ]

let doc = "Manage the shared cache of build artifacts"

let info = Cmd.info name ~doc ~man

let trim ~trimmed_size ~size =
  Log.init_disabled ();
  let open Result.O in
  match
    let+ goal =
      match (trimmed_size, size) with
      | Some trimmed_size, None -> Result.Ok trimmed_size
      | None, Some size ->
        Result.Ok (Int64.sub (Dune_cache.Trimmer.overhead_size ()) size)
      | _ -> Result.Error "specify either --size or --trimmed-size"
    in
    Dune_cache.Trimmer.trim ~goal
  with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok { trimmed_bytes } ->
    User_message.print
      (User_message.make [ Pp.textf "Freed %s" (Bytes_unit.pp trimmed_bytes) ])

type mode =
  | Trim
  | Start_deprecated
  | Stop_deprecated

let modes =
  [ ("start", Start_deprecated); ("stop", Stop_deprecated); ("trim", Trim) ]

(* CR-someday amokhov: See https://github.com/ocaml/dune/issues/4471. *)

(* We don't want to list deprecated subcommands in help. *)
let non_deprecated_modes = [ ("trim", Trim) ]

(* We do want to print a nice error message if a deprecated subcommand is
   run. *)
let deprecated_error () =
  User_error.raise
    [ Pp.text
        "Dune no longer uses the cache daemon, and so the `start` and `stop` \
         subcommands of `dune cache` were removed."
    ]

let term =
  Term.ret
  @@ let+ mode =
       Arg.(
         value
         & pos 0 (some (enum modes)) None
         & info [] ~docv:"ACTION"
             ~doc:
               (Printf.sprintf "The cache action to perform (%s)"
                  (Arg.doc_alts_enum non_deprecated_modes)))
     and+ trimmed_size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "trimmed-size" ]
             ~doc:"Size to trim from the cache.")
     and+ size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "size" ] ~doc:"Size to trim the cache to.")
     in
     match mode with
     | Some Trim -> `Ok (trim ~trimmed_size ~size)
     | Some Start_deprecated | Some Stop_deprecated -> deprecated_error ()
     | None -> `Help (`Pager, Some name)

let command = Cmd.v info term
