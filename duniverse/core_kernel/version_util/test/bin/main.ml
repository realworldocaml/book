(* Force [Version_util] to be linked and make sure
   that that one can read/parse build_info. *)
let () = Printf.printf "ocaml_version is %s\n%!" Version_util.ocaml_version
let () = Printf.printf "build_info is %s\n%!" Version_util.build_info
let () = Printf.printf "version is %s\n%!" Version_util.version

let () =
  match Sys.backend_type with
  | Native ->
    if not Version_util.build_system_supports_version_util
    then failwith "Build system does not provide the version util support";
    (match Version_util.Version.current_version () with
     | None -> ()
     | Some [] -> failwith "Build system provided an empty list of hg versions"
     | Some (_ : Version_util.Version.t list) -> ())
  | _ ->
    (* version util is not supported in javascript *)
    ()
;;

let () =
  match Sys.argv with
  | [| _ |] -> ()
  | [| _; "version-util-must-be-set" |] ->
    (match Version_util.Version.current_version () with
     | None | Some [] -> failwith "version util is unset"
     | Some (_ :: _ as l) ->
       Base.List.iter l ~f:(fun { repo; version } ->
         assert (Base.String.mem repo '/' && String.length version >= 12)))
  | _ -> failwith "unexpected command line arguments"
;;

let () =
  (* This makes sure that the code for [insert_version_util] is linked in, so that the
     [count_pattern_occurrences] check in [test_rewrite.ml] is effective at making sure
     we don't rewrite our own code. *)
  match Version_util.Expert.insert_version_util ~contents_of_exe:"" [] with
  | exception _ -> ()
  | _ -> ()
;;
