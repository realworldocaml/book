(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



module Common = Bisect_common

let default_bisect_file = ref "bisect"

let default_bisect_silent = ref "bisect.log"

type message =
  | Unable_to_create_file
  | Unable_to_write_file
  | String of string

let string_of_message = function
  | Unable_to_create_file ->
      " *** Bisect runtime was unable to create file."
  | Unable_to_write_file ->
      " *** Bisect runtime was unable to write file."
  | String s ->
      " *** " ^ s

let full_path fname =
  if Filename.is_implicit fname then
    Filename.concat Filename.current_dir_name fname
  else
    fname

let env_to_fname env default = try Sys.getenv env with Not_found -> !default

let verbose =
  lazy begin
    let fname = env_to_fname "BISECT_SILENT" default_bisect_silent in
    match (String.uppercase [@ocaml.warning "-3"]) fname with
    | "YES" | "ON" -> fun _ -> ()
    | "ERR"        -> fun msg -> prerr_endline (string_of_message msg)
    | _uc_fname    ->
        let oc_l = lazy (
          (* A weird race condition is caused if we use this invocation instead
            let oc = open_out_gen [Open_append] 0o244 (full_path fname) in
            Note that verbose is called only during [at_exit]. *)
          let oc = open_out_bin (full_path fname) in
          at_exit (fun () -> close_out_noerr oc);
          oc)
        in
        fun msg ->
          Printf.fprintf (Lazy.force oc_l) "%s\n" (string_of_message msg)
  end

let verbose message =
  (Lazy.force verbose) message

let get_coverage_data =
  Common.runtime_data_to_string

let write_coverage_data () =
  match get_coverage_data () with
  | None ->
    ()
  | Some data ->
    let rec create_file attempts =
      let filename = Common.random_filename "bisect" in
      let flags = [Open_wronly; Open_creat; Open_excl; Open_binary] in
      match open_out_gen flags 0o644 filename with
      | exception exn ->
        if attempts = 0 then
          raise exn
        else
          create_file (attempts - 1)
      | channel ->
        output_string channel data;
        close_out_noerr channel
    in
    create_file 100

let file_channel () =
  let base_name = full_path (env_to_fname "BISECT_FILE" default_bisect_file) in
  let rec create_file () =
    let filename = Common.random_filename base_name in
    try
      let fd = Unix.(openfile filename [O_WRONLY; O_CREAT; O_EXCL] 0o644) in
      let channel = Unix.out_channel_of_descr fd in
      Some channel
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> create_file ()
    | Unix.Unix_error (code, _, _) ->
      let detail = Printf.sprintf "%s: %s" (Unix.error_message code) filename in
      verbose Unable_to_create_file;
      verbose (String detail);
      None
  in
  create_file ()

let reset_counters =
  Common.reset_counters

let dump_counters_exn =
  Common.write_runtime_data

let dump () =
  match file_channel () with
  | None -> ()
  | Some channel ->
      (try
        dump_counters_exn channel
      with _ ->
        verbose Unable_to_write_file);
      close_out_noerr channel

let register_dump : unit Lazy.t =
  lazy (at_exit dump)

let register_file
    ~bisect_file ~bisect_silent file ~point_count ~point_definitions =
  (match bisect_file with None -> () | Some v -> default_bisect_file := v);
  (match bisect_silent with None -> () | Some v -> default_bisect_silent := v);
  let () = Lazy.force register_dump in
  Common.register_file file ~point_count ~point_definitions
