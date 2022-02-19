module Unix_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)

  module Unix = struct
    open Astring
    include Unix

    let mkdir_p path mode =
      let is_win_drive_letter x =
        String.length x = 2 && x.[1] = ':' && Char.Ascii.is_letter x.[0]
      in
      let sep = Filename.dir_sep in
      let rec mk parent = function
        | [] -> ()
        | name :: names ->
            let path = parent ^ sep ^ name in
            (try Unix.mkdir path mode
             with Unix.Unix_error (Unix.EEXIST, _, _) ->
               if Sys.is_directory path then () (* the directory exists *)
               else Fmt.str "mkdir: %s: is a file" path |> failwith);
            mk path names
      in
      match String.cuts ~empty:true ~sep path with
      | "" :: xs -> mk sep xs
      (* check for Windows drive letter *)
      | dl :: xs when is_win_drive_letter dl -> mk dl xs
      | xs -> mk "." xs
  end

  open M.Syntax

  let time = Unix.gettimeofday
  let getcwd = Sys.getcwd

  let unlink_if_exists file =
    let rec inner ~retries =
      try Unix.unlink file with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          if retries > 5 then
            Fmt.failwith "Failed %d times to unlink file %s (Unix.EINTR)."
              retries file
          else inner ~retries:(retries + 1)
    in
    inner ~retries:0

  let symlink ~to_dir ~target ~link_name =
    let rec inner ~retries =
      try Unix.symlink ~to_dir target link_name
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        if retries > 5 then
          Fmt.failwith "Failed %d times to create symlink %s (Unix.EEXIST)"
            retries target
        else (
          unlink_if_exists link_name;
          inner ~retries:(retries + 1))
    in
    inner ~retries:0

  let prepare_log_trap ~root ~uuid ~name =
    let dir = Filename.concat root uuid in
    if not (Sys.file_exists dir) then (
      Unix.mkdir_p dir 0o770;
      if (Sys.unix || Sys.cygwin) && Unix.has_symlink () then (
        let this_exe = Filename.concat root name
        and latest = Filename.concat root "latest" in
        unlink_if_exists this_exe;
        unlink_if_exists latest;
        symlink ~to_dir:true ~target:dir ~link_name:this_exe;
        symlink ~to_dir:true ~target:dir ~link_name:latest))
    else if not (Sys.is_directory dir) then
      Fmt.failwith "exists but is not a directory: %S" dir

  let stdout_isatty () = Unix.(isatty stdout)

  let stdout_columns () =
    if Sys.win32 then None
    else
      match Terminal.get_dimensions () with
      | Some { columns; _ } -> Some columns
      | None -> None

  external before_test :
    output:out_channel -> stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_before_test"

  external after_test : stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_after_test"

  type file_descriptor = out_channel

  let log_trap_supported = true
  let file_exists = Sys.file_exists
  let open_write_only = open_out
  let close = close_out

  let with_redirect fd_file fn =
    let* () = M.return () in
    Fmt.(flush stdout) ();
    Fmt.(flush stderr) ();
    before_test ~output:fd_file ~stdout ~stderr;
    let+ r = try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e in
    Fmt.(flush stdout ());
    Fmt.(flush stderr ());
    after_test ~stdout ~stderr;
    match r with `Ok x -> x | `Error e -> raise e

  let setup_std_outputs = Fmt_tty.setup_std_outputs

  (* Implementation similar to that of [Bos.Os.Dir]. *)
  let home_directory () =
    let env_var_fallback () =
      try Ok (Sys.getenv "HOME")
      with Not_found -> Error (`Msg "HOME environment variable is undefined")
    in
    if Sys.win32 then env_var_fallback ()
    else
      try
        let uid = Unix.getuid () in
        Ok (Unix.getpwuid uid).Unix.pw_dir
      with Not_found -> env_var_fallback ()
end

module V1 = struct
  include Alcotest_engine.V1.Test

  module T =
    Alcotest_engine.V1.Cli.Make (Unix_platform) (Alcotest_engine.Monad.Identity)

  include T
end

include V1
