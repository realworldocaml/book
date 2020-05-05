(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let unix_buffer_size = 65536                      (* UNIX_BUFFER_SIZE 4.0.0 *)

(* Unix pretty printers *)

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)
let pp_process_status ppf = function
| Unix.WEXITED c -> Fmt.pf ppf "exited with %d" c
| Unix.WSIGNALED s -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal s
| Unix.WSTOPPED s -> Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal s

(* Error messages *)

let err_empty_line = "no command, empty command line"
let err_file f e = R.error_msgf "%a: %a" Fpath.pp f pp_unix_error e
let err_run cmd pp e = R.error_msgf "run %a: %a" Bos_cmd.dump cmd pp e

(* Primitives from Unix *)

let rec waitpid flags pid = try Unix.waitpid flags pid with
| Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let rec create_process prog args stdin stdout stderr =
  try Unix.create_process prog args stdin stdout stderr with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args stdin stdout stderr

let rec create_process_env prog args env stdin stdout stderr =
  try Unix.create_process_env prog args env stdin stdout stderr with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process_env prog args env stdin stdout stderr

let rec pipe () = try Unix.pipe () with
| Unix.Unix_error (Unix.EINTR, _, _) -> pipe ()

let rec set_close_on_exec fd = try Unix.set_close_on_exec fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> set_close_on_exec fd

let rec clear_close_on_exec fd = try Unix.clear_close_on_exec fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> clear_close_on_exec fd

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
| Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let rec close fd = try Unix.close fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> close fd

let close_no_err fd = try close fd with e -> ()

let rec select r w e t = try Unix.select r w e t with
| Unix.Unix_error (Unix.EINTR, _, _) -> select r w e t

(* Process creation primitive. *)

let create_process cmd env ~stdin ~stdout ~stderr =
  let log_header pid = "EXEC:" ^ String.of_int pid in
  let line = Bos_cmd.to_list cmd in
  let prog = try List.hd line with Failure _ -> failwith err_empty_line in
  let line = Array.of_list line in
  match env with
  | None ->
      let pid = create_process prog line stdin stdout stderr in
      Bos_log.debug
        (fun m -> m ~header:(log_header pid) "@[<1>%a@]" Bos_cmd.dump cmd);
      pid
  | Some env ->
      let env = Bos_os_env.to_array env in
      let pid = create_process_env prog line env stdin stdout stderr in
      Bos_log.debug
        (fun m -> m ~header:(log_header pid) "@[<v>%a@,%a@]"
            Fmt.Dump.(array String.dump) env Bos_cmd.dump cmd);
      pid

(* Tool existence and search *)

let default_path_sep = if Sys.win32 then ";" else ":"
let dir_sep = Fpath.dir_sep.[0]
let exe_is_path t = String.exists (Char.equal dir_sep) t

let tool_file ~dir tool = match dir.[String.length dir - 1] with
| c when c = dir_sep -> dir ^ tool
| _ -> String.concat ~sep:Fpath.dir_sep [dir; tool]

let search_in_path tool =
  let rec loop tool = function
  | "" -> None
  | p ->
      let dir, p = match String.cut ~sep:default_path_sep p with
      | None -> p, ""
      | Some (dir, p) -> dir, p
      in
      if dir = "" then loop tool p else
      let tool_file = tool_file ~dir tool in
      match Bos_os_file._is_executable tool_file with
      | false -> loop tool p
      | true -> Some (Fpath.v tool_file)
  in
  try loop tool (Unix.getenv "PATH") with
  | Not_found -> None

let search_in_dirs ~dirs tool =
  let rec loop tool = function
  | [] -> None
  | d :: dirs ->
      let tool_file = tool_file ~dir:(Fpath.to_string d) tool in
      match Bos_os_file._is_executable tool_file with
      | false -> loop tool dirs
      | true -> Some (Fpath.v tool_file)
  in
  loop tool dirs

let ensure_exe_suffix_if_win32 = match Sys.win32 with
| false -> fun t -> t
| true ->
    fun t -> match String.is_suffix ~affix:".exe" t with
    | true -> t
    | false -> t ^ ".exe"

let _find_tool ?search tool = match tool with
| "" -> Ok None
| tool ->
    let tool = ensure_exe_suffix_if_win32 tool in
    match exe_is_path tool with
    | true ->
        begin match Fpath.of_string tool with
        | Ok t -> Ok (Some t)
        | Error (`Msg _) as e -> e
        end
    | false ->
        match search with
        | None -> Ok (search_in_path tool)
        | Some dirs -> Ok (search_in_dirs ~dirs tool)

let find_tool ?search cmd = match Bos_cmd.to_list cmd with
| [] -> Ok None
| c :: _ -> _find_tool ?search c

let err_not_found ?search cmd = match Bos_cmd.is_empty cmd with
| true -> R.error_msg err_empty_line
| false ->
    let pp_search ppf = function
    | None -> Fmt.string ppf "PATH"
    | Some dirs ->
        let pp_dir ppf d = Fmt.string ppf (Filename.quote @@ Fpath.to_string d)
        in
        Fmt.(list ~sep:(Fmt.unit ",@ ") pp_dir) ppf dirs
    in
    let tool = List.hd @@ Bos_cmd.to_list cmd in
    R.error_msgf "%s: no such command in %a" tool pp_search search

let get_tool ?search cmd = match find_tool ?search cmd with
| Ok (Some t) -> Ok t
| Ok None -> err_not_found ?search cmd
| Error _ as e -> e

let exists ?search cmd = match find_tool ?search cmd with
| Ok (Some _) -> Ok true
| Ok None -> Ok false
| Error _ as e -> e

let must_exist ?search cmd = match find_tool ?search cmd with
| Ok (Some _) -> Ok cmd
| Ok None -> err_not_found ?search cmd
| Error _ as e -> e

let resolve ?search cmd = match find_tool ?search cmd with
| Ok (Some t) ->
    let t = Fpath.to_string t in
    Ok (Bos_cmd.of_list (t :: List.tl (Bos_cmd.to_list cmd)))
| Ok None -> err_not_found ?search cmd
| Error _ as e -> e

let search_path_dirs ?(sep = default_path_sep) path =
  let rec loop acc = function
  | ""  -> Ok (List.rev acc)
  | p ->
      let dir, p = match String.cut ~sep p with
      | None -> p, ""
      | Some (dir, p) -> dir, p
      in
      if dir = "" then loop acc p else
      match Fpath.of_string dir with
      | Error (`Msg m) -> R.error_msgf "search path value %S: %s" path m
      | Ok d -> loop (d :: acc) p
  in
  loop [] path

(* Fd utils *)

module Fds = struct

  (* Maintains a set of fds to close, standard fds are never in the set. *)

  module Fd = struct
    type t = Unix.file_descr
    let compare : t -> t -> int = compare
  end
  module S = Set.Make (Fd)

  type t = S.t ref
  let empty () = ref S.empty
  let rem fd s = s := S.remove fd !s
  let add fd s =
    if fd = Unix.stdin || fd = Unix.stdout || fd = Unix.stderr then () else
    (s := S.add fd !s)

  let close_all s = S.iter close_no_err !s; s := S.empty
  let close fd s = if S.mem fd !s then (close_no_err fd; s := S.remove fd !s)
end

let write_fd_for_file ~append f =
  try
    let flags = Unix.([O_WRONLY; O_CREAT]) in
    let flags = (if append then Unix.O_APPEND else Unix.O_TRUNC) :: flags in
    Ok (openfile (Fpath.to_string f) flags 0o644)
  with Unix.Unix_error (e, _, _) -> err_file f e

let read_fd_for_file f =
  try Ok (openfile (Fpath.to_string f) [Unix.O_RDONLY] 0o644)
  with Unix.Unix_error (e, _, _) -> err_file f e

let string_of_fd_async fd =
  let len = unix_buffer_size in
  let buf = Buffer.create len in
  let b = Bytes.create len in
  let rec step fd store b () =
    try match Unix.read fd b 0 len with
    | 0 -> `Ok (Buffer.contents buf)
    | n ->
        (* FIXME After 4.01 Buffer.add_subbytes buf b 0 n; step fd store b () *)
        Buffer.add_substring buf (Bytes.unsafe_to_string b) 0 n;
        step fd store b ()
    with
    | Unix.Unix_error (Unix.EPIPE, _, _) when Sys.win32 ->
        (* That's the Windows way to say end, see
           https://msdn.microsoft.com/en-us/library/windows/\
           desktop/aa365467(v=vs.85).aspx *)
        `Ok (Buffer.contents buf)
    | Unix.Unix_error (Unix.EINTR, _, _) -> step fd buf b ()
    | Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
      `Await (step fd buf b)
  in
  step fd buf b

let string_of_fd fd =
  let rec loop = function `Ok s -> s | `Await step -> loop (step ()) in
  loop (string_of_fd_async fd ())

let string_to_fd_async s fd =
  let rec step fd s first len () =
(* FIXME After 4.01 try match Unix.single_write_substring fd s first len with *)
    let b = Bytes.unsafe_of_string s in
    try match Unix.single_write fd b first len with
    | c when c = len -> `Ok ()
    | c -> step fd s (first + c) (len - c) ()
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> step fd s first len ()
    | Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        `Await (step fd s first len)
  in
  step fd s 0 (String.length s)

let string_to_fd s fd =
  let rec loop = function `Ok () -> () | `Await step -> loop (step ()) in
  loop (string_to_fd_async s fd ())

let string_to_of_fd s ~to_fd ~of_fd =
  let never () = assert false in
  let wset, write = [to_fd], string_to_fd_async s to_fd in
  let rset, read = [of_fd], string_of_fd_async of_fd in
  let ret = ref "" in
  let rec loop rset read wset write =
    let rable, wable, _ = select rset wset [] (-1.) in
    let rset, read = match rable with
    | [] -> rset, read
    | _ ->
        match read () with
        | `Ok s -> ret := s; [], never
        | `Await step -> rset, step
    in
    let wset, write = match wable with
    | [] -> wset, write
    | _ ->
        match write () with
        | `Ok () -> close_no_err to_fd; [], never
        | `Await step -> wset, step
    in
    if rset = [] && wset = [] then !ret else
    loop rset read wset write
  in
  let sigpipe =
    if Sys.win32 then None else
    Some (Sys.signal Sys.sigpipe Sys.Signal_ignore)
  in
  let restore () = match sigpipe with
  | None -> ()
  | Some sigpipe -> Sys.set_signal Sys.sigpipe sigpipe
  in
  try let ret = loop rset read wset write in restore (); ret
  with e -> restore (); raise e

(* Command runs *)

(* Run statuses *)

type status = [ `Exited of int | `Signaled of int ]

type run_info = Bos_cmd.t
let run_info_cmd ri = ri

let pp_status ppf = function
| `Exited c -> Fmt.pf ppf "exited with %d" c
| `Signaled s -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal s

type run_status = run_info * status

let success = function
| Ok (v, (_, `Exited 0)) -> Ok v
| Ok (_, (cmd, s)) -> err_run cmd pp_status s
| Error _ as e -> e

(* Run standard errors *)

type run_err =
| Err_file of Fpath.t * bool
| Err_fd of Unix.file_descr
| Err_run_out
| Err_stderr

let err_file ?(append = false) f = Err_file (f, append)
let err_null = err_file Bos_os_file.null
let err_run_out = Err_run_out
let err_stderr = Err_stderr

let fd_for_run_err out_fd = function
| Err_file (f, append) -> write_fd_for_file ~append f
| Err_fd fd -> Ok fd
| Err_run_out -> Ok out_fd
| Err_stderr -> Ok Unix.stderr

(* Run standard inputs *)

type pipeline =
  { write : (string * Unix.file_descr) option;
    read : Unix.file_descr;
    pids : (Bos_cmd.t * int) list }

type run_in =
| In_string of string
| In_file of Fpath.t
| In_run_out of pipeline
| In_fd of Unix.file_descr

let in_string s = In_string s
let in_file f = In_file f
let in_null = in_file Bos_os_file.null
let in_stdin = In_fd Unix.stdin

(* Run standard outputs *)

type _ _run_out =
| To_string : (string * run_status) _run_out
| To_file : Fpath.t * bool -> (unit * run_status)  _run_out
| To_run_in : run_in _run_out
| To_fd : Unix.file_descr -> (unit * run_status) _run_out

type run_out =
  { env : Bos_os_env.t option;
    cmd : Bos_cmd.t;
    run_err : run_err;
    run_in : run_in; }

(* Waiting for processes *)

let rec wait_pids rev_pids = (* On failure returns the first failure *)
  let rec loop ret = function
  | (cmd, pid) :: pids ->
      let s = snd (waitpid [] pid) in
      if ret <> None then loop ret pids else
      begin match s with
      | Unix.WEXITED 0 -> loop ret pids
      | Unix.WEXITED c -> loop (Some (cmd, `Exited c)) pids
      | Unix.WSIGNALED s -> loop (Some (cmd, `Signaled s)) pids
      | Unix.WSTOPPED _ -> assert false
      end
  | [] ->
      match ret with
      | None -> (fst (List.hd rev_pids), `Exited 0)
      | Some s -> s
  in
  loop None (List.rev rev_pids)

(* Running *)

let do_in_fd_read_stdout stdin o pids do_read =
  let fds = Fds.empty () in
  try
    Fds.add stdin fds;
    let read_stdout, stdout = pipe () in
    Fds.add read_stdout fds;
    Fds.add stdout fds;
    match fd_for_run_err stdout o.run_err with
    | Error _ as e -> Fds.close_all fds; e
    | Ok stderr ->
        Fds.add stderr fds;
        set_close_on_exec read_stdout; (* child close *)
        let pid = create_process o.cmd o.env ~stdin ~stdout ~stderr in
        clear_close_on_exec read_stdout; (* not in further childs (pipes) *)
        Fds.close stdin fds;
        Fds.close stdout fds;
        do_read fds read_stdout ((o.cmd, pid) :: pids)
  with
  | Failure msg -> Error (`Msg msg)
  | Unix.Unix_error (e, _, _) ->
      Fds.close_all fds; err_run o.cmd pp_unix_error e

let do_in_fd_out_string stdin o pids =
  do_in_fd_read_stdout stdin o pids
    begin fun fds read_stdout pids ->
      let res = string_of_fd read_stdout in
      let ret = wait_pids pids in
      Fds.close_all fds;
      Ok (res, ret)
    end

let do_in_fd_out_run_in stdin o pids =
  do_in_fd_read_stdout stdin o pids
    begin fun fds read_stdout pids ->
      Fds.rem read_stdout fds;
      Fds.close_all fds;
      Ok (In_run_out { write = None; read = read_stdout; pids })
    end

let do_in_fd_out_fd stdin stdout o pids =
  let fds = Fds.empty () in
  try
    Fds.add stdin fds;
    Fds.add stdout fds;
    match fd_for_run_err stdout o.run_err with
    | Error _ as e -> Fds.close_all fds; e
    | Ok stderr ->
        Fds.add stderr fds;
        let pid = create_process o.cmd o.env ~stdin ~stdout ~stderr in
        let ret = wait_pids ((o.cmd, pid) :: pids) in
        Fds.close_all fds;
        Ok ((), ret)
  with
  | Failure msg -> Error (`Msg msg)
  | Unix.Unix_error (e, _, _) ->
      Fds.close_all fds; err_run o.cmd pp_unix_error e

let do_in_run_out_string p o = do_in_fd_out_string p.read o p.pids
let do_in_run_out_run_in p o = do_in_fd_out_run_in p.read o p.pids
let do_in_run_out_fd p out_fd o = do_in_fd_out_fd p.read out_fd o p.pids

let do_in_string_read_stdout s o do_read =
  let fds = Fds.empty () in
  try
    let stdin, write_stdin = pipe () in
    Fds.add stdin fds;
    Fds.add write_stdin fds;
    let read_stdout, stdout = pipe () in
    Fds.add read_stdout fds;
    Fds.add stdout fds;
    match fd_for_run_err stdout o.run_err with
    | Error _ as e -> Fds.close_all fds; e
    | Ok stderr ->
        Fds.add stderr fds;
        set_close_on_exec read_stdout; (* child close *)
        set_close_on_exec write_stdin; (* child close *)
        let pid = create_process o.cmd o.env ~stdin ~stdout ~stderr in
        Fds.close stdin fds;
        Fds.close stdout fds;
        do_read fds write_stdin read_stdout pid
  with
  | Failure msg -> Error (`Msg msg)
  | Unix.Unix_error (e, _, _) ->
      Fds.close_all fds; err_run o.cmd pp_unix_error e

let do_in_string_out_string s o =
  do_in_string_read_stdout s o
    begin fun fds write_stdin read_stdout pid ->
      let res = string_to_of_fd s ~to_fd:write_stdin ~of_fd:read_stdout in
      Fds.close write_stdin fds; (* signal EOF *)
      let ret = wait_pids [(o.cmd, pid)] in
      Fds.close_all fds;
      Ok (res, ret)
    end

let do_in_string_out_run_in s o =
  do_in_string_read_stdout s o
    begin fun fds write_stdin read_stdout pid ->
      Fds.rem read_stdout fds;
      Fds.close_all fds;
      Ok (In_run_out { write = Some (s, write_stdin);
                       read = read_stdout; pids = [o.cmd, pid] })
    end

let do_in_string_out_fd s stdout o =
  let fds = Fds.empty () in
  try
    Fds.add stdout fds;
    let stdin, write_stdin = pipe () in
    Fds.add stdin fds;
    Fds.add write_stdin fds;
    match fd_for_run_err stdout o.run_err with
    | Error _ as e -> Fds.close_all fds; e
    | Ok stderr ->
        Fds.add stderr fds;
        set_close_on_exec write_stdin; (* child close *)
        let pid = create_process o.cmd o.env ~stdin ~stdout ~stderr in
        string_to_fd s write_stdin;
        Fds.close write_stdin fds; (* signal EOF *)
        let ret = wait_pids [(o.cmd, pid)] in
        Fds.close_all fds;
        Ok ((), ret)
  with
  | Failure msg -> Error (`Msg msg)
  | Unix.Unix_error (e, _, _) ->
      Fds.close_all fds; err_run o.cmd pp_unix_error e

let do_in_fd :
  type a. Unix.file_descr -> run_out -> a _run_out -> (a, [> R.msg]) result =
fun in_fd o ret -> match ret with
| To_string -> do_in_fd_out_string in_fd o []
| To_run_in -> do_in_fd_out_run_in in_fd o []
| To_fd out_fd -> do_in_fd_out_fd in_fd out_fd o []
| To_file (f, append) ->
    write_fd_for_file ~append f >>= fun fd -> do_in_fd_out_fd in_fd fd o []

let run_cmd : type a. run_out -> a _run_out -> (a, [> R.msg]) result =
fun o ret -> match o.run_in with
| In_string s ->
    begin match ret with
    | To_string -> do_in_string_out_string s o
    | To_run_in -> do_in_string_out_run_in s o
    | To_fd out_fd -> do_in_string_out_fd s out_fd o
    | To_file (f, append) ->
        write_fd_for_file ~append f >>= fun fd -> do_in_string_out_fd s fd o
    end
| In_run_out p ->
    begin match ret with
    | To_string -> do_in_run_out_string p o
    | To_run_in -> do_in_run_out_run_in p o
    | To_fd out_fd -> do_in_run_out_fd p out_fd o
    | To_file (f, append) ->
        write_fd_for_file ~append f >>= fun fd -> do_in_run_out_fd p fd o
    end
| In_fd fd -> do_in_fd fd o ret
| In_file f -> read_fd_for_file f >>= fun fd -> do_in_fd fd o ret

let out_string ?(trim = true) o = match run_cmd o To_string with
| Ok (s, st) when trim -> Ok (String.trim s, st)
| r -> r

let out_lines ?trim o =
  out_string ?trim o  >>= fun (s, st) ->
  Ok ((if s = "" then [] else String.cuts ~sep:"\n" s), st)

let out_file ?(append = false) f o = run_cmd o (To_file (f, append))
let out_run_in o = run_cmd o To_run_in
let out_null o = out_file Bos_os_file.null o
let out_stdout o = run_cmd o (To_fd Unix.stdout)

let to_string ?trim o = out_string ?trim o |> success
let to_lines ?trim o = out_lines ?trim o |> success
let to_file ?append f o = out_file ?append f o |> success
let to_null o = out_null o |> success
let to_stdout o = out_stdout o |> success

let run_io ?env ?err:(run_err = Err_stderr) cmd run_in =
  { env; cmd; run_err; run_in }

let run_out ?env ?err cmd = run_io ?env ?err cmd in_stdin
let run_in ?env ?err cmd i = run_io ?env ?err cmd i |> to_stdout
let run ?env ?err cmd = run_io  ?env ?err cmd in_stdin |> to_stdout
let run_status ?env ?err ?(quiet = false) cmd =
  let err = match err with
  | None -> if quiet then err_null else err_stderr
  | Some err -> err
  in
  let ret = match quiet with
  | true -> in_null |> run_io ?env ~err cmd |> out_null
  | false -> in_stdin |> run_io ?env ~err cmd |> out_stdout
  in
  match ret with
  | Ok ((), (_, status)) -> Ok status
  | Error _ as e -> e

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
