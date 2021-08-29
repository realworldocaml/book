(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let uerror = Unix.error_message

(* Existence, creation, deletion, contents *)

let exists = Bos_os_path.dir_exists
let must_exist = Bos_os_path.dir_must_exist
let delete = Bos_os_path.delete_dir

let create ?(path = true) ?(mode = 0o755) dir =
  let rec mkdir d mode = try Ok (Unix.mkdir (Fpath.to_string d) mode) with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
      if d = dir
      then R.error_msgf "create directory %a: %s" Fpath.pp d (uerror e)
      else R.error_msgf "create directory %a: %a: %s"
             Fpath.pp dir Fpath.pp d (uerror e)
  in
  exists dir >>= function
  | true -> Ok false
  | false ->
      match path with
      | false -> mkdir dir mode >>= fun () -> Ok false
      | true ->
          let rec dirs_to_create p acc = exists p >>= function
          | true -> Ok acc
          | false -> dirs_to_create (Fpath.parent p) (p :: acc)
          in
          let rec create_them dirs () = match dirs with
          | dir :: dirs -> mkdir dir mode >>= create_them dirs
          | [] -> Ok ()
          in
          dirs_to_create dir []
          >>= fun dirs -> create_them dirs ()
          >>= fun () -> Ok true

let rec contents ?(dotfiles = false) ?(rel = false) dir =
  let rec readdir dh acc =
    match (try Some (Unix.readdir dh) with End_of_file -> None) with
    | None -> Ok acc
    | Some (".." | ".") -> readdir dh acc
    | Some f when dotfiles || not (String.is_prefix "." f) ->
        begin match Fpath.of_string f with
        | Ok f ->
            readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
        | Error (`Msg m) ->
            R.error_msgf
              "directory contents %a: cannot parse element to a path (%a)"
              Fpath.pp dir String.dump f
        end
    | Some _ -> readdir dh acc
  in
  try
    let dh = Unix.opendir (Fpath.to_string dir) in
    Bos_base.apply (readdir dh) [] ~finally:Unix.closedir dh
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> contents ~rel dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "directory contents %a: %s" Fpath.pp dir (uerror e)

let fold_contents ?err ?dotfiles ?elements ?traverse f acc d =
  contents d >>= Bos_os_path.fold ?err ?dotfiles ?elements ?traverse f acc

(* User and current working directory *)

let user () =
  let debug err = Bos_log.debug (fun m -> m "OS.Dir.user: %s" err) in
  let env_var_fallback () =
    Bos_os_env.(parse "HOME" (some path) ~absent:None) >>= function
    | Some p -> Ok p
    | None -> R.error_msgf "cannot determine user home directory: \
                            HOME environment variable is undefined"
  in
  if Sys.os_type = "Win32" then env_var_fallback () else
  try
    let uid = Unix.getuid () in
    let home = (Unix.getpwuid uid).Unix.pw_dir in
    match Fpath.of_string home with
    | Ok p -> Ok p
    | Error _ ->
        debug (strf "could not parse path (%a) from passwd entry"
                 String.dump home);
        env_var_fallback ()
  with
  | Unix.Unix_error (e, _, _) -> (* should not happen *)
      debug (uerror e); env_var_fallback ()
  | Not_found ->
      env_var_fallback ()

let rec current () =
  try
    let p = Unix.getcwd () in
    match Fpath.of_string p with
    | Ok dir ->
        if Fpath.is_abs dir then Ok dir else
        R.error_msgf "getcwd(3) returned a relative path: (%a)" Fpath.pp dir
    | Error _ ->
        R.error_msgf
          "get current working directory: cannot parse it to a path (%a)"
          String.dump p
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "get current working directory: %s" (uerror e)

let rec set_current dir = try Ok (Unix.chdir (Fpath.to_string dir)) with
| Unix.Unix_error (Unix.EINTR, _, _) -> set_current dir
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "set current working directory to %a: %s"
      Fpath.pp dir (uerror e)

let with_current dir f v =
  current () >>= fun old ->
  try
    set_current dir >>= fun () ->
    let ret = f v in
    set_current old >>= fun () -> Ok ret
  with
  | exn -> ignore (set_current old); raise exn

(* Temporary directories *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let delete_tmp dir = ignore (delete ~recurse:true dir)
let tmps = ref Fpath.Set.empty
let tmps_add file = tmps := Fpath.Set.add file !tmps
let tmps_rem file = delete_tmp file; tmps := Fpath.Set.remove file !tmps
let delete_tmps () = Fpath.Set.iter delete_tmp !tmps
let () = at_exit delete_tmps

let default_tmp_mode = 0o700

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
  let err () =
    R.error_msgf "create temporary directory %s in %a: \
                  too many failing attempts"
      (strf pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let dir = Bos_os_tmp.rand_path dir pat in
    try Ok (Unix.mkdir (Fpath.to_string dir) mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "create temporary directory %s in %a: %s"
          (strf pat "XXXXXX") Fpath.pp dir (uerror e)
  in
  match loop 10000 with
  | Ok dir as r -> tmps_add dir; r
  | Error _ as e -> e

let with_tmp ?mode ?dir pat f v =
  tmp ?mode ?dir pat >>= fun dir ->
  try
    let ret = f dir v in
    tmps_rem dir;
    Ok ret
  with e -> tmps_rem dir; raise e

(* Default temporary directory *)

let default_tmp = Bos_os_tmp.default_dir
let set_default_tmp = Bos_os_tmp.set_default_dir

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
