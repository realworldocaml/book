(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Error messages *)

let err_empty_buf = "buffer size can't be 0"
let err_invalid_input = "input no longer valid, did it escape its scope ?"
let err_invalid_output = "output no longer valid, did it escape its scope ?"
let uerror = Unix.error_message

(* Famous file paths *)

let null = Fpath.v (if Sys.os_type = "Win32" then "NUL" else "/dev/null")
let dash = Fpath.v "-"
let is_dash = Fpath.equal dash

(* Existence and deletion *)

let exists = Bos_os_path.file_exists
let must_exist = Bos_os_path.file_must_exist
let delete = Bos_os_path.delete_file

let rec truncate p size =
  try Ok (Unix.truncate (Fpath.to_string p) size) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "truncate file %a: %s" Fpath.pp p (uerror e)

(* Executability *)

let _is_executable file = try Unix.access file [Unix.X_OK]; true with
| Unix.Unix_error _ -> false

let is_executable file = _is_executable (Fpath.to_string file)

(* Bytes buffers *)

let io_buffer_size = 65536                          (* IO_BUFFER_SIZE 4.0.0 *)
let bytes_buf = function
| None -> Bytes.create io_buffer_size
| Some bytes ->
    if Bytes.length bytes <> 0 then bytes else
    invalid_arg err_empty_buf

(* Input *)

type input = unit -> (Bytes.t * int * int) option

let with_input ?bytes file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin (Fpath.to_string file)
    in
    let ic_valid = ref true in
    let close ic =
      ic_valid := false; if is_dash file then () else close_in ic
    in
    let b = bytes_buf bytes in
    let bsize = Bytes.length b in
    let input () =
      if not !ic_valid then invalid_arg err_invalid_input else
      let rc = input ic b 0 bsize in
      if rc = 0 then None else Some (b, 0, rc)
    in
    try Ok (Bos_base.apply (f input) v ~finally:close ic) with
    | Sys_error e -> R.error_msgf "%a: %s" Fpath.pp file e
  with
  | Sys_error e -> R.error_msg e

let with_ic file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin (Fpath.to_string file)
    in
    let close ic = if is_dash file then () else close_in ic in
    try Ok (Bos_base.apply (f ic) v ~finally:close ic) with
    | Sys_error e -> R.error_msgf "%a: %s" Fpath.pp file e
  with
  | End_of_file -> R.error_msgf "%a: unexpected end of file" Fpath.pp file
  | Sys_error e -> R.error_msg e

let read file =
  let input_stdin () =
    let bsize = 65536 (* IO_BUFFER_SIZE *) in
    let buf = Buffer.create bsize in
    let b = Bytes.create bsize in
    let rec loop () =
      let rc = input stdin b 0 bsize in
      if rc = 0 then Ok (Buffer.contents buf) else
(* FIXME After 4.01  (Buffer.add_subbytes buf b 0 rc; loop ()) *)
      (Buffer.add_substring buf (Bytes.unsafe_to_string b) 0 rc; loop ())
    in
    loop ()
  in
  let input ic () =
    if ic == stdin then input_stdin () else
    let len = in_channel_length ic in
    if len <= Sys.max_string_length then begin
      let s = Bytes.create len in
      really_input ic s 0 len;
      Ok (Bytes.unsafe_to_string s)
    end else begin
      R.error_msgf "read %a: file too large (%a, max supported size: %a)"
        Fpath.pp file Fmt.byte_size len Fmt.byte_size Sys.max_string_length
    end
  in
  match with_ic file input () with
  | Ok (Ok _ as v) -> v
  | Ok (Error _ as e) -> e
  | Error _ as e -> e

let fold_lines f acc file =
  let input ic acc =
    let rec loop acc =
      match try Some (input_line ic) with End_of_file -> None with
      | None -> acc
      | Some line -> loop (f acc line)
    in
    loop acc
  in
  with_ic file input acc

let read_lines file = fold_lines (fun acc l -> l :: acc) [] file >>| List.rev

(* Temporary files *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let rec unlink_tmp file = try Unix.unlink (Fpath.to_string file) with
| Unix.Unix_error (Unix.EINTR, _, _) -> unlink_tmp file
| Unix.Unix_error (e, _, _) -> ()

let tmps = ref Fpath.Set.empty
let tmps_add file = tmps := Fpath.Set.add file !tmps
let tmps_rem file = unlink_tmp file; tmps := Fpath.Set.remove file !tmps
let unlink_tmps () = Fpath.Set.iter unlink_tmp !tmps

let () = at_exit unlink_tmps

let create_tmp_path mode dir pat =
  let err () =
    R.error_msgf "create temporary file %s in %a: too many failing attempts"
      (strf pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let file = Bos_os_tmp.rand_path dir pat in
    let sfile = Fpath.to_string file in
    let open_flags = Unix.([O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE]) in
    try Ok (file, Unix.(openfile sfile open_flags mode)) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "create temporary file %a: %s" Fpath.pp file (uerror e)
  in
  loop 10000

let default_tmp_mode = 0o600

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
  create_tmp_path mode dir pat >>= fun (file, fd) ->
  let rec close fd = try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd
  | Unix.Unix_error (e, _, _) -> ()
  in
  close fd; tmps_add file; Ok file

let with_tmp_oc ?(mode = default_tmp_mode) ?dir pat f v =
  try
    let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    let oc = Unix.out_channel_of_descr fd in
    let delete_close oc = tmps_rem file; close_out oc in
    tmps_add file;
    try Ok (Bos_base.apply (f file oc) v ~finally:delete_close oc) with
    | Sys_error e -> R.error_msgf "%a: %s" Fpath.pp file e
  with Sys_error e -> R.error_msg e

let with_tmp_output ?(mode = default_tmp_mode) ?dir pat f v =
  try
    let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    let oc = Unix.out_channel_of_descr fd in
    let oc_valid = ref true in
    let delete_close oc = oc_valid := false; tmps_rem file; close_out oc in
    let output b =
      if not !oc_valid then invalid_arg err_invalid_output else
      match b with
      | Some (b, pos, len) -> output oc b pos len
      | None -> flush oc
    in
    tmps_add file;
    try Ok (Bos_base.apply (f file output) v ~finally:delete_close oc) with
    | Sys_error e -> R.error_msgf "%a: %s" Fpath.pp file e
  with Sys_error e -> R.error_msg e

(* Output *)

type output = (Bytes.t * int * int) option -> unit

let default_mode = 0o644

let rec rename src dst =
  try Unix.rename (Fpath.to_string src) (Fpath.to_string dst); Ok () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> rename src dst
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "rename %a to %a: %s"
        Fpath.pp src Fpath.pp dst (uerror e)

let stdout_with_output f v =
  try
    let output_valid = ref true in
    let close () = output_valid := false in
    let output b =
      if not !output_valid then invalid_arg err_invalid_output else
      match b with
      | Some (b, pos, len) -> output stdout b pos len
      | None -> flush stdout
    in
    Ok (Bos_base.apply (f output) v ~finally:close ())
  with Sys_error e -> R.error_msg e

let with_output ?(mode = default_mode) file f v =
  if is_dash file then stdout_with_output f v else
  let do_write tmp tmp_out v = match f tmp_out v with
  | Error _ as v -> Ok v
  | Ok _ as v ->
      match rename tmp file with
      | Error _ as e -> e
      | Ok () -> Ok v
  in
  match with_tmp_output ~mode ~dir:(Fpath.parent file) "bos-%s.tmp" do_write v
  with
  | Ok (Ok _ as r) -> r
  | Ok (Error _ as e) -> e
  | Error _ as e -> e

let with_oc ?(mode = default_mode) file f v =
  if is_dash file
  then Ok (Bos_base.apply (f stdout) v ~finally:(fun () -> ()) ())
  else
  let do_write tmp tmp_oc v = match f tmp_oc v with
  | Error _ as v -> Ok v
  | Ok _ as v ->
      match rename tmp file with
      | Error _ as e -> e
      | Ok () -> Ok v
  in
  match with_tmp_oc ~mode ~dir:(Fpath.parent file) "bos-%s.tmp" do_write v with
  | Ok (Ok _ as r) -> r
  | Ok (Error _ as e) -> e
  | Error _ as e -> e

let write ?mode file contents =
  let write oc contents = output_string oc contents; Ok () in
  R.join @@ with_oc ?mode file write contents

let writef ?mode file fmt = (* FIXME avoid the kstrf  *)
  Fmt.kstrf (fun content -> write ?mode file content) fmt

let write_lines ?mode file lines =
  let rec write oc = function
  | [] -> Ok ()
  | l :: ls ->
      output_string oc l;
      if ls <> [] then (output_char oc '\n'; write oc ls) else Ok ()
  in
  R.join @@ with_oc ?mode file write lines

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
