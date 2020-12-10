(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open StdLabels
open Or_error

type directory = Fpath.t
type file = Fpath.t

module File = struct
  type t = file

  let dirname = Fpath.parent
  let basename = Fpath.base

  let set_ext e p = Fpath.set_ext e p
  let has_ext e p = Fpath.has_ext e p

  let create ~directory ~name =
    match Fpath.of_string name with
    | Result.Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.create: " ^ e)
    | Result.Ok psuf -> Fpath.(normalize @@ directory // psuf)

  let to_string = Fpath.to_string
  let of_string s =
    match Fpath.of_string s with
    | Result.Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.of_string: " ^ e)
    | Result.Ok p -> p

  let read file =
    let with_ic ~close ic f =
      let close ic = try close ic with Sys_error _ -> () in
      match f ic with v -> close ic; v | exception e -> close ic; raise e
    in
    let input_one_shot len ic =
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      Result.Ok (Bytes.unsafe_to_string buf)
    in
    let input_stream file ic =
      let bsize = 65536 (* IO_BUFFER_SIZE *) in
      let buf = Buffer.create bsize in
      let rec loop () = match Buffer.add_channel buf ic bsize with
      | () -> loop ()
      | exception End_of_file -> Result.Ok (Buffer.contents buf)
      | exception Failure _  ->
          Result.Error (`Msg (Printf.sprintf "%s: input too large" file))
      in
      loop ()
    in
    try
      let file = Fpath.to_string file in
      let is_dash = file = "-" in
      let ic = if is_dash then stdin else open_in_bin file in
      let close ic = if is_dash then () else close_in ic in
      with_ic ~close ic @@ fun ic ->
      match in_channel_length ic with
      | 0 (* e.g. stdin or /dev/stdin *) -> input_stream file ic
      | len when len <= Sys.max_string_length -> input_one_shot len ic
      | len ->
          let err = Printf.sprintf "%s: file too large (%d bytes)" file len in
          Result.Error (`Msg err)
    with
    | Sys_error e -> Result.Error (`Msg e)


  module Table = Hashtbl.Make(struct
      type nonrec t = t
      let equal = Fpath.equal
      let hash = Hashtbl.hash
    end)
end

module Directory = struct
  type t = directory

  let dirname = Fpath.parent
  let basename = Fpath.base

  let append = Fpath.append

  let make_path p name =
    match Fpath.of_string name with
    | Result.Error _ as e -> e
    | Result.Ok psuf ->
        Result.Ok (Fpath.(normalize @@ to_dir_path @@ p // psuf))

  let reach_from ~dir path =
    match make_path dir path with
    | Result.Error (`Msg e) -> invalid_arg ("Odoc.Fs.Directory.create: " ^ e)
    | Result.Ok path ->
      let pstr = Fpath.to_string path in
      if Sys.file_exists pstr && not (Sys.is_directory pstr) then
        invalid_arg "Odoc.Fs.Directory.create: not a directory";
      path

  let mkdir_p dir =
    let mkdir d =
      try Unix.mkdir (Fpath.to_string d) 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
      | exn -> raise exn
    in
    let rec dirs_to_create p acc =
      if Sys.file_exists (Fpath.to_string p) then acc else
        dirs_to_create (Fpath.parent p) (p :: acc)
    in
    List.iter (dirs_to_create dir []) ~f:mkdir


  let to_string = Fpath.to_string
  let of_string s =
    match Fpath.of_string s with
    | Result.Error (`Msg e) -> invalid_arg ("Odoc.Fs.Directory.of_string: " ^ e)
    | Result.Ok p -> Fpath.to_dir_path p

  let fold_files_rec ?(ext = "") f acc d =
    let fold_non_dirs ext f acc files =
      let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
      let has_ext ext file = Filename.check_suffix file ext in
      let dirs, files = List.partition ~f:is_dir files in
      let files = List.find_all ~f:(has_ext ext) files in
      let f acc fn = f acc (Fpath.v fn) in
      List.fold_left ~f ~init:acc files, dirs
    in
    let rec loop ext f acc = function
    | (d :: ds) :: up ->
        let rdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> [] in
        let files = List.rev (List.rev_map ~f:(Filename.concat d) (rdir d)) in
        let acc, dirs = fold_non_dirs ext f acc files in
        loop ext f acc (dirs :: ds :: up)
    | [] :: up -> loop ext f acc up
    | [] -> acc
    in
    loop ext f acc ([Fpath.to_string d] :: []);;

  exception Stop_iter of msg

  let fold_files_rec_result ?ext f acc d =
    let f acc fn =
      match f acc fn with
      | Ok acc -> acc
      | Error e -> raise (Stop_iter e)
    in
    try Ok (fold_files_rec ?ext f acc d)
    with Stop_iter (`Msg _ as e) -> Error e

  module Table = Hashtbl.Make(struct
      type nonrec t = t
      let equal = Fpath.equal
      let hash = Hashtbl.hash
    end)
end
