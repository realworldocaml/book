(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



type point_definition = {
    offset : int;
    identifier : int;
  }

(* Utility functions *)

let try_finally x f h =
  let res =
    try
      f x
    with e ->
      (try h x with _ -> ());
      raise e in
  (try h x with _ -> ());
  res

let try_in_channel bin x f =
  let open_ch = if bin then open_in_bin else open_in in
  try_finally (open_ch x) f (close_in_noerr)

let try_out_channel bin x f =
  let open_ch = if bin then open_out_bin else open_out in
  try_finally (open_ch x) f (close_out_noerr)


(* I/O functions *)

(* filename + reason *)
exception Invalid_file of string * string

exception Unsupported_version of string

exception Modified_file of string

let magic_number_rtd = Bytes.of_string "BISECT-RTD"

let supported_versions = [
  2, 0
]

let format_version = (2, 0)

let write_channel channel magic write_digest x =
  output_bytes channel magic;
  output_value channel format_version;
  (match write_digest with
  | Some file -> output_value channel (Digest.file file)
  | None -> ());
  output_value channel x

let check_channel channel filename magic check_digest =
  let magic_length = Bytes.length magic in
  let file_magic = Bytes.create magic_length in
  begin
    try really_input channel file_magic 0 magic_length;
    with End_of_file ->
      raise
        (Invalid_file
          (filename, "unexpected end of file while reading magic number"))
  end;
  let file_version =
    if file_magic = magic then
      let file_version : (int * int) = input_value channel in
      if not (List.mem file_version supported_versions) then
        raise (Unsupported_version filename)
      else
        file_version
    else
      raise (Invalid_file (filename, "bad magic number")) in
  (match check_digest with
  | Some file ->
      let file_digest : string = input_value channel in
      let digest = Digest.file file in
      if file_digest <> digest then raise (Modified_file filename)
  | None -> ());
  file_version

let write_runtime_data channel content =
  write_channel channel magic_number_rtd None (Array.of_list content)

let write_points points =
  let points_array = Array.of_list points in
  Array.sort compare points_array;
  Marshal.to_string points_array []

let read_runtime_data' filename =
  try_in_channel
    true
    filename
    (fun channel ->
      let version = check_channel channel filename magic_number_rtd None in
      match version with
      | 2, 0 ->
        let file_content : (string * (int array * string)) array =
          try input_value channel
          with e ->
            raise
              (Invalid_file
                (filename, "exception reading data: " ^ Printexc.to_string e))
        in
        Array.to_list file_content
      | _ -> assert false)

let read_points' s =
  let points_array : point_definition array = Marshal.from_string s 0 in
  Array.sort compare points_array;
  Array.to_list points_array

(* Simulate the old behavior for current ocveralls. This is quite fragile,
   because it depends on two things:
   - read_points is only called after all .out files are read with
     read_runtime_data.
   - There are no duplicate source file names anywhere in the project. This is
     necessary because read_runtime_data finds unprefixed source file names,
     while read_points receives file names with the -I option already
     applied. *)
let points : (string, point_definition list) Hashtbl.t = Hashtbl.create 17

let read_runtime_data filename =
  let data = read_runtime_data' filename in
  data |> List.map (fun (source_file, (counts, file_points)) ->
    let basename = Filename.basename source_file in
    Hashtbl.replace points basename (read_points' file_points);
    source_file, counts)

let read_points filename =
  Hashtbl.find points (Filename.basename filename)
