(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Fts_types
open Fts_if

let usage = "fts_cmd path [ path .. ]"

let sort_by_name lp rp =
  let open Ctypes in
  let open FTSENT in
  String.compare (name !@lp) (name !@rp)

let ents ?compar path_argv =
  let fts : FTS.t = fts_open ~path_argv ?compar ~options:[] in
  Stream.from (fun _ -> fts_read fts)

let main paths =
  let indent = ref 0 in
  let show_path ent =
    Printf.printf "%*s%s\n" !indent "" (FTSENT.path ent);
  in
  Stream.iter
    FTSENT.(fun ent ->
      match info ent with
        | FTS_D -> begin
          show_path ent;
          incr indent
        end
        | FTS_F
        | FTS_SL
        | FTS_SLNONE -> show_path ent
        | FTS_DP -> decr indent
        | _ -> ())
    (ents ~compar:sort_by_name paths)

let () = 
  match List.tl (Array.to_list Sys.argv) with
    | [] -> prerr_endline usage
    | l  -> main l
