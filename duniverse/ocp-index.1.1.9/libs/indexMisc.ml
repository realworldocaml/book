(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)


let debug_enabled =
  try match Sys.getenv "OCP_DEBUG" with "" | "0" -> false | _ -> true
  with Not_found -> false

let debug =
  if debug_enabled then
    fun fmt -> Printf.eprintf ("\027[36m"^^fmt^^"\027[m%!")
  else
    fun fmt -> Printf.ifprintf stderr fmt

let timer () =
  if debug_enabled then
    let t = Sys.time () in
    fun () -> Sys.time () -. t
  else
    fun () -> 0.

let rec foldl_next f acc = function
  | [] -> acc
  | [x] -> f acc x None
  | x1::(x2::_ as tl) -> foldl_next f (f acc x1 (Some x2)) tl

type key = char list

(* Used as path separator in keys *)
let dot = char_of_int 0
let dots = String.make 1 dot

let string_to_key s =
  let rec aux acc i =
    if i >= 0 then
      let c = match s.[i] with
        | '.' | '#' as c when i > 0 ->
            (match s.[i-1] with 'a'..'z' | 'A'..'Z' | '0'..'9' -> dot
                              | _ -> c)
        | c -> c
      in
      aux (c::acc) (i - 1)
    else acc
  in
  aux [] (String.length s - 1)

let key_to_string l =
  let rec aux n = function
    | [] -> Bytes.create n
    | c::r ->
        let s = aux (n+1) r in
  Bytes.set s n
    (if c = dot then '.' else c);
        s
  in
  let s = aux 0 l in
    Bytes.to_string s


let modpath_to_key ?(enddot=true) path =
  List.fold_right (fun p acc ->
      let acc = if acc <> [] || enddot then dot::acc else acc in
      string_to_key p @ acc) path []

let key_to_modpath l =
  let rec aux acc1 acc2 = function
    | '\000'::r -> aux [] (acc1::acc2) r
    | c::r -> aux (c::acc1) acc2 r
    | [] -> if acc1 = [] then acc2 else acc1::acc2
   in
   List.rev_map (fun l -> key_to_string (List.rev l)) (aux [] [] l)

let modpath_to_string path = String.concat "." path

let parent_type id =
  match id.IndexTypes.kind with
  | Field parent | Variant parent | Method parent -> Some parent
  | Type | Value | Exception | Module | ModuleType | Class
  | OpenType
  | ClassType | Keyword -> None


let unique_subdirs ?(skip = fun _ -> false) dir_list =
  let rec subdirs acc path =
    Array.fold_left
      (fun acc p ->
        if skip p then acc else
        let path = Filename.concat path p in
        if try Sys.is_directory path with Sys_error _ -> false
        then subdirs acc path else acc)
      (path::acc)
      (Sys.readdir path)
  in
  let remove_dups l =
    let rec aux = function
      | a::(b::_ as r) when a = b -> aux r
      | a::r -> a :: aux r
      | [] -> []
    in
    aux (List.sort compare l)
  in
  remove_dups (List.fold_left subdirs [] dir_list)


(* - Project root finding - *)

let build_roots = (* by increasing order of priority *)
  [ "_darcs"; ".hg"; ".git";
    "jengaroot.ml"; "omakeroot"; "_build"; "_obuild" ]

let find_build_dir path =
  let ( / ) = Filename.concat in
  let files = Sys.readdir path in
  let _, root =
    let rec memsuffix x = function
      | a::r -> if x = a then Some r else memsuffix x r
      | [] -> None
    in
    Array.fold_left (fun (roots,found) f ->
        match memsuffix f roots with
        | None -> roots, found
        | Some roots -> roots, Some f)
      (build_roots, None) files
  in
  match root with
  | None -> None
  | Some ("_obuild" | "_build" as dir) -> Some (path / dir)
  | Some _ -> Some path

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let make_relative ?(path=Sys.getcwd()) file =
  if Filename.is_relative file then file
  else
  let rec aux cwd file = match cwd, file with
    | d1::cwd, d2::file when d1 = d2 -> aux cwd file
    | cwd, file ->
        List.fold_left
          (fun file _ -> Filename.parent_dir_name :: file)
          file cwd
  in
  let sep = Filename.dir_sep.[0] in
  let d =
    aux (string_split sep path) (string_split sep file)
  in
  if d = [] then Filename.current_dir_name
  else String.concat Filename.dir_sep d

let project_root ?(path=Sys.getcwd()) () =
  let ( / ) = Filename.concat in
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let path =
    if Filename.is_relative path then Sys.getcwd () / path
    else path
  in
  let rec find path =
    if path = home then None
    else match find_build_dir path with
    | None ->
        let parent = Filename.dirname path in
        if path = parent then None
        else find parent
    | Some build -> Some (path, build)
  in
  match find path with
  | None -> None, None
  | Some (root, build) -> Some root, Some build
