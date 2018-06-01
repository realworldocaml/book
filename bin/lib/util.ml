open Core
open Async
let (/) = Filename.concat

let find_files dir =
  let is_file x =
    Sys.is_file ~follow_symlinks:false x >>| function
    | `Yes -> true
    | `No | `Unknown -> false
  in
  let is_dir x =
    Sys.is_directory ~follow_symlinks:false x >>| function
    | `Yes -> true
    | `No | `Unknown -> false
  in
  let get_files_dirs dir : (string list * string list) Deferred.t =
    Sys.ls_dir dir >>= fun all ->
    Deferred.List.filter all ~f:(fun x -> is_file (dir/x)) >>= fun files ->
    Deferred.List.filter all ~f:(fun x -> is_dir (dir/x)) >>= fun dirs ->
    return (
      List.map files ~f:(fun x -> dir/x),
      List.map dirs  ~f:(fun x -> dir/x)
    )
  in
  let accum = ref [] in
  let rec loop dir =
    get_files_dirs dir >>= fun (files,dirs) ->
    accum := files::!accum;
    Deferred.List.iter dirs ~f:loop
  in
  loop dir >>| fun () ->
  List.concat !accum

let string_pair_equal (a1,a2) (b1,b2) =
  String.equal a1 b1 && String.equal a2 b2
