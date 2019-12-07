open! Import
open! Std_internal

type t = string [@@deriving bin_io, compare, sexp]

include struct
  open Caml.Filename

  let check_suffix = check_suffix
  let chop_extension = chop_extension
  let chop_suffix = chop_suffix
  let current_dir_name = current_dir_name
  let is_implicit = is_implicit
  let is_relative = is_relative
  let parent_dir_name = parent_dir_name
  let dir_sep = dir_sep
  let quote = quote
  let temp_dir_name = get_temp_dir_name ()
end

let is_absolute p = not (is_relative p)

let concat p1 p2 =
  if String.is_empty p1
  then
    failwithf
      "Filename.concat called with an empty string as its first argument (second \
       argument: %s)"
      p2
      ();
  let rec collapse_trailing s =
    match String.rsplit2 s ~on:'/' with
    | Some ("", ("." | "")) -> ""
    | Some (s, ("." | "")) -> collapse_trailing s
    | None | Some _ -> s
  in
  let rec collapse_leading s =
    match String.lsplit2 s ~on:'/' with
    | Some (("." | ""), s) -> collapse_leading s
    | Some _ | None -> s
  in
  collapse_trailing p1 ^ "/" ^ collapse_leading p2
;;

(* Finds the largest index i in [s] that is less than [from] and for which
   [f s.[i]]
   returns true. Then it returns [i+1]. Raises an exception if [from] isn't a
   valid index
   in [s]. *)
let string_rexists s ~f ~from:n =
  let rec loop n =
    if n = 0 then None else if f s.[n - 1] then Some n else loop (n - 1)
  in
  loop n
;;

let skip_end_slashes s ~from =
  match string_rexists s ~from ~f:(fun c -> c <> '/') with
  | Some v -> `Ends_at v
  | None -> `All_slashes
;;

(*
   Fix for #0004549. (in the inria bug tracker)
*)
let split = function
  | "" -> ".", "."
  | s ->
    (match skip_end_slashes s ~from:(String.length s) with
     | `All_slashes -> "/", "/"
     | `Ends_at basename_end ->
       (match string_rexists s ~f:(fun c -> c = '/') ~from:basename_end with
        | None -> ".", String.sub ~pos:0 ~len:basename_end s
        | Some basename_start ->
          let basename =
            String.sub s ~pos:basename_start ~len:(basename_end - basename_start)
          in
          let dirname =
            match skip_end_slashes s ~from:basename_start with
            | `All_slashes -> "/"
            | `Ends_at dirname_end -> String.sub ~pos:0 ~len:dirname_end s
          in
          dirname, basename))
;;

(*
   http://www.opengroup.org/onlinepubs/9699919799/utilities/basename.html
   http://www.opengroup.org/onlinepubs/9699919799/utilities/dirname.html
*)
let basename path = snd (split path)
let dirname path = fst (split path)

(* [max_pathname_component_size] comes from getconf _POSIX_NAME_MAX / *)
let max_pathname_component_size = 255

let is_posix_pathname_component s =
  let module S = String in
  s <> "."
  && s <> ".."
  && 0 < S.length s
  && S.length s <= max_pathname_component_size
  && not (S.contains s '/')
  && not (S.contains s '\000')
;;

let root = "/"

let split_extension fn =
  let dir, fn =
    match String.rsplit2 ~on:'/' fn with
    | None -> None, fn
    | Some (path, fn) -> Some path, fn
  in
  let fn, ext =
    match String.rsplit2 ~on:'.' fn with
    | None -> fn, None
    | Some (base_fn, ext) -> base_fn, Some ext
  in
  let fn =
    match dir with
    | None -> fn
    | Some dir -> dir ^ "/" ^ fn
  in
  fn, ext
;;

let parts filename =
  let rec loop acc filename =
    match split filename with
    | ("." as base), "." -> base :: acc
    | ("/" as base), "/" -> base :: acc
    | rest, dir -> loop (dir :: acc) rest
  in
  loop [] filename
;;

let of_parts = function
  | [] -> failwith "Filename.of_parts: empty parts list"
  | root :: rest -> List.fold rest ~init:root ~f:Caml.Filename.concat
;;
