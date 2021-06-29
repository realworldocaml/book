module Stable = struct
  module V1 = struct
    include (
      String :
      sig
        type t = string [@@deriving bin_io, compare, hash, sexp]

        include
          Comparable.Stable.V1.S
          with type comparable := t
          with type comparator_witness = String.Stable.V1.comparator_witness

        val comparator : (t, comparator_witness) Comparator.t

        include Hashable.Stable.V1.S with type key := t
      end)
  end
end

open! Import
open! Std_internal

include (
  String :
  sig
    type t = string [@@deriving bin_io, compare, sexp]

    include
      Comparable.S
      with type t := t
      with type comparator_witness = String.comparator_witness

    val comparator : (t, comparator_witness) Comparator.t

    include Hashable.S with type t := t
  end)

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
  let dirname = dirname
  let basename = basename
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

let to_absolute_exn p ~relative_to =
  if is_relative relative_to
  then
    failwithf
      "Filename.to_absolute_exn called with a [relative_to] that is a relative path: %s"
      relative_to
      ()
  else if is_absolute p
  then p
  else concat relative_to p
;;

let split s = dirname s, basename s

(* [max_pathname_component_size] comes from getconf _POSIX_NAME_MAX / *)
let max_pathname_component_size = 255

let is_posix_pathname_component s =
  let module S = String in
  s <> "."
  && s <> ".."
  && Int.(0 < S.length s)
  && Int.(S.length s <= max_pathname_component_size)
  && (not (S.contains s '/'))
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
