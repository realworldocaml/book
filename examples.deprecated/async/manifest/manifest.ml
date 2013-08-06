open Core.Std
open Async.Std

let manifest_size filename =
  Reader.file_lines filename
  >>= fun lines ->
  Deferred.all
    (List.map lines ~f:(fun file -> Reader.file_lines file))
  >>= fun lengths ->
  let total_length = List.fold ~init:0 ~f:(+) lengths in
  return total_length
;;
