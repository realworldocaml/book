open Core.Std
open Cryptokit

let () =
  let hash_fn =
    match Filename.basename Sys.argv.(0) with
    |"md5" -> Hash.md5 ()
    |"sha1" -> Hash.sha1 ()
    |_ -> Hash.md5 ()
  in
  In_channel.(input_all stdin) |!
  hash_string hash_fn |!
  transform_string (Hexa.encode ()) |!
  print_endline
