open Core.Std
open Cryptokit

let _ =
  In_channel.(input_all stdin) |!
  hash_string (Hash.md5 ()) |!
  transform_string (Hexa.encode ()) |!
  print_endline
