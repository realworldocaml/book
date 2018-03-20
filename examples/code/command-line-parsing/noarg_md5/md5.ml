open Core

let do_hash () =
  let open Cryptokit in
  hash_channel (Hash.md5 ()) In_channel.stdin
  |> transform_string (Hexa.encode ())
  |> print_endline
[@@@part "1"];;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.return do_hash
[@@@part "2"];;
let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
[@@@part "3"];;
