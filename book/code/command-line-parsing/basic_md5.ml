open Core

let do_hash file =
  In_channel.with_file file ~f:(fun ic ->
    let open Cryptokit in
    hash_channel (Hash.md5 ()) ic
    |> transform_string (Hexa.encode ())
    |> print_endline
  )
[@@@part "1"];;
let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)
[@@@part "2"];;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> do_hash filename)
[@@@part "3"];;
let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
[@@@part "4"];;
