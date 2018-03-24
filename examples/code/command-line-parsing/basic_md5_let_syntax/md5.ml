open Core

let do_hash hash_length filename =
  In_channel.with_file filename ~f:(fun ic ->
    let open Cryptokit in
    hash_channel (Hash.md5 ()) ic
    |> transform_string (Hexa.encode ())
    |> (fun s -> String.prefix s hash_length)
    |> print_endline
  )

[@@@part "1"];;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map
       hash_length = anon ("hash_length" %: int)
     and filename  = anon ("filename" %: string)
     in
     fun () -> do_hash hash_length filename)

[@@@part "2"];;
let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
