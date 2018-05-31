open Core

let do_hash file =
  In_channel.with_file file ~f:(fun ic ->
    let open Cryptokit in
    hash_channel (Hash.md5 ()) ic
    |> transform_string (Hexa.encode ())
    |> print_endline
  )

[@@@part "1"];;
let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    [%map_open
      let filename = anon (maybe ("filename" %: string)) in
      do_hash filename
    ]

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
