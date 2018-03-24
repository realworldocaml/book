open Core

let do_hash hash_length filename =
  In_channel.with_file filename ~f:(fun ic ->
    let open Cryptokit in
    hash_channel (Hash.md5 ()) ic
    |> transform_string (Hexa.encode ())
    |> (fun s -> String.prefix s hash_length)
    |> print_endline
  )

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
      map (both
            (anon ("hash_length" %: int))
            (anon ("filename" %: string)))
       ~f:(fun (hash_length,filename) ->
            (fun () -> do_hash hash_length filename)))

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
