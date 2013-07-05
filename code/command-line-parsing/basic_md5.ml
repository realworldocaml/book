open Core.Std

let do_hash file =
  let open Cryptokit in
  In_channel.read_all file
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(
      empty
      +> anon ("filename" %: string)
    )
    (fun filename () -> do_hash filename)

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
