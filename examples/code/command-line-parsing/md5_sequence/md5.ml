open Core

let do_hash filename ic =
  let open Cryptokit in
  hash_channel (Hash.md5 ()) ic
  |> transform_string (Hexa.encode ())
  |> fun md5 -> printf "MD5 (%s) = %s\n" filename md5

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = anon (sequence ("filename" %: file)) in
      fun () ->
        match files with
        | [] -> do_hash "-" In_channel.stdin
        | _ ->
          List.iter files ~f:(fun file ->
              In_channel.with_file ~f:(do_hash file) file))

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
