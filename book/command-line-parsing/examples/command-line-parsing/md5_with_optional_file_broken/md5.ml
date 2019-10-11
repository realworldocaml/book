open Core

let do_hash file =
  Md5.digest_file_blocking file
  |> Md5.to_hex
  |> print_endline

[@@@part "1"];;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: string)) in
      fun () -> do_hash filename)

[@@@part "2"];;
let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
