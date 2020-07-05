open Core

let do_hash filename =
  Md5.digest_file_blocking filename
  |> Md5.to_hex
  |> print_endline

[@@@part "1"] ;;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open file = anon ("filename" %: Filename.arg_type) in
      fun () -> do_hash file)
[@@@part "2"] ;;

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
