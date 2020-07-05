open Core

let get_contents = function
  | None | Some "-" ->
    In_channel.input_all In_channel.stdin
  | Some filename ->
    In_channel.read_all filename

let do_hash filename =
  get_contents filename
  |> Md5.digest_string
  |> Md5.to_hex
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename =
        anon (maybe ("filename" %: Filename.arg_type))
      in
      fun () -> do_hash filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
