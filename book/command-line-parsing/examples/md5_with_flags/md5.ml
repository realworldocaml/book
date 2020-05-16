open Core

let checksum_from_string buf =
  Md5.digest_string buf
  |> Md5.to_hex
  |> print_endline

let checksum_from_file filename =
  let contents = match filename with
    | "-"      -> In_channel.input_all In_channel.stdin
    | filename -> In_channel.read_all filename
  in
  Md5.digest_string contents
  |> Md5.to_hex
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    Command.Let_syntax.(
      let%map_open
        use_string = flag "-s" (optional string)
          ~doc:"string Checksum the given string"
      and trial = flag "-t" no_arg ~doc:" run a built-in time trial"
      and filename =
        anon (maybe_with_default "-" ("filename" %: Filename.arg_type))
      in
      fun () ->
        if trial then printf "Running time trial\n"
        else match use_string with
          | Some buf -> checksum_from_string buf
          | None -> checksum_from_file filename)

let () = Command.run command
