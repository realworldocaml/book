open Core.Std

let get_file_data file checksum =
  match file, checksum with
  | None, Some buf -> buf
  | _, Some buf -> eprintf "Warning: ignoring file\n"; buf
  | (None|Some "-"), None -> In_channel.(input_all stdin)
  | Some file, None -> In_channel.read_all file
 
let do_hash file checksum =
  let open Cryptokit in
  get_file_data file checksum
  |> hash_string (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    Command.Spec.(
      empty
      +> flag "-s" (optional string) ~doc:"string Checksum the given string"
      +> flag "-t" no_arg ~doc:" run a built-in time trial"
      +> anon (maybe ("filename" %: string))
    )
  (fun checksum trial file () ->
    match trial with
    | true -> printf "Running time trial\n"
    | false -> do_hash file checksum)

let () = Command.run command
