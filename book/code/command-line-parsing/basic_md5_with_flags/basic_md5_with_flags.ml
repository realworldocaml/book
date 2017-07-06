open Core
open Cryptokit

let checksum_from_string buf =
  hash_string (Hash.md5 ()) buf
  |> transform_string (Hexa.encode ())
  |> print_endline

let checksum_from_file filename =
  let ic = match filename with
    | "-" -> In_channel.stdin
    | _   -> In_channel.create ~binary:true filename
  in
  hash_channel (Hash.md5 ()) ic
  |> transform_string (Hexa.encode ())
  |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    Command.Spec.(
      empty
      +> flag "-s" (optional string) ~doc:"string Checksum the given string"
      +> flag "-t" no_arg ~doc:" run a built-in time trial"
      +> anon (maybe_with_default "-" ("filename" %: file))
    )
    (fun use_string trial filename () ->
       match trial with
       | true -> printf "Running time trial\n"
       | false -> begin
           match use_string with
           | Some buf -> checksum_from_string buf
           | None -> checksum_from_file filename
         end
    )

let () = Command.run command
