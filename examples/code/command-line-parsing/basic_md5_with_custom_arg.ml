open Core

let do_hash file () =
  In_channel.with_file file ~f:(fun ic ->
    let open Cryptokit in
    hash_channel (Hash.md5 ()) ic
    |> transform_string (Hexa.encode ())
    |> print_endline
  )

let regular_file =
  Command.Arg_type.create
    (fun filename ->
       match Sys.is_file filename with
       | `Yes -> filename
       | `No | `Unknown ->
         eprintf "'%s' is not a regular file.\n%!" filename;
         exit 1
    )

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    [%map_open
      let filename = anon ("filename" %: regular_file) in
      do_hash filename
    ]

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
