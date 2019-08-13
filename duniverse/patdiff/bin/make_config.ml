open Core

let doc = "FILE Write default configuration file"

let main file =
  let sample = Patdiff_lib.Configuration.default in
  let delete =
    if Sys.file_exists_exn file
    then (
      printf "%s already exists. Overwrite? (y/n) %!" file;
      let resp = In_channel.input_line In_channel.stdin in
      let resp = Option.value ~default:"" resp in
      let resp = String.lowercase resp in
      match resp with
      | "yes" | "y" -> true
      | _ -> false)
    else true
  in
  if delete
  then (
    try
      Out_channel.with_file ~f:(fun oc -> Out_channel.output_string oc sample) file;
      printf "Default configuration written to %s\n%!" file
    with
    | e -> failwithf "Error: %s" (Exn.to_string e) ())
  else (
    printf "Configuration file not written!\n%!";
    exit 1)
;;
