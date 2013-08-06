open Core.Std

let confirm filename =
  let question =
    "Do you want to overwrite the contents of "^filename^"? (y/n): "
  in
  Out_channel.output_string stdout question;
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> false
  | Some response ->
    match String.strip (String.lowercase response) with
    | "y" | "yes" -> true
    | _ -> false

let destructive_sort filename =
  if not (confirm filename)
  then Out_channel.output_string stderr "Exiting.\n"
  else
    let lines =
      In_channel.with_file filename ~f:In_channel.input_lines
      |! List.sort ~cmp:String.compare
    in
    Out_channel.with_file filename ~f:(fun outc ->
      Out_channel.output_lines outc lines)


let command =
  Command.basic
    ~summary:"Sort the lines in a file, saving over the original file"
    Command.Spec.(empty +> anon ("FILENAME" %: file))
    (fun filename () ->
      destructive_sort filename)

let () = Command.run command

