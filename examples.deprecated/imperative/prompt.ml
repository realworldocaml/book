open Core.Std

let (|>) = (|!)

let destructive_sort_1 filename =
  let lines =
    let inc = In_channel.create filename in
    protect ~f:(fun () -> In_channel.input_lines inc)
      ~finally:(fun () -> In_channel.close inc)
  in
  let lines = List.sort ~cmp:String.compare lines in
  let outc = Out_channel.create filename in
  protect ~f:(fun () -> Out_channel.output_lines outc lines)
    ~finally:(fun () -> Out_channel.close outc)

let confirm filename =
  let question =
    "Do you want to overwrite the contents of "^filename^"?"
  in
  Out_channel.output_string stdout (question ^ " (y/n): ");
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> false
  | Some response ->
    match String.lowercase response with
    | "y" | "yes" -> true
    | _ -> false

let destructive_sort_3 filename =
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
      destructive_sort_3 filename)

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

