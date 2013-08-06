open Core.Std

let destructive_sort filename =
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

