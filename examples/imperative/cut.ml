open Core.Std

let destructive_sort filename =
  let lines =
    let inc = In_channel.create filename in
    protect ~f:(fun () -> In_channel.input_lines inc)
      ~finally:(fun () -> In_channel.close inc)
  in
  let lines = List.sort ~cmp:String.compare lines in
  let outc = Out_channel.create filename in
  protect ~f:(fun () -> Out_channel.output_lines outc lines)
    ~finally:(fun () -> Out_channel.close outc)

let command =
  Command.basic
    ~summary:"Sort the lines in a file, saving over the original file"
    Command.Spec.(empty +> anon ("FILENAME" %: file))
    (fun filename () -> destructive_sort filename)

let () = Command.run command

