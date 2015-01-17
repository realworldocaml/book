(* Convert *)

open Core.Std

let () =
  Out_channel.output_string stdout "Print in uppercase? (y/N) \n";
  let in_uppercase =
    match In_channel.input_line stdin with
    | None -> false
    | Some answer -> String.lowercase answer = "y"
  in
  let hw = "Hello World!\n" in
  let hw = if in_uppercase then String.uppercase hw else hw in
  Out_channel.output_string stdout hw
