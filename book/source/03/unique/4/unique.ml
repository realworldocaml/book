open Core.Std

(* [already_read] is a list containing lines that have already
   been written out and therefore don't need to be written out again.
*)
let rec process_lines already_read =
   match In_channel.input_line stdin with
   | None -> () (* we're at the end-of-file *)
   | Some line ->
     if String_set.mem already_read line then
       process_lines already_read (* skip on to the next line *)
     else (
       (* print out the line, and continue the loop with an updated
          [already_read] *)
       Out_channel.output_string stdout line;
       Out_channel.newline stdout;
       process_lines (String_set.add already_read line)
     )

let () = process_lines []
