open Core.Std

let rec process_lines already_read =
  printf "> %!"; (* %! flushes the channel *)
  match In_channel.input_line stdin with
  | None -> () (* EOF *)
  | Some line ->
    if List.mem already_read line then
      process_lines already_read
    else (
      printf "%s\n" line;
      process_lines (line :: already_read)
    )

let () = process_lines []
