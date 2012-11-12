open Core.Std

let rec read_and_accumulate ~line_num sum_so_far =
  match In_channel.input_line stdin with
  | None -> sum_so_far
  | Some line ->
    let x =
      try Float.of_string (String.strip line)
      with err ->
        eprintf "Ignoring line %d: %s\n" line_num (Exn.to_string err);
        0.
    in
    read_and_accumulate ~line_num:(line_num + 1) (sum_so_far +. x)

let () =
  printf "Total: %F\n" (read_and_accumulate ~line_num:1 0.)
