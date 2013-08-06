open Core.Std

let rec read_and_accumulate sum_so_far =
  let line = In_channel.input_line stdin in
  match line with
  | None -> sum_so_far
  | Some x -> read_and_accumulate (sum_so_far +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
