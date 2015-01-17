open Core.Std

let rec find_max best =
  match In_channel.input_line stdin with
  | None   -> best
  | Some x -> find_max (max best (Float.of_string x))

let () =
  match In_channel.input_line stdin with
  | None -> failwith "Empty file!"
  | Some first ->
    printf "Largest number: %F\n" (find_max (Float.of_string first))
