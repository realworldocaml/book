open Core.Std

let read_float () =
  match In_channel.input_line stdin with
  | None   -> None
  | Some s -> Some (Float.of_string s)

let rec find_max best =
  match read_float () with
  | None   -> best  (* end of file, so return *)
  | Some x -> find_max (Float.max best x)

let () =
  match read_float () with
  | None   -> failwith "Empty file!"
  | Some x -> printf "Largest number: %F\n" (find_max x)
