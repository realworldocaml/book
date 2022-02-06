(* small helper to determine whether two files differ *)

type comparison = Same | Different

let rec compare first second =
  match input_line first with
  | first_line -> (
      match input_line second with
      | second_line ->
          match String.equal first_line second_line with
          | true -> compare first second
          | false ->
              (* we found a difference between the lines *)
              Different
      | exception End_of_file ->
          (* the second file ended before the first *)
          Different)
  | exception End_of_file ->
      (* the first file ended first *)
      match input_line second with
      | _ ->
          (* the second file continues: a difference *)
          Different
      | exception End_of_file ->
          (* the second file ended too *)
          Same

let main () =
  let first = Sys.argv.(1) |> open_in in
  let second = Sys.argv.(2) |> open_in in
  let comparison = compare first second in
  close_in first;
  close_in second;
  match comparison with
  | Same ->
    prerr_endline "The files appear to be identical";
    (* we didn't find a difference, exit with a failure code *)
    exit 1
  | Different -> ()

let () =
  main ()
