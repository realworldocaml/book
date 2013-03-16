open Core.Std


(* [build_counts] computes a frequency count of lines read from [stdin]  *)
let build_counts () =
  (* [f] takes in the current frequency count and the new line, and returns an
     updated frequency count.  [In_channel.fold_lines] calls [f] once per line
     in the file, returning the final frequency count produced. *)
  In_channel.fold_lines stdin ~init:[] ~f:(fun counts line ->
    (* # of times this line has been seen --- 0 if the line is new *)
    let count =
      match List.Assoc.find counts line with
      | None -> 0
      | Some x -> x
    in
    (* update the frequency count *)
    List.Assoc.add counts line (count + 1)
  )

let () =
  (* Compute the line counts *)
  let counts = build_counts () in
  (* Sort the line counts in descending order of frequency *)
  let sorted_counts = List.sort ~cmp:(fun (_,x) (_,y) -> compare y x) counts  in
  (* Print out the frequency in the sorted count list *)
  List.iter (List.take sorted_counts 10) ~f:(fun (line,count) ->
    printf "%3d: %s\n" count line)
