(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let make summary_only =
  object (self)
    method header = ""
    method footer = ""
    method summary s = "Coverage summary: " ^ (self#sum s)
    method file_header f = if not summary_only then Printf.sprintf "File '%s': " f else ""
    method file_footer _ = ""
    method file_summary s = if not summary_only then self#sum s else ""
    method point _ _ = ""
    method private sum s =
      let numbers x y =
        if y > 0 then
          let p = ((float_of_int x) *. 100.) /. (float_of_int y) in
          Printf.sprintf "%d/%d (%.2f%%)" x y p
        else
          "none" in
      Report_utils.(numbers s.visited s.total) ^ "\n"
  end



let output ~per_file counts =
  let stats =
    Hashtbl.fold (fun file counts acc ->
      let total = Array.length counts in
      let visited =
        Array.fold_left
          (fun acc count -> if count > 0 then acc + 1 else acc) 0 counts
      in
      (file, visited, total)::acc) counts []
  in

  let percentage numerator denominator =
    if denominator > 0 then
      let p =
        ((float_of_int numerator) *. 100.) /. (float_of_int denominator) in
      Printf.sprintf "%.2f" p
    else
      "0.00"
  in

  let second (_, v, _) = v in
  let third (_, _, v) = v in

  let total projection =
    stats
    |> List.map projection
    |> List.fold_left (+) 0
  in
  let visited_total = total second in
  let overall_total = total third in

  if per_file then begin
    let digits i =
      let rec loop bound count =
        if bound > i then
          count
        else
          loop (bound * 10) (count + 1)
      in
      loop 10 1
    in
    let digits projection =
      ("", visited_total, overall_total)::stats
      |> List.map projection
      |> List.map digits
      |> List.fold_left max 1
    in
    let visited_digits = digits second in
    let total_digits = digits third in

    stats
    |> List.sort (fun (file, _, _) (file', _, _) -> String.compare file file')
    |> List.iter begin fun (name, visited, total) ->
      Printf.printf "%6s %%   %*i/%-*i   %s\n"
        (percentage visited total)
        visited_digits visited
        total_digits total
        name
    end;

    Printf.printf "%6s %%   %i/%i   Project coverage\n%!"
      (percentage visited_total overall_total) visited_total overall_total
  end
  else
    Printf.printf "Coverage: %i/%i (%s%%)\n%!"
      visited_total overall_total (percentage visited_total overall_total)
