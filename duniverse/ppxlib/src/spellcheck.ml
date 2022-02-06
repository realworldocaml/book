open! Import

exception Cutoff_met

(* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
let levenshtein_distance s t cutoff =
  let m = String.length s and n = String.length t in
  if cutoff = 0 || abs (m - n) >= cutoff then None
  else
    (* for all i and j, d.(i).(j) will hold the Levenshtein distance between the
       first i characters of s and the first j characters of t *)
    let d = Array.make_matrix ~dimx:(m + 1) ~dimy:(n + 1) 0 in
    for i = 0 to m do
      (* the distance of any first string to an empty second string *)
      d.(i).(0) <- i
    done;
    for j = 0 to n do
      (* the distance of any second string to an empty first string *)
      d.(0).(j) <- j
    done;
    (* the minimum of each line together with the column index will be used
       to notice cutoff exceeding and return early in that case *)
    let line_min = ref 0 in
    let distance =
      try
        for j = 1 to n do
          if !line_min >= cutoff - 1 && j >= cutoff - 1 then raise Cutoff_met;
          line_min := max m n;
          for i = 1 to m do
            let value =
              if Char.equal s.[i - 1] t.[j - 1] then d.(i - 1).(j - 1)
                (* no operation required *)
              else
                min
                  (d.(i - 1).(j) + 1) (* a deletion *)
                  (min
                     (d.(i).(j - 1) + 1) (* an insertion *)
                     (d.(i - 1).(j - 1) + 1) (* a substitution *))
            in
            d.(i).(j) <- value;
            line_min := min !line_min value
          done
        done;
        if d.(m).(n) < cutoff then Some d.(m).(n) else None
      with Cutoff_met -> None
    in
    distance

let spellcheck names name =
  let cutoff =
    match String.length name with
    | 1 | 2 -> 0
    | 3 | 4 -> 1
    | 5 | 6 -> 2
    | _ -> 3
  in
  let _, suggestions =
    List.fold_left names ~init:(Int.max_int, [])
      ~f:(fun ((best_distance, names_at_best_distance) as acc) registered_name
         ->
        match levenshtein_distance name registered_name cutoff with
        | None -> acc
        | Some dist ->
            if dist < best_distance then (dist, [ registered_name ])
            else if dist > best_distance then acc
            else (dist, registered_name :: names_at_best_distance))
  in
  match List.rev suggestions |> List.filter ~f:(String.( <> ) name) with
  | [] -> None
  | last :: rev_rest ->
      Some
        (Printf.sprintf "Hint: Did you mean %s%s%s?"
           (String.concat ~sep:", " (List.rev rev_rest))
           (if List.is_empty rev_rest then "" else " or ")
           last)
