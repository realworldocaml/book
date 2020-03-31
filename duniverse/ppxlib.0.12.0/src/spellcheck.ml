open! Import

let spellcheck names name =
  let cutoff =
    match String.length name with
    | 1 | 2 -> 0
    | 3 | 4 -> 1
    | 5 | 6 -> 2
    | _ -> 3
  in
  let _, suggestions =
    List.fold_left names ~init:(Int.max_value, [])
      ~f:(fun ((best_distance, names_at_best_distance) as acc) registered_name ->
        match Ocaml_common.Misc.edit_distance name registered_name cutoff with
        | None -> acc
        | Some dist ->
          if dist < best_distance then
            (dist, [registered_name])
          else if dist > best_distance then
            acc
          else
            (dist, registered_name :: names_at_best_distance))
  in
  match List.rev suggestions |> List.filter ~f:(String.(<>) name) with
  | [] -> None
  | last :: rev_rest ->
    Some
      (Printf.sprintf "Hint: Did you mean %s%s%s?"
         (String.concat ~sep:", " (List.rev rev_rest))
         (if List.is_empty rev_rest then "" else " or ")
         last)
;;
