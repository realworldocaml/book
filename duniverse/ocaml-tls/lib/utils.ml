module List_set = struct
  let subset ?(compare = compare) l1 l2 =
    let rec loop xs ys =
      match (xs, ys) with
      | ([], _)          -> true
      | (_, [])          -> false
      | (x::xss, y::yss) ->
          match compare x y with
          | -1 -> false
          |  1 -> loop xs yss
          |  _ -> loop xss yss in
    loop (List.sort compare l1) (List.sort compare l2)

  let is_proper_set l =
    let rec repeats = function
      | x::(y::_ as xs) -> x = y || repeats xs
      | _               -> false in
    not @@ repeats (List.sort compare l)
end

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs

let rec map_find ~f = function
  | []    -> None
  | x::xs ->
      match f x with
      | None         -> map_find ~f xs
      | Some _ as x' -> x'

let option none some = function
  | None   -> none
  | Some x -> some x

let init_and_last list =
  List.fold_right (fun x -> function
    | None         -> Some ([], x)
    | Some (xs, y) -> Some (x::xs, y))
  list None

let rec first_match l1 = function
  | []                      -> None
  | x::_ when List.mem x l1 -> Some x
  | _::xs                   -> first_match l1 xs
