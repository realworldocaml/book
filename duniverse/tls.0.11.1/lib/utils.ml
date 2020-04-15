
let const a _ = a

let id a = a

let o f g x = f (g x)

module List_set = struct

  let inter ?(compare = compare) l1 l2 =
    let rec loop xs ys =
      match (xs, ys) with
      | ([], _) | (_, []) -> []
      | (x::xss, y::yss)  ->
          match compare x y with
          | -1 -> loop xss ys
          |  1 -> loop xs yss
          |  _ -> x :: loop xss yss in
    loop List.(sort compare l1) List.(sort compare l2)

  let union ?(compare = compare) l1 l2 =
    let rec loop xs ys =
      match (xs, ys) with
      | ([], _)          -> ys
      | (_, [])          -> xs
      | (x::xss, y::yss) ->
          match compare x y with
          | -1 -> x :: loop xss ys
          |  1 -> y :: loop xs yss
          |  _ -> x :: loop xss yss in
    loop List.(sort compare l1) List.(sort compare l2)

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
    loop List.(sort compare l1) List.(sort compare l2)

  let equal ?(compare = compare) l1 l2 =
    List.(sort compare l1 = sort compare l2)

  let is_proper_set l =
    let rec repeats = function
      | x::(y::_ as xs) -> x = y || repeats xs
      | _               -> false in
    not @@ repeats (List.sort compare l)

end

module Cs = struct

  let appends = function
    | []   -> Cstruct.create 0
    | [cs] -> cs
    | csn  ->
        let cs = Cstruct.(create @@ lenv csn) in
        let _ =
          List.fold_left
            (fun off e ->
              let len = Cstruct.len e in
              ( Cstruct.blit e 0 cs off len ; off + len ))
            0 csn in
        cs

  let append cs1 cs2 = appends [ cs1; cs2 ]

  let (<+>) = append

  let equal cs1 cs2 =
    Cstruct.((len cs1 = len cs2) && (to_bigarray cs1 = to_bigarray cs2))

  let empty = Cstruct.create 0

  let null cs = Cstruct.len cs = 0
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
