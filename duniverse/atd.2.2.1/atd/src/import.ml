module String = struct
  [@@@ocaml.warning "-3-32"]
  let lowercase_ascii = StringLabels.lowercase
  let uppercase_ascii = StringLabels.uppercase
  let capitalize_ascii = StringLabels.capitalize
  include String
end

module Char = struct
  [@@@ocaml.warning "-3-32"]
  let uppercase_ascii = Char.uppercase
  include Char
end

module List = struct
  include List

  let rec filter_map f = function
      [] -> []
    | x :: l ->
        match f x with
          None -> filter_map f l
        | Some y -> y :: filter_map f l

  let concat_map f l =
    List.map f l
    |> List.flatten

  let map_first f = function
    | [] -> []
    | x :: l ->
        let y = f ~is_first:true x in
        y :: List.map (f ~is_first:false) l

  let init n f = Array.to_list (Array.init n f)

  let mapi l f =
    Array.of_list l
    |> Array.mapi f
    |> Array.to_list

  let rec find_map f = function
    | [] -> None
    | x :: l ->
        match f x with
          None -> find_map f l
        | Some _ as y -> y

  (* replace first occurrence, if any *)
  let rec assoc_update k v = function
    |  (k', _) as x :: l ->
        if k = k' then
          (k, v) :: l
        else
          x :: assoc_update k v l
    | [] ->
        []

  let rec insert_sep t ~sep =
    match t with
    | []
    | [_] -> t
    | x :: xs -> x :: sep @ (insert_sep xs ~sep)

  let split3 l =
    let (x, y, z) =
      List.fold_left (fun (xs, ys, zs) (x, y, z) ->
        (x::xs, y::ys, z::zs)
      ) ([], [], []) l in
    (List.rev x, List.rev y, List.rev z)

  let assoc_exn = assoc

  let assoc key xs =
    match List.assoc key xs with
    | s -> Some s
    | exception Not_found -> None
end

module Option = struct
  let map f = function
    | None -> None
    | Some s -> Some (f s)

  let value_exn = function
    | None -> failwith "Option.value_exn"
    | Some s -> s

  let value ~default = function
    | None -> default
    | Some s -> s

  let is_some = function
    | None -> false
    | Some _ -> true

  module O = struct
    let (>>=) x f =
      match x with
      | None -> None
      | Some x -> f x
  end
end

let sprintf = Printf.sprintf
let printf = Printf.printf
let eprintf = Printf.eprintf
let bprintf = Printf.bprintf
let fprintf = Printf.fprintf
