let ( >> ) f g x = x |> f |> g

module Fun = struct
  let id x = x
end

module Int = struct
  module Set = Set.Make (struct
    type t = int

    let compare = (compare : int -> int -> int)
  end)
end

module String = struct
  include Astring.String

  let length_utf8 = Uutf.String.fold_utf_8 (fun count _ _ -> count + 1) 0

  let prefix_utf8 uchars string =
    let exception Found_new_length of int in
    try
      let (_ : int) =
        Uutf.String.fold_utf_8
          (fun count start_pos _ ->
            if count = uchars then raise (Found_new_length start_pos)
            else count + 1)
          0 string
      in
      string
    with Found_new_length l -> String.sub string 0 l
end

module List = struct
  include List

  type 'a t = 'a list

  let rev_head n l =
    let rec aux acc n l =
      match l with
      | x :: xs ->
          if n > 0 then (aux [@tailcall]) (x :: acc) (n - 1) xs else acc
      | [] -> acc
    in
    aux [] n l

  let concat_map f l =
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
          let xs = f x in
          (aux [@tailcall]) f (rev_append xs acc) l
    in
    aux f [] l

  let filter_map f l =
    let rec inner acc = function
      | [] -> rev acc
      | x :: xs -> (
          match f x with
          | None -> (inner [@tailcall]) acc xs
          | Some y -> (inner [@tailcall]) (y :: acc) xs)
    in
    inner [] l

  let lift_result l =
    List.fold_right
      (fun a b ->
        match (a, b) with
        | Ok o, Ok acc -> Ok (o :: acc)
        | Ok _, Error e -> Error e
        | Error e, Error acc -> Error (e :: acc)
        | Error e, Ok _ -> Error [ e ])
      l (Ok [])

  let init n f =
    let rec aux acc i = if i >= n then rev acc else aux (f i :: acc) (i + 1) in
    aux [] 0
end

module Result = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
end

module Option = struct
  let is_some = function Some _ -> true | None -> false
  let map f = function Some x -> Some (f x) | None -> None

  let get_exn = function
    | Some x -> x
    | None -> invalid_arg "Option.get_exn: None"

  let value ~default = function None -> default | Some x -> x

  let ( || ) a b =
    match (a, b) with
    | None, None -> None
    | (Some _ as x), _ | None, (Some _ as x) -> x
end

module Cmdliner_syntax = struct
  open Cmdliner

  let ( let+ ) t f = Term.(const f $ t)
  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
  let ( >>| ) x f = Term.(app (const f) x)
end
