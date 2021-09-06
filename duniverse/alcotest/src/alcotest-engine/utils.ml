let ( >> ) f g x = x |> f |> g

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

  let get_exn = function
    | Some x -> x
    | None -> invalid_arg "Option.get_exn: None"

  let value ~default = function None -> default | Some x -> x

  let ( || ) a b =
    match (a, b) with
    | None, None -> None
    | (Some _ as x), _ | None, (Some _ as x) -> x
end
