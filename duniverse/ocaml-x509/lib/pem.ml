let ( let* ) = Result.bind

module Cs = struct
  open Cstruct

  let begins_with cs target =
    let l1 = length cs and l2 = length target in
    l1 >= l2 && equal (sub cs 0 l2) target

  let ends_with cs target =
    let l1 = length cs and l2 = length target in
    l1 >= l2 && equal (sub cs (l1 - l2) l2) target

  let null cs = length cs = 0

  let open_begin = of_string "-----BEGIN "
  and open_end   = of_string "-----END "
  and close      = of_string "-----"

  let tok_of_line cs =
    if null cs then
      `Empty
    else if get_char cs 0 = '#' then
      `Empty
    else if begins_with cs open_begin && ends_with cs close then
      `Begin (to_string @@ sub cs 11 (length cs - 16))
    else if begins_with cs open_end && ends_with cs close then
      `End (to_string @@ sub cs 9 (length cs - 14))
    else
      `Data cs

  let chop cs off len =
    let (a, b) = split cs off in (a, shift b len)

  let rec lines cs =
    let rec eol i =
      match get_char cs i with
      | '\r' when get_char cs (i + 1) = '\n' -> chop cs i 2
      | '\n' -> chop cs i 1
      | _    -> eol (i + 1) in
    match eol 0 with
    | exception Invalid_argument _ -> [ tok_of_line cs ]
    | a, b -> tok_of_line a :: lines b

  let combine ilines =
    let rec accumulate t acc = function
      | `Empty :: tail -> accumulate t acc tail
      | `Data cs :: tail -> accumulate t (cs :: acc) tail
      | `End t' :: tail ->
        if String.equal t t' then
          Ok (concat (List.rev acc), tail)
        else
          Error (`Msg ("invalid end, expected " ^ t ^ ", found " ^ t'))
      | _ :: _ -> Error (`Msg "invalid line, expected data or end")
      | [] -> Error (`Msg "end of input")
    in

    let rec block acc = function
      | `Begin t :: tail ->
        let* body, tail = accumulate t [] tail in
        let* data = Base64.decode (Cstruct.to_string body) in
        block ((t, Cstruct.of_string data) :: acc) tail
      | _::xs -> block acc xs
      | []    -> Ok (List.rev acc)
    in
    block [] ilines

  let parse data= combine (lines data)

  let unparse ~tag value =
    let rec split_at_64 acc = function
      | x when length x <= 64 -> List.rev (x :: acc)
      | x -> let here, rest = split x 64 in
        split_at_64 (here :: acc) rest
    in
    let raw = Cstruct.of_string (Base64.encode_string (Cstruct.to_string value)) in
    let pieces = split_at_64 [] raw in
    let nl = of_string "\n" in
    let lines = List.flatten (List.map (fun x -> [ x ; nl ]) pieces)
    in

    let tag = of_string tag in
    let first = [ open_begin ; tag ; close ; nl ]
    and last = [ open_end ; tag ; close ; nl ]
    in
    concat (first @ lines @ last)
end

let parse, unparse = Cs.(parse, unparse)

let exactly_one ~what = function
  | []  -> Error (`Msg ("No " ^ what))
  | [x] -> Ok x
  | _   -> Error (`Msg ("Multiple " ^ what ^ "s"))

let foldM f data =
  let wrap acc data =
    let* datas' = acc in
    let* data = f data in
    Ok (data :: datas')
  in
  let* res = List.fold_left wrap (Ok []) data in
  Ok (List.rev res)
