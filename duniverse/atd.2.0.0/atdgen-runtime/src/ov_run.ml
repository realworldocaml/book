let validate_list f path l =
  let rec loop f path i = function
    | [] -> None
    | x :: l ->
        let subpath = `Index i :: path in
        match f subpath x with
            None -> loop f path (i+1) l
          | err -> err
  in
  loop f path 0 l

let validate_array f path a =
  let rec loop f path a len i =
    if i >= len then None
    else
      match f (`Index i :: path) a.(i) with
          None -> loop f path a len (i+1)
        | err -> err
  in
  loop f path a (Array.length a) 0

let validate_option f path = function
    None -> None
  | Some x -> f path x
