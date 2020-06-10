module Html = Tyxml.Html

(* Shared utility functions *)

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep :: tl

let rec list_concat_map_list_sep ~sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map_list_sep ~sep ~f xs in
    hd @ sep @ tl

let optional_code children =
  match children with
  | [] -> []
  | children -> [ Html.code children ]
