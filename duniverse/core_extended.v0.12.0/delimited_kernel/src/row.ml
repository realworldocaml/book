open! Core_kernel

type t =
  { header_map : int String.Map.t
  ; fields : string array
  }
[@@deriving compare, fields]

let is_empty t = Array.for_all t.fields ~f:String.is_empty

let sexp_of_t t =
  let names_by_indices =
    Map.to_sequence t.header_map
    |> Sequence.fold ~init:Int.Map.empty ~f:(fun init (name, index) ->
      Map.set init ~key:index ~data:name)
  in
  Array.mapi t.fields ~f:(fun i v ->
    let k =
      match Map.find names_by_indices i with
      | None -> Int.to_string i
      | Some name -> name
    in
    k, v)
  |> [%sexp_of: (string * string) array]
;;

let to_string t = Sexp.to_string_hum (sexp_of_t t)

let index_exn t header =
  try String.Map.find_exn t.header_map header with
  | _ -> raise_s [%message "Header not found" (header : string)]
;;

let get_exn_p t header here =
  let i = index_exn t header in
  try t.fields.(i) with
  | _ ->
    raise_s
      [%message
        "Header exists in file but not for this row"
          (here : Source_code_position.t)
          (header : string)
          ~row:(t : t)]
;;

let get_exn t header = get_exn_p t header [%here]

let get_conv_exn t header here conv =
  let v = get_exn_p t header here in
  try conv v with
  | exn ->
    raise_s
      [%message
        "Failed to parse"
          (here : Source_code_position.t)
          (header : string)
          ~row:(t : t)
          (exn : exn)]
;;

let get t header =
  try Some (get_exn t header) with
  | _ -> None
;;

let get_opt_exn t header =
  match get t header with
  | None -> raise_s [%message "No header in row" (header : string) ~row:(t : t)]
  | Some "" -> None
  | Some str -> Some str
;;

let get_conv_opt_exn t header here conv =
  match get_opt_exn t header with
  | None -> None
  | Some v ->
    (try Some (conv v) with
     | exn ->
       raise_s
         [%message
           "Failed to parse"
             (here : Source_code_position.t)
             (header : string)
             ~row:(t : t)
             (exn : exn)])
;;

let nth_exn t i = t.fields.(i)

let nth_conv_exn t i here conv =
  try conv (nth_exn t i) with
  | exn ->
    raise_s
      [%message
        "Failed to parse"
          (here : Source_code_position.t)
          (i : int)
          ~row:(t : t)
          (exn : exn)]
;;

let nth t i =
  try Some (nth_exn t i) with
  | _ -> None
;;

let nth_conv t i conv =
  try Some (conv (nth_exn t i)) with
  | _ -> None
;;

let create' header_map fields = { header_map; fields }

let create header_table fields =
  { header_map =
      Hashtbl.fold header_table ~init:String.Map.empty ~f:(fun ~key ~data init ->
        Map.set init ~key ~data)
  ; fields
  }
;;

let to_list t = Array.to_list t.fields
let to_array t = t.fields
let length t = Array.length t.fields
let equal t1 t2 = 0 = [%compare: t] t1 t2

let fold t ~init ~f =
  Map.fold t.header_map ~init ~f:(fun ~key:header ~data:i acc ->
    f acc ~header ~data:t.fields.(i))
;;

let iter t ~f = fold t ~init:() ~f:(fun () ~header ~data -> f ~header ~data)
let headers t = t.header_map

let list_of_headers t =
  Map.to_alist t.header_map
  |> List.sort ~compare:(fun a b -> Int.compare (snd a) (snd b))
  |> List.map ~f:fst
;;

module Expert = struct
  let of_buffer header_map row_queue =
    create' header_map (Append_only_buffer.to_array row_queue)
  ;;
end
