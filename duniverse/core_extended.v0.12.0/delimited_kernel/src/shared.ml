open Core_kernel

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

type ('a, 'b) reader =
  ?strip:bool
  -> ?skip_lines:int
  -> ?on_parse_error:[`Raise | `Handle of string Queue.t -> exn -> [`Continue | `Finish]]
  -> header:'a
  -> 'b

let strip_buffer buf =
  let len = Buffer.length buf in
  let rec first_non_space n =
    if n >= len
    then None
    else if Buffer.nth buf n <> ' '
    then Some n
    else first_non_space (n + 1)
  in
  let rec last_non_space n =
    if n < 0
    then None
    else if Buffer.nth buf n <> ' '
    then Some n
    else last_non_space (n - 1)
  in
  match first_non_space 0 with
  | None -> ""
  | Some s ->
    (match last_non_space (len - 1) with
     | None -> assert false
     | Some e -> Buffer.To_string.sub buf ~pos:s ~len:(e - s + 1))
;;

let make_emit_field ~strip current_row field =
  stage (fun () ->
    Queue.enqueue
      current_row
      (if strip then strip_buffer field else Buffer.contents field);
    Buffer.clear field)
;;

let set_headers header_index headers =
  List.iteri headers ~f:(fun i h ->
    Option.iter h ~f:(fun h ->
      match Hashtbl.find header_index h with
      | None -> Hashtbl.set header_index ~key:h ~data:i
      | Some other_i ->
        failwithf "header %s duplicated at position %i and %i" h other_i i ()))
;;

let make_emit_row current_row row_queue header ~lineno =
  let module Table = String.Table in
  let header_index =
    match (header : Header.t) with
    | `No | `Yes | `Require _ | `Transform _ | `Filter_map _ -> Table.create () ~size:1
    | `Replace headers
    | `Add headers -> Table.of_alist_exn (List.mapi headers ~f:(fun i s -> s, i))
  in
  let header_processed =
    ref
      (match header with
       | `No | `Add _ -> true
       | `Require _ | `Replace _ | `Transform _ | `Filter_map _ | `Yes -> false)
  in
  ( `on_eof (fun () -> if not !header_processed then failwith "Header line was not found")
  , fun () ->
    if not !header_processed
    then (
      header_processed := true;
      match header with
      | `No | `Add _ -> assert false
      | `Require at_least ->
        let headers = Queue.to_list current_row in
        List.iter at_least ~f:(fun must_exist ->
          match List.findi headers ~f:(fun _ h -> h = must_exist) with
          | None ->
            failwithf
              "The required header '%s' was not found in '%s' (lineno=%d)"
              must_exist
              (String.concat ~sep:"," headers)
              !lineno
              ()
          | Some (i, _) -> Hashtbl.set header_index ~key:must_exist ~data:i)
      | `Replace _new_headers -> () (* already set above *)
      | `Transform f ->
        Queue.to_list current_row
        |> f
        |> List.map ~f:Option.some
        |> set_headers header_index
      | `Filter_map f -> Queue.to_list current_row |> f |> set_headers header_index
      | `Yes ->
        Queue.to_list current_row
        |> List.map ~f:Option.some
        |> set_headers header_index)
    else Queue.enqueue row_queue (Row.create header_index (Queue.to_array current_row));
    lineno := !lineno + 1;
    Queue.clear current_row )
;;
