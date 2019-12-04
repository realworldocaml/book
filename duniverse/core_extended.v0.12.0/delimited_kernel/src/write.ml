open Core_kernel
open! Int.Replace_polymorphic_compare

type -'a t =
  { headers : string list
  ; to_columns : 'a -> tail:string list -> string list
  }
[@@deriving fields]

let empty = { headers = []; to_columns = (fun _ ~tail -> tail) }

let column to_string ~header =
  { headers = [ header ]; to_columns = (fun x ~tail -> to_string x :: tail) }
;;

let append l r =
  let to_columns_l = l.to_columns
  and to_columns_r = r.to_columns in
  { headers = List.append l.headers r.headers
  ; to_columns = (fun x ~tail -> to_columns_l x ~tail:(to_columns_r x ~tail))
  }
;;

let of_list = function
  | [] -> empty
  | [ x ] -> x
  | first :: others -> List.fold others ~init:first ~f:append
;;

let contra_map x ~f =
  let to_columns = x.to_columns in
  { x with to_columns = (fun x ~tail -> to_columns (f x) ~tail) }
;;

let map_headers t ~f = { t with headers = List.map t.headers ~f }
let to_columns t x = to_columns t x ~tail:[]

module Fields_O = struct
  let ( !! ) to_string field =
    let read_field = Field.get field in
    column (fun r -> to_string (read_field r)) ~header:(Field.name field)
  ;;

  let ( !> ) inner field =
    map_headers
      inner
      ~f:
        (let prefix = Field.name field ^ "_" in
         fun name -> prefix ^ name)
    |> contra_map ~f:(Field.get field)
  ;;
end

module O = struct
  let ( <<| ) t f = contra_map t ~f
  let ( <> ) = append
end

let to_string_m (type t) (module T : Stringable.S with type t = t) = T.to_string
let column_m m ~header = column (to_string_m m) ~header

let column_m_opt ?(default = "") m ~header =
  column (Option.value_map ~default ~f:(to_string_m m)) ~header
;;

module Expert = struct
  (* The standard string transformations are split in two:
     - one to get the length of the result (can work on substring)
     - another one to perform the action (with string blit semmantic)

     Common arguments

     -> to figure out how to escape/print quote and separators.
     -> to operate on substrings : pos len
     -> to perform string transformations: all the blit arguments

  *)

  (* Field handling *)
  let rec quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos src_end =
    if src_pos = src_end
    then dst_pos
    else (
      match src.[src_pos] with
      | c
        when Char.equal c quote ->
        Bytes.set dst dst_pos quote;
        Bytes.set dst (dst_pos + 1) quote;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 2)
          src_end
      | c ->
        Bytes.set dst dst_pos c;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 1)
          src_end)
  ;;

  let quote_blit ~(quote : char) ~src ~dst ~src_pos ~dst_pos ~len =
    quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos (src_pos + len)
  ;;

  (** Find the length of a quoted field... *)
  let rec quote_len_loop ~quote ~sep ~pos ~end_pos ~should_escape s acc =
    if pos = end_pos
    then if should_escape then Some acc else None
    else (
      match s.[pos] with
      | c
        when Char.equal c quote ->
        quote_len_loop s ~quote ~sep ~pos:(pos + 1) ~end_pos ~should_escape:true (acc + 1)
      | c
        when Char.equal c sep ->
        quote_len_loop s ~quote ~sep ~pos:(pos + 1) ~end_pos ~should_escape:true acc
      | '\n' ->
        quote_len_loop s ~quote ~sep ~pos:(pos + 1) ~end_pos ~should_escape:true acc
      | _ -> quote_len_loop s ~quote ~sep ~pos:(pos + 1) ~end_pos ~should_escape acc)
  ;;

  let quote_len ~quote ~sep ~pos ~len s =
    if len = 0
    then None
    else (
      let ( = ) = Char.( = ) in
      let trailling_ws =
        s.[pos] = ' '
        || s.[pos] = '\t'
        || s.[pos + len - 1] = ' '
        || s.[pos + len - 1] = '\t'
      in
      quote_len_loop
        s
        ~quote
        ~sep
        ~pos
        ~end_pos:(len + pos)
        ~should_escape:trailling_ws
        len)
  ;;

  (** Tables *)

  let maybe_escape_field ?(quote = '"') ?(sep = ',') s =
    let len = String.length s in
    match quote_len s ~quote ~sep ~len ~pos:0 with
    | None -> s
    | Some qlen ->
      let res = Bytes.create (qlen + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (qlen + 1) quote;
      ignore (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len : int);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;

  let escape_field ?(quote = '"') s =
    let len = String.length s in
    match quote_len s ~quote ~sep:',' ~len ~pos:0 with
    | None ->
      let res = Bytes.create (len + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (len + 1) quote;
      Bytes.From_string.blit ~src_pos:0 ~dst_pos:1 ~len ~src:s ~dst:res;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
    | Some qlen ->
      let res = Bytes.create (qlen + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (qlen + 1) quote;
      ignore (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len : int);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;
end

module By_row = struct
  type row = string list

  (** Line handling *)
  let rec line_spec_loop ~quote ~sep esc_acc size acc =
    match acc, esc_acc with
    | [], [] -> [], 0
    | [], _ -> List.rev esc_acc, size - 1 (* We overshot our count by one comma*)
    | h :: t, _ ->
      let len = String.length h in
      (match Expert.quote_len h ~quote ~sep ~len ~pos:0 with
       | None -> line_spec_loop ~quote ~sep ((false, h) :: esc_acc) (size + len + 1) t
       | Some qlen ->
         line_spec_loop ~quote ~sep ((true, h) :: esc_acc) (size + qlen + 3) t)
  ;;

  let field_blit ~quote ~dst ~pos = function
    | true, h ->
      Bytes.set dst pos quote;
      let len = String.length h in
      let qpos =
        Expert.quote_blit ~quote ~src:h ~src_pos:0 ~dst ~dst_pos:(pos + 1) ~len
      in
      Bytes.set dst qpos quote;
      qpos + 1
    | false, h ->
      let len = String.length h in
      Bytes.From_string.blit ~dst_pos:pos ~src_pos:0 ~dst ~src:h ~len;
      pos + len
  ;;

  let rec line_blit_loop ~quote ~sep ~dst ~pos = function
    | [] -> pos
    | [ v ] -> field_blit ~quote:'"' ~dst ~pos v
    | v :: (_ :: _ as t) ->
      let pos = field_blit ~quote:'"' ~dst ~pos v in
      Bytes.set dst pos sep;
      line_blit_loop ~quote ~sep ~dst ~pos:(pos + 1) t
  ;;

  let line_to_string ?(quote = '"') ?(sep = ',') l =
    let spec, len = line_spec_loop ~quote ~sep [] 0 l in
    let res = Bytes.create len in
    ignore (line_blit_loop ~quote ~sep ~dst:res ~pos:0 spec : int);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;

  let rec output_lines_loop ~quote ~sep ~buff ~eol oc = function
    | [] -> ()
    | h :: t ->
      let spec, len = line_spec_loop ~quote ~sep [] 0 h in
      let buff = if Bytes.length buff < len then Bytes.create (2 * len) else buff in
      ignore (line_blit_loop ~quote ~sep ~dst:buff ~pos:0 spec : int);
      Out_channel.output oc ~buf:buff ~pos:0 ~len;
      Out_channel.output_string oc eol;
      output_lines_loop ~quote ~sep ~buff ~eol oc t
  ;;

  let output_lines ?(quote = '"') ?(sep = ',') ?(eol = `Dos) oc l =
    let eol =
      match eol with
      | `Dos -> "\r\n"
      | `Unix -> "\n"
    in
    output_lines_loop ~quote ~sep ~buff:(Bytes.create 256) ~eol oc l
  ;;
end
