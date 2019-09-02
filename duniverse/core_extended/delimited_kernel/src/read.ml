open Core_kernel
open! Int.Replace_polymorphic_compare

exception Bad_csv_formatting = Parse_state.Bad_csv_formatting

module On_invalid_row = struct
  type 'a t =
    int String.Map.t
    -> string Append_only_buffer.t
    -> exn
    -> [`Skip | `Yield of 'a | `Raise of exn]

  let raise _ _ exn = `Raise exn
  let skip _ _ _ = `Skip
  let create = Fn.id
end

module Header = Header

module Builder = struct
  type _ t =
    | Column : int -> string t
    | Header : string -> string t
    | Header_opt : string -> string option t
    | Return : 'a -> 'a t
    | Apply : ('b -> 'a) t * 'b t -> 'a t
    | Map : ('b -> 'a) * 'b t -> 'a t
    | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Lambda : (int String.Map.t -> string Append_only_buffer.t -> 'a) -> 'a t

  module T = struct
    let return x = Return x

    let apply f x =
      match f with
      | Return f -> Map (f, x)
      | Map (f, w) -> Map2 (f, w, x)
      | _ -> Apply (f, x)
    ;;

    let map x ~f =
      match x with
      | Return x -> Return (f x)
      | _ -> Map (f, x)
    ;;

    let ( >>| ) t f = map t ~f

    let map2 x y ~f =
      match x, y with
      | Return x, Return y -> Return (f x y)
      | _ -> Map2 (f, x, y)
    ;;

    let map3 x y z ~f =
      match x, y, z with
      | Return x, Return y, Return z -> Return (f x y z)
      | _ -> Apply (Map2 (f, x, y), z)
    ;;

    let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
    let all_unit ts = map ~f:ignore (all ts)
    let all_ignore = all_unit
    let both x y = Both (x, y)
    let ( <*> ) = apply
    let ( *> ) u v = return (fun () y -> y) <*> u <*> v
    let ( <* ) u v = return (fun x () -> x) <*> u <*> v

    module Applicative_infix = struct
      let ( <*> ) = ( <*> )
      let ( *> ) = ( *> )
      let ( <* ) = ( <* )
      let ( >>| ) = ( >>| )
    end
  end

  include T

  let at_index i ~f = Map (f, Column i)
  let at_header h ~f = Map (f, Header h)
  let at_header_opt h ~f = Map (f, Header_opt h)
  let lambda f = Lambda f

  module Let_syntax = struct
    module Let_syntax = struct
      include T

      module Open_on_rhs = struct
        let at_index = at_index
        let at_header = at_header
        let at_header_opt = at_header_opt
      end
    end
  end

  module Without_headers = struct
    type 'a t =
      | Column : int -> string t
      | Return : 'a -> 'a t
      | Apply : ('b -> 'a) t * 'b t -> 'a t
      | Map : ('b -> 'a) * 'b t -> 'a t
      | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
      | Both : ('a t * 'b t) -> ('a * 'b) t
      | Lambda :
          (int String.Map.t -> string Append_only_buffer.t -> 'a) * int String.Map.t
        -> 'a t

    let get_fields_used t =
      let open Option.Let_syntax in
      let rec fields : type a. a t -> Int.Set.t option = function
        | Return _ -> Some Int.Set.empty
        | Column i -> Some (Int.Set.singleton i)
        | Apply (f, x) ->
          let%bind f = fields f in
          let%map x = fields x in
          Set.union f x
        | Map (_, x) -> fields x
        | Map2 (_, x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Both (x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Lambda _ -> None
      in
      fields t
    ;;

    let build t =
      let rec build' : type a. a t -> string Append_only_buffer.t -> a =
        fun t row ->
          match t with
          | Return x -> x
          | Column i -> Append_only_buffer.nth_exn row i
          | Apply (f, x) -> (build' f row) (build' x row)
          | Map (f, x) -> f (build' x row)
          | Map2 (f, x, y) -> f (build' x row) (build' y row)
          | Both (x, y) -> build' x row, build' y row
          | Lambda (f, header_map) -> f header_map row
      in
      let fields_used = get_fields_used t in
      match fields_used with
      | None -> build' t, None
      | Some fields_used ->
        let fields_used = Set.to_list fields_used in
        let mapping =
          List.mapi fields_used ~f:(fun i field_index -> field_index, i)
          |> Int.Map.of_alist_exn
        in
        let rec remap : type a. a t -> a t =
          fun t ->
            match t with
            | Column i -> Column (Map.find_exn mapping i)
            | Return x -> Return x
            | Apply (f, x) -> Apply (remap f, remap x)
            | Map (f, x) -> Map (f, remap x)
            | Map2 (f, x, y) -> Map2 (f, remap x, remap y)
            | Both (x, y) -> Both (remap x, remap y)
            | Lambda _ -> t
        in
        build' (remap t), Some (Array.of_list fields_used)
    ;;
  end

  let build ~header_map t =
    let rec transform : type a. a t -> a Without_headers.t = function
      | Return x -> Without_headers.Return x
      | Column i -> Without_headers.Column i
      | Header h ->
        (match String.Map.find_exn header_map h with
         | index -> Without_headers.Column index
         | exception (Not_found_s _ | Caml.Not_found) ->
           raise_s
             [%message "Header not found" (h : string) (header_map : int String.Map.t)])
      | Header_opt h ->
        (match String.Map.find_exn header_map h with
         | index -> Without_headers.Map (Option.some, Column index)
         | exception (Not_found_s _ | Caml.Not_found) -> Without_headers.Return None)
      | Apply (f, x) -> Without_headers.Apply (transform f, transform x)
      | Map (f, x) -> Without_headers.Map (f, transform x)
      | Map2 (f, x, y) -> Without_headers.Map2 (f, transform x, transform y)
      | Both (x, y) -> Without_headers.Both (transform x, transform y)
      | Lambda f -> Without_headers.Lambda (f, header_map)
    in
    let transformed = transform t in
    Without_headers.build transformed
  ;;
end

include Builder

module Parse_header = struct
  (* This exception is used to return early from the parser, so we don't consume more
     input than necessary. This is almost [With_return], except declaring the exception at
     top-level so we can freely pass around a closure that raises it. *)
  exception Header_parsed of string array * int

  type t =
    { state : unit Parse_state.t
    ; transform : string array -> int String.Map.t
    }

  let is_at_beginning_of_row t = Parse_state.is_at_beginning_of_row t.state

  let header_map_opt header_row =
    Array.foldi header_row ~init:String.Map.empty ~f:(fun i map header ->
      match header with
      | None -> map
      | Some header -> Map.set map ~key:header ~data:i)
  ;;

  let header_map header_row = header_map_opt (Array.map ~f:Option.some header_row)

  let require_header required_headers' csv_headers' =
    let required_headers = String.Set.of_list required_headers' in
    let csv_headers = String.Set.of_array csv_headers' in
    let missing = Set.diff required_headers csv_headers in
    if not (Set.is_empty missing)
    then
      raise_s
        [%message
          "Header specified in `Require not present in csv document"
            (required_headers : String.Set.t)
            (csv_headers : String.Set.t)
            (missing : String.Set.t)];
    header_map csv_headers'
  ;;

  let create' ?strip ?sep ?quote transform =
    let f offset () row =
      raise (Header_parsed (Append_only_buffer.to_array row, offset))
    in
    { state = Parse_state.create ?strip ?sep ?quote ~fields_used:None ~init:() ~f ()
    ; transform
    }
  ;;

  let create ?strip ?sep ?quote ?(header = `No) () =
    match header with
    | `No -> Second String.Map.empty
    | `Add headers -> Second (header_map (Array.of_list headers))
    | `Yes ->
      let f headers = header_map headers in
      First (create' ?strip ?sep ?quote f)
    | `Require headers ->
      let f csv_headers = require_header headers csv_headers in
      First (create' ?strip ?sep ?quote f)
    | `Replace headers ->
      let f _ = header_map (Array.of_list headers) in
      First (create' ?strip ?sep ?quote f)
    | `Transform f ->
      let f headers = header_map (Array.of_list (f (Array.to_list headers))) in
      First (create' ?strip ?sep ?quote f)
    | `Filter_map f ->
      let f headers = header_map_opt (Array.of_list (f (Array.to_list headers))) in
      First (create' ?strip ?sep ?quote f)
  ;;

  let input_string t ~len input =
    try First { t with state = Parse_state.input_string t.state ~len input } with
    | Header_parsed (row, offset) ->
      Second (t.transform row, String.sub input ~pos:offset ~len:(len - offset))
  ;;

  let input t ~len input =
    try First { t with state = Parse_state.input t.state ~len input } with
    | Header_parsed (row, offset) ->
      Second (t.transform row, Bytes.To_string.sub input ~pos:offset ~len:(len - offset))
  ;;
end

module Expert = struct
  module Append_only_buffer = Append_only_buffer
  module Parse_state = Parse_state
  module Builder = Builder
  module Parse_header = Parse_header

  let create_parse_state
        ?strip
        ?sep
        ?quote
        ?(on_invalid_row = On_invalid_row.raise)
        ~header_map
        builder
        ~init
        ~f
    =
    let row_to_'a, fields_used = Builder.build ~header_map builder in
    let f _offset init row =
      try f init (row_to_'a row) with
      | exn ->
        (match on_invalid_row header_map row exn with
         | `Yield x -> f init x
         | `Skip -> init
         | `Raise exn -> raise exn)
    in
    Parse_state.create ?strip ?sep ?quote ~fields_used ~init ~f ()
  ;;

  let manual_parse_state ?strip ?sep ?quote header_map =
    Parse_state.create
      ?strip
      ?sep
      ?quote
      ~fields_used:None
      ~init:(Append_only_buffer.create ())
      ~f:(fun _ queue row ->
        Append_only_buffer.append queue (Row.Expert.of_buffer header_map row);
        queue)
      ()
  ;;

  let manual_parse_data parse_state input =
    let parse_state =
      match input with
      | `Eof -> Parse_state.finish parse_state
      | `Data s -> Parse_state.input_string parse_state s
    in
    let queue = Parse_state.acc parse_state in
    let result = Append_only_buffer.to_list queue in
    Append_only_buffer.lax_clear queue;
    Second parse_state, result
  ;;

  let manual_parse_header ?strip ?sep ?quote header_state input =
    let input =
      match input with
      | `Eof -> ""
      | `Data s -> s
    in
    match Parse_header.input_string header_state ~len:(String.length input) input with
    | First header_state -> First header_state, []
    | Second (header_map, input) ->
      let state = manual_parse_state ?strip ?sep ?quote header_map in
      manual_parse_data state (`Data input)
  ;;

  let create_partial ?strip ?sep ?quote ?header () =
    let state =
      Parse_header.create ?strip ?sep ?quote ?header ()
      |> Either.Second.map ~f:(manual_parse_state ?strip ?sep ?quote)
      |> ref
    in
    let parse_chunk input =
      let state', results =
        match !state with
        | First state -> manual_parse_header ?strip ?sep state input
        | Second state -> manual_parse_data state input
      in
      state := state';
      results
    in
    stage parse_chunk
  ;;
end

let fold_string ?strip ?sep ?quote ?header ?on_invalid_row builder ~init ~f csv_string =
  match
    match Parse_header.create ?strip ?sep ?quote ?header () with
    | Second header_map -> Some (header_map, csv_string)
    | First header_parse ->
      (match
         Parse_header.input_string
           header_parse
           ~len:(String.length csv_string)
           csv_string
       with
       | First _ ->
         if String.is_empty csv_string
         then None
         else
           raise_s
             [%message
               "String ended mid-header row"
                 (csv_string : string)
                 (sep : char option)
                 (header : Header.t option)]
       | Second (header_map, csv_string) -> Some (header_map, csv_string))
  with
  | None -> init
  | Some (header_map, csv_string) ->
    Parse_state.input_string
      (Expert.create_parse_state
         ?strip
         ?sep
         ?quote
         ?on_invalid_row
         ~header_map
         builder
         ~init
         ~f)
      csv_string
    |> Parse_state.finish
    |> Parse_state.acc
;;

let list_of_string ?strip ?sep ?quote ?header ?on_invalid_row builder csv_string =
  fold_string
    ?strip
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    csv_string
    ~init:(Append_only_buffer.create ())
    ~f:(fun queue row ->
      Append_only_buffer.append queue row;
      queue)
  |> Append_only_buffer.to_list
;;

module Row = struct
  type 'a builder_t = 'a t

  include Row

  let builder = lambda Row.Expert.of_buffer
end
