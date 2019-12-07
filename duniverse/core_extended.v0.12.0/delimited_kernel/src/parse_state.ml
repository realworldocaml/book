open Core_kernel
module Row_buffer = Append_only_buffer
open! Int.Replace_polymorphic_compare

exception Bad_csv_formatting of string list * string

module Step = struct
  type t =
    | Field_start
    | In_unquoted_field
    | In_quoted_field
    | In_quoted_field_after_quote
end

open Step

type 'a t =
  { acc : 'a
  ; sep : char
  ; quote : char
  ; use_quoting :
      bool
  ; lineno : int
  ; step : Step.t
  ; field : string
  ; current_row : string list
  ; emit_field : string Row_buffer.t -> Buffer.t -> unit
  ; f : int -> 'a -> string Row_buffer.t -> 'a
  ; fields_used : int array option
  ; current_field : int
  ; next_field_index : int
  }
[@@deriving fields]

let make_emit_field ~strip current_row field =
  Row_buffer.append
    current_row
    (if strip then Shared.strip_buffer field else Buffer.contents field);
  Buffer.clear field
;;

let emit_row f i acc current_row =
  let acc = f (i + 1) acc current_row in
  Row_buffer.lax_clear current_row;
  acc
;;

let set_acc t acc = { t with acc }

let create ?(strip = false) ?(sep = ',') ?(quote = `Using '"') ~fields_used ~init ~f () =
  let fields_used =
    match fields_used with
    | None -> None
    | Some fields_used as x
      when Array.is_sorted_strictly fields_used ~compare:Int.ascending -> x
    | Some fields_used ->
      Some
        (Array.of_list
           (List.dedup_and_sort (Array.to_list fields_used) ~compare:Int.ascending))
  in
  { acc = init
  ; sep
  ; quote =
      (match quote with
       | `Using char -> char
       | `No_quoting -> '"')
  ; use_quoting =
      (match quote with
       | `Using _ -> true
       | `No_quoting -> false)
  ; lineno = 1
  ; step = Field_start
  ; field = ""
  ; current_row = []
  ; emit_field = make_emit_field ~strip
  ; f
  ; fields_used
  ; current_field = 0
  ; next_field_index = 0
  }
;;

let is_at_beginning_of_row t =
  String.is_empty t.field
  && List.is_empty t.current_row
  &&
  match t.step with
  | Field_start -> true
  | In_unquoted_field | In_quoted_field | In_quoted_field_after_quote -> false
;;

let mutable_of_t t =
  let field = Buffer.create (String.length t.field) in
  Buffer.add_string field t.field;
  let current_row = Row_buffer.of_list t.current_row in
  field, current_row
;;

(* To reduce the number of allocations, we keep an array [fields_used] of the field
   indexes we care about. [current_field] is the position of the parser within the
   input row, and [next_field_index] is an index into the [fields_used] array
   indicating the next field that we need to store.

   If [fields_used] is None, we need to store every field.
*)
let should_enqueue fields_used current_field next_field_index =
  match fields_used with
  | None -> true
  | Some array ->
    next_field_index < Array.length array && array.(next_field_index) = current_field
;;

let input_aux ~get_length ~get t ?(pos = 0) ?len input =
  let field, current_row = mutable_of_t t in
  let enqueue = ref (should_enqueue t.fields_used t.current_field t.next_field_index) in
  let current_field = ref t.current_field in
  let next_field_index = ref t.next_field_index in
  let increment_field () =
    current_field := !current_field + 1;
    next_field_index := if !enqueue then !next_field_index + 1 else !next_field_index;
    enqueue := should_enqueue t.fields_used !current_field !next_field_index
  in
  let reset_field () =
    current_field := 0;
    next_field_index := 0;
    enqueue := should_enqueue t.fields_used !current_field !next_field_index
  in
  let loop_bound =
    match len with
    | Some i -> i + pos
    | None -> get_length input
  in
  let rec loop i t step =
    if i >= loop_bound
    then
      { t with step; current_field = !current_field; next_field_index = !next_field_index
      }
    else
      let open Char.Replace_polymorphic_compare in
      let continue = loop (i + 1) in
      let c = get input i in
      if c = '\r'
      then continue t step
      else (
        match step with
        | Field_start ->
          if c = t.quote && t.use_quoting
          then continue t In_quoted_field
          else if c = t.sep
          then (
            if !enqueue then t.emit_field current_row field;
            increment_field ();
            continue t Field_start)
          else if c = '\n'
          then (
            if !enqueue then t.emit_field current_row field;
            reset_field ();
            continue
              { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
              Field_start)
          else (
            if !enqueue then Buffer.add_char field c;
            continue t In_unquoted_field)
        | In_unquoted_field ->
          if c = t.sep
          then (
            if !enqueue then t.emit_field current_row field;
            increment_field ();
            continue t Field_start)
          else if c = '\n'
          then (
            if !enqueue then t.emit_field current_row field;
            reset_field ();
            continue
              { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
              Field_start)
          else (
            if !enqueue then Buffer.add_char field c;
            continue t step)
        | In_quoted_field ->
          if c = t.quote
          then continue t In_quoted_field_after_quote
          else (
            if !enqueue then Buffer.add_char field c;
            continue t step)
        | In_quoted_field_after_quote ->
          (* We must be using quoting to be in this state. *)
          if c = t.quote
          then (
            (* doubled quote *)
            if !enqueue then Buffer.add_char field t.quote;
            continue t In_quoted_field)
          else if c = '0'
          then (
            if !enqueue then Buffer.add_char field '\000';
            continue t In_quoted_field)
          else if c = t.sep
          then (
            if !enqueue then t.emit_field current_row field;
            increment_field ();
            continue t Field_start)
          else if c = '\n'
          then (
            if !enqueue then t.emit_field current_row field;
            reset_field ();
            continue
              { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
              Field_start)
          else if Char.is_whitespace c
          then continue t step
          else
            failwithf
              "In_quoted_field_after_quote looking at '%c' (lineno=%d)"
              c
              t.lineno
              ())
  in
  let t' = loop pos t t.step in
  { t' with
    field = Buffer.contents field
  ; current_row = Row_buffer.to_list current_row
  ; current_field = !current_field
  ; next_field_index = !next_field_index
  }
;;

let input t ?pos ?len input =
  input_aux ~get_length:Bytes.length ~get:Bytes.get t ?pos ?len input
;;

let input_string t ?pos ?len input =
  input_aux ~get_length:String.length ~get:String.get t ?pos ?len input
;;

let finish t =
  let field, current_row = mutable_of_t t in
  let enqueue = should_enqueue t.fields_used t.current_field t.next_field_index in
  let acc =
    match t.step with
    | Field_start ->
      if Row_buffer.length current_row <> 0
      then (
        if enqueue then t.emit_field current_row field;
        emit_row t.f 0 t.acc current_row)
      else t.acc
    | In_unquoted_field | In_quoted_field_after_quote ->
      if enqueue then t.emit_field current_row field;
      emit_row t.f 0 t.acc current_row
    | In_quoted_field ->
      raise (Bad_csv_formatting (Row_buffer.to_list current_row, Buffer.contents field))
  in
  { t with
    field = Buffer.contents field
  ; current_row = Row_buffer.to_list current_row
  ; current_field = 0
  ; next_field_index = 0
  ; acc
  }
;;
