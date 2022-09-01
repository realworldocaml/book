open! Core
open! Import
include Column_intf

type 'a t =
  { max_width : int
  ; header : Utf8_text.t
  ; col_func : 'a -> Cell.t
  ; align : Align.t
  ; min_width : int option
  ; show : Show.t
  }
[@@deriving fields, sexp_of]

let lift t ~f = { t with col_func = (fun x -> t.col_func (f x)) }
let header t = Utf8_text.to_string t.header

let to_data t a =
  let attr, lines = Cell.to_tuple (t.col_func a) in
  attr, List.map lines ~f:Utf8_text.to_string
;;

type constraints =
  { total_width : int
  ; min_widths : (Utf8_text.t * int) list
  }
[@@deriving sexp_of]

exception Impossible_table_constraints of constraints [@@deriving sexp_of]

let create_attr
      ?(align = Align.Left)
      ?min_width
      ?(max_width = 90)
      ?(show = `Yes)
      str
      parse_func
  =
  { max_width
  ; header = Utf8_text.of_string str
  ; col_func =
      (fun x ->
         match parse_func x with
         | a, b -> Cell.create a b)
  ; align
  ; (* We add one for the '|' on the left. *)
    min_width = Option.map min_width ~f:(( + ) 1)
  ; show
  }
;;

let create ?(align = Align.Left) ?min_width ?(max_width = 90) ?show str parse_func =
  create_attr ?min_width ~align ~max_width ?show str (fun x -> [], parse_func x)
;;

let to_cell t ~value = t.col_func value

let desired_width ~spacing data t =
  let column_data = List.map data ~f:t.col_func in
  let header_width = Utf8_text.split t.header ~on:'\n' |> list_max ~f:Utf8_text.width in
  (* We need to account for the '|' to the left, so we add 1 plus the spacing
     on either side. *)
  1
  + (2 * spacing)
  + min
      (t.max_width - (2 * spacing))
      (max header_width (list_max column_data ~f:Cell.width))
;;

let layout ts data ~spacing ~max_width:table_width =
  let desired_widths = List.map ts ~f:(desired_width ~spacing data) in
  let all_min_width = List.filter_map ts ~f:(fun t -> t.min_width) in
  (* [generic_min_chars] = minimum number of characters for a column that doesn't have
     an [min_width] value. *)
  let table_constraints_are_impossible, generic_min_chars =
    let columns_with_no_min_width = List.length ts - List.length all_min_width in
    if Int.( <> ) 0 columns_with_no_min_width (* need to avoid a divide-by-zero *)
    then (
      let width = table_width - list_sum all_min_width ~f:Fn.id in
      let generic_min_chars = width / columns_with_no_min_width in
      let impossible = generic_min_chars < 1 + (1 + (spacing * 2)) in
      impossible, generic_min_chars)
    else (
      let min_total = List.fold ~init:0 all_min_width ~f:Int.( + ) in
      let extra_per_col = 1 + 1 + (spacing * 2) in
      let impossible = table_width < min_total + (List.length ts * extra_per_col) in
      (* the zero is a nonsense value, but we only generate it when every column has a
         min width and therefore this zero will never be used. *)
      impossible, 0)
  in
  if table_constraints_are_impossible
  then
    raise
      (Impossible_table_constraints
         { total_width = table_width + 1
         ; min_widths =
             List.filter_map ts ~f:(fun t ->
               Option.map t.min_width ~f:(fun min_width -> t.header, min_width))
         });
  let left = ref (list_sum ~f:Fn.id desired_widths - table_width) in
  let stop = ref false in
  (* This layout algorithm looks unbearably inefficient, but it's
     simple and works reasonably well in the common case. *)
  let rec decide_widths desired_widths =
    if !stop
    then desired_widths
    else (
      stop := true;
      assert (List.length ts = List.length desired_widths);
      decide_widths
        (List.map2_exn ts desired_widths ~f:(fun t column_width ->
           let min_chars =
             match t.min_width with
             | Some x -> x
             | None -> generic_min_chars
           in
           if column_width <= min_chars || !left <= 0
           then column_width
           else (
             left := !left - 1;
             stop := false;
             column_width - 1))))
  in
  (* The widths used in [loop] include the '|' to the left of each element,
     which isn't important after layout, so we subtract off 1 and the spacing
     on either side. *)
  List.map ~f:(fun x -> x - (1 + (spacing * 2))) (decide_widths desired_widths)
;;

module Of_field = struct
  let field ?align ?min_width ?max_width ?show ?header to_string record_field =
    create
      ?align
      ?min_width
      ?max_width
      ?show
      (Option.value header ~default:(Field.name record_field))
      (fun record -> to_string (Field.get record_field record))
  ;;

  let field_attr
        ?align
        ?min_width
        ?max_width
        ?show
        ?header
        to_string_and_attr
        record_field
    =
    create_attr
      ?align
      ?min_width
      ?max_width
      ?show
      (Option.value header ~default:(Field.name record_field))
      (fun record -> to_string_and_attr (Field.get record_field record))
  ;;

  let field_opt ?align ?min_width ?max_width ?show ?header to_string record_field =
    field
      ?align
      ?min_width
      ?max_width
      ?show
      ?header
      (function
        | None -> ""
        | Some x -> to_string x)
      record_field
  ;;

  let field_opt_attr
        ?align
        ?min_width
        ?max_width
        ?show
        ?header
        to_string_and_attr
        record_field
    =
    field_attr
      ?align
      ?min_width
      ?max_width
      ?show
      ?header
      (function
        | None -> [], ""
        | Some x -> to_string_and_attr x)
      record_field
  ;;
end

module Private = struct
  let layout = layout
  let to_cell = to_cell
end
