open! Core
open! Import
include Grid_intf

type t =
  { data : Cell.t list list
  ; heights : int list
  ; widths : int list
  ; aligns : Column.Align.t list
  ; spacing : int
  ; display : Display.t
  }
[@@deriving sexp_of]

let create
      cols
      raw_data
      ~display
      ~display_empty_rows
      ~header_attr:h_attr
      ~max_width
      ~spacing
      ~prefer_split_on_spaces
  =
  let body =
    List.map raw_data ~f:(fun value -> List.map cols ~f:(Column.Private.to_cell ~value))
  in
  let empty =
    List.fold
      body
      ~init:(List.map cols ~f:(fun _ -> true))
      ~f:(List.map2_exn ~f:(fun is_empty element -> is_empty && Cell.is_empty element))
  in
  let keep =
    List.map2_exn cols empty ~f:(fun column is_empty ->
      match Column.show column with
      | `Yes -> true
      | `No -> false
      | `If_not_empty -> not is_empty)
  in
  let filter l = List.filter_opt (List.map2_exn keep l ~f:Option.some_if) in
  let cols = filter cols in
  let body = List.map body ~f:filter in
  (* We subtract 1 from max_width because later we're going to add a line of
     '|'s to form the right wall of the table. *)
  let widths = Column.Private.layout cols raw_data ~spacing ~max_width:(max_width - 1) in
  let grid_data =
    List.map cols ~f:(fun column -> Cell.create h_attr (Column.header column)) :: body
  in
  let heights =
    if [%compare.equal: Display.t] display Line
    then List.map grid_data ~f:(fun _ -> 1)
    else
      List.map grid_data ~f:(fun row ->
        assert (List.length widths = List.length row);
        List.map2_exn widths row ~f:(fun width element ->
          Cell.height element ~display_empty_rows ~width ~prefer_split_on_spaces)
        |> list_max ~f:Fn.id)
  in
  let aligns = List.map cols ~f:Column.align in
  { data = grid_data; heights; widths; aligns; spacing; display }
;;

let to_screen t ~prefer_split_on_spaces =
  assert (List.length t.data = List.length t.heights);
  let mid_row = if [%compare.equal: Display.t] t.display Tall_box then 1 else 0 in
  (* The total width of the table includes the '|'s to the left of elements, so we add 1
     and the spacing on either side when summing. *)
  let cols = list_sum t.widths ~f:(( + ) (1 + (t.spacing * 2))) + 1 in
  let rows = list_sum t.heights ~f:(( + ) mid_row) + 3 - (2 * mid_row) in
  let screen = Screen.create ~rows ~cols in
  let texel : Screen.Texel.t =
    if [%compare.equal: Display.t] t.display Column_titles then Blank else Line
  in
  Screen.hline screen texel ~row:0;
  Screen.hline screen texel ~row:(rows - 1);
  if not ([%compare.equal: Display.t] t.display Blank)
  then (
    Screen.vline screen texel ~col:0;
    ignore
      (List.fold t.widths ~init:0 ~f:(fun col width ->
         let col = col + 1 + width + (t.spacing * 2) in
         Screen.vline screen texel ~col;
         col)
       : int));
  ignore
    (List.fold2_exn t.data t.heights ~init:1 ~f:(fun row row_elements height ->
       let header_row = row = 1 in
       ignore
         (List.fold2_exn
            row_elements
            (List.zip_exn t.widths t.aligns)
            ~init:(1 + t.spacing)
            ~f:(fun col element (width, align) ->
              let lines = Cell.wrap element ~width ~prefer_split_on_spaces in
              let attr = Cell.attr element in
              if [%compare.equal: Display.t] t.display Line
              then (
                match lines with
                | [] -> ()
                | [ line ] -> Screen.string screen align attr line ~row ~col ~width
                | line :: _ ->
                  Screen.string screen align attr line ~row ~col ~width;
                  for col = col + max 0 (width - 3) to col + width - 1 do
                    Screen.char screen [] (Uchar.of_char '.') ~row ~col
                  done)
              else
                ignore
                  (List.fold lines ~init:row ~f:(fun row line ->
                     Screen.string screen align attr line ~row ~col ~width;
                     row + 1)
                   : int);
              col + 1 + (t.spacing * 2) + width)
          : int);
       let row = row + height in
       if [%compare.equal: Display.t] t.display Tall_box || header_row
       then (
         if not ([%compare.equal: Display.t] t.display Blank)
         then Screen.hline screen Line ~row;
         row + 1)
       else row)
     : int);
  screen
;;
