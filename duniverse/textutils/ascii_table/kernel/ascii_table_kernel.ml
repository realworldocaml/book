open Core_kernel
include Ascii_table_kernel_intf

let list_sum ~f lst = List.fold lst ~init:0 ~f:(fun a b -> a + f b)
let list_max ~f lst = List.fold lst ~init:0 ~f:(fun a b -> max a (f b))

module El = struct
  (* One element in the table. *)
  type t = Attr.t list * string list
  type row = t list
  type grid = row list

  let create attr str = attr, String.split_lines str
  let width (_, lines) = list_max ~f:String.length lines

  let height width (_, lines) ~display_empty_rows =
    let height =
      list_sum lines ~f:(fun s -> max ((String.length s + (width - 1)) / max width 1) 1)
    in
    if display_empty_rows then max height 1 else height
  ;;

  let rec slices width lines =
    match lines with
    | [] -> []
    | line :: lines -> slices_split width lines line (String.length line) 0

  and slices_split width lines line line_len pos =
    let chunk_len = min width (line_len - pos) in
    let completely_fits = Int.( = ) chunk_len (line_len - pos) in
    let chunk = String.sub line ~pos ~len:chunk_len in
    if completely_fits
    then chunk :: slices width lines
    else chunk :: slices_split width lines line line_len (pos + width)
  ;;
end

type show =
  [ `Yes
  | `No
  | `If_not_empty
  ]

module Column = struct
  type 'a t =
    { max_width : int
    ; header : string
    ; col_func : 'a -> El.t
    ; align : Align.t
    ; min_width : int option
    ; show : show
    }

  let header t = t.header
  let to_data t = t.col_func

  type constraints =
    { total_width : int
    ; min_widths : (string * int) list
    }
  [@@deriving sexp]

  exception Impossible_table_constraints of constraints [@@deriving sexp]

  let create_attr
        ?(align = Align.Left)
        ?min_width
        ?(max_width = 90)
        ?(show = `Yes)
        str
        parse_func
    =
    { max_width
    ; header = str
    ; col_func =
        (fun x ->
           match parse_func x with
           | a, b -> El.create a b)
    ; align
    ; (* We add one for the '|' on the left. *)
      min_width = Option.map min_width ~f:(( + ) 1)
    ; show
    }
  ;;

  let create ?(align = Align.Left) ?min_width ?(max_width = 90) ?show str parse_func =
    create_attr ?min_width ~align ~max_width ?show str (fun x -> [], parse_func x)
  ;;

  let header_to_el alst t = El.create alst t.header
  let make col_val t = t.col_func col_val

  let desired_width ~spacing data t =
    let column_data = List.map data ~f:t.col_func in
    let header_width = String.split t.header ~on:'\n' |> list_max ~f:String.length in
    (* We need to account for the '|' to the left, so we add 1 plus the spacing
       on either side. *)
    1
    + (2 * spacing)
    + min
        (t.max_width - (2 * spacing))
        (max header_width (list_max column_data ~f:El.width))
  ;;

  let layout ~spacing table_width ts data =
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
                 Option.map t.min_width ~f:(fun num_chars -> t.header, num_chars))
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
end

module Table_char = struct
  type t =
    { ascii : char
    ; utf8 : string
    }

  let connect ?top ?bottom ?left ?right () =
    let top, bottom, left, right =
      is_some top, is_some bottom, is_some left, is_some right
    in
    let ascii, utf8 =
      match top, bottom, left, right with
      | false, false, true, true -> '-', "\226\148\128"
      | true, true, false, false -> '|', "\226\148\130"
      | false, true, false, true -> '|', "\226\148\140"
      | false, true, true, false -> '|', "\226\148\144"
      | true, false, false, true -> '|', "\226\148\148"
      | true, false, true, false -> '|', "\226\148\152"
      | true, true, false, true -> '|', "\226\148\156"
      | true, true, true, false -> '|', "\226\148\164"
      | false, true, true, true -> '-', "\226\148\172"
      | true, false, true, true -> '-', "\226\148\180"
      | true, true, true, true -> '+', "\226\148\188"
      | false, false, true, false -> '-', "\226\149\180"
      | true, false, false, false -> '|', "\226\149\181"
      | false, false, false, true -> '-', "\226\149\182"
      | false, true, false, false -> '|', "\226\149\183"
      | false, false, false, false -> ' ', " "
    in
    { ascii; utf8 }
  ;;
end

module Screen = struct
  type point =
    | Line
    | Blank
    | Char of Attr.t list * char
  [@@deriving compare, sexp_of]

  type t =
    { data : point array array
    ; rows : int
    ; cols : int
    }
  [@@deriving compare, sexp_of]

  let create ~rows ~cols =
    let data = Array.make_matrix ~dimx:rows ~dimy:cols (Char ([], ' ')) in
    { data; rows; cols }
  ;;

  let set_screen_point t ~row ~col ~point =
    let c = t.data.(row).(col) in
    let new_point =
      match c, point with
      | Blank, _ -> Blank
      | _, Blank -> Blank
      | _, point -> point
    in
    t.data.(row).(col) <- new_point
  ;;

  let hline t ~row ~col1 ~col2 ?(point = Line) () =
    for col = col1 to col2 do
      set_screen_point t ~row ~col ~point
    done
  ;;

  let vline t ~col ~row1 ~row2 ?(point = Line) () =
    for row = row1 to row2 do
      set_screen_point t ~row ~col ~point
    done
  ;;

  let char t ~row ~col ~char ~attr = t.data.(row).(col) <- Char (attr, char)

  let string t ~row ~col ~string ~attr =
    for i = 0 to String.length string - 1 do
      char t ~row ~col:(col + i) ~char:string.[i] ~attr
    done
  ;;

  let aligned t ~row ~col ~string:str ~attr ~width ~align =
    let col =
      match align with
      | Align.Left -> col
      | Align.Right -> col + width - String.length str
      | Align.Center -> col + (max 0 (width - String.length str) / 2)
    in
    string t ~row ~col ~string:str ~attr
  ;;

  let get_symbol t ~row ~col =
    let top = row > 0 && [%compare.equal: point] t.data.(row - 1).(col) Line in
    let bottom =
      row < t.rows - 1 && [%compare.equal: point] t.data.(row + 1).(col) Line
    in
    let left = col > 0 && [%compare.equal: point] t.data.(row).(col - 1) Line in
    let right =
      col < t.cols - 1 && [%compare.equal: point] t.data.(row).(col + 1) Line
    in
    Table_char.connect
      ?top:(Option.some_if top ())
      ?bottom:(Option.some_if bottom ())
      ?left:(Option.some_if left ())
      ?right:(Option.some_if right ())
      ()
  ;;

  let render t ~bars ~output ~close =
    let buf = Buffer.create 1024 in
    let current_attr = ref [] in
    let update_attr attr =
      let attr = List.sort ~compare:[%compare: Attr.t] attr in
      if not ([%compare.equal: Attr.t list] attr !current_attr)
      then (
        if Buffer.length buf > 0 then output !current_attr buf;
        current_attr := attr)
    in
    for row = 0 to t.rows - 1 do
      for col = 0 to t.cols - 1 do
        match t.data.(row).(col) with
        | Char (attr, ch) ->
          update_attr attr;
          Buffer.add_char buf ch
        | Blank -> Buffer.add_char buf ' '
        | Line ->
          update_attr [];
          let { Table_char.ascii; utf8 } = get_symbol t ~row ~col in
          (match bars with
           | `Ascii -> Buffer.add_char buf ascii
           | `Unicode -> Buffer.add_string buf utf8)
      done;
      update_attr [];
      Buffer.add_char buf '\n'
    done;
    output !current_attr buf;
    close buf
  ;;

  let to_string t ~bars ~string_with_attr =
    let buf = Buffer.create 1024 in
    render
      t
      ~bars
      ~output:(fun attr buf' ->
        Buffer.add_string buf (string_with_attr attr (Buffer.contents buf'));
        Buffer.clear buf')
      ~close:(fun _ -> Buffer.contents buf)
  ;;
end

module Display = struct
  type t =
    | Short_box
    | Tall_box
    | Line
    | Blank
    | Column_titles
  [@@deriving compare, sexp_of]

  let short_box = Short_box
  let tall_box = Tall_box
  let line = Line
  let blank = Blank
  let column_titles = Column_titles
end

module Grid = struct
  type t =
    { data : El.grid
    ; heights : int list
    ; widths : int list
    ; aligns : Align.t list
    }

  let create ~spacing ~display max_width h_attr cols raw_data ~display_empty_rows =
    let body = List.map raw_data ~f:(fun x -> List.map cols ~f:(Column.make x)) in
    let empty =
      List.fold
        body
        ~init:(List.map cols ~f:(fun _ -> true))
        ~f:
          (List.map2_exn ~f:(fun is_empty (_attr, lines) ->
             is_empty && List.for_all lines ~f:(String.equal "")))
    in
    let keep =
      List.map2_exn cols empty ~f:(fun { Column.show; _ } is_empty ->
        match show with
        | `Yes -> true
        | `No -> false
        | `If_not_empty -> not is_empty)
    in
    let filter l = List.filter_opt (List.map2_exn keep l ~f:Option.some_if) in
    let cols = filter cols in
    let body = List.map body ~f:filter in
    (* We subtract 1 from max_width because later we're going to add a line of
       '|'s to form the right wall of the table. *)
    let widths = Column.layout ~spacing (max_width - 1) cols raw_data in
    let grid_data = List.map cols ~f:(Column.header_to_el h_attr) :: body in
    let heights =
      if [%compare.equal: Display.t] display Line
      then List.map grid_data ~f:(fun _ -> 1)
      else
        List.map grid_data ~f:(fun row ->
          assert (List.length widths = List.length row);
          list_max
            ~f:Fn.id
            (List.map2_exn widths row ~f:(El.height ~display_empty_rows)))
    in
    let aligns = List.map cols ~f:(fun c -> c.Column.align) in
    { data = grid_data; heights; widths; aligns }
  ;;

  let draw ~spacing ~display t =
    assert (List.length t.data = List.length t.heights);
    let mid_row = if [%compare.equal: Display.t] display Tall_box then 1 else 0 in
    (* The total width of the table includes the '|'s to the left of elements, so we add 1
       and the spacing on either side when summing. *)
    let cols = list_sum t.widths ~f:(( + ) (1 + (spacing * 2))) + 1 in
    let rows = list_sum t.heights ~f:(( + ) mid_row) + 3 - (2 * mid_row) in
    let screen = Screen.create ~rows ~cols in
    let point =
      if [%compare.equal: Display.t] display Column_titles
      then Screen.Blank
      else Screen.Line
    in
    Screen.hline screen ~row:0 ~col1:0 ~col2:(cols - 1) ~point ();
    Screen.hline screen ~row:(rows - 1) ~col1:0 ~col2:(cols - 1) ~point ();
    if not ([%compare.equal: Display.t] display Blank)
    then (
      Screen.vline screen ~col:0 ~row1:0 ~row2:(rows - 1) ~point ();
      ignore
        (List.fold t.widths ~init:0 ~f:(fun col width ->
           let col = col + 1 + width + (spacing * 2) in
           Screen.vline screen ~col ~row1:0 ~row2:(rows - 1) ~point ();
           col)
         : int));
    ignore
      (List.fold2_exn t.data t.heights ~init:1 ~f:(fun row row_elements height ->
         let header_row = row = 1 in
         ignore
           (List.fold2_exn
              row_elements
              (List.zip_exn t.widths t.aligns)
              ~init:(1 + spacing)
              ~f:(fun col (attr, lines) (width, align) ->
                let strings = El.slices width lines in
                if [%compare.equal: Display.t] display Line
                then (
                  match strings with
                  | [] -> ()
                  | [ string ] ->
                    Screen.aligned screen ~row ~col ~attr ~string ~align ~width
                  | string :: _ ->
                    Screen.aligned screen ~row ~col ~attr ~string ~align ~width;
                    for col = col + max 0 (width - 3) to col + width - 1 do
                      Screen.char screen ~row ~col ~char:'.' ~attr:[]
                    done)
                else
                  ignore
                    (List.fold strings ~init:row ~f:(fun row string ->
                       Screen.aligned screen ~row ~col ~attr ~string ~align ~width;
                       row + 1)
                     : int);
                col + 1 + (spacing * 2) + width)
            : int);
         let row = row + height in
         if [%compare.equal: Display.t] display Tall_box || header_row
         then (
           if not ([%compare.equal: Display.t] display Blank)
           then Screen.hline screen ~row ~col1:0 ~col2:(cols - 1) ();
           row + 1)
         else row)
       : int);
    screen
  ;;
end

let draw
      ?(display = Display.short_box)
      ?(spacing = 1)
      ?(limit_width_to = 90)
      ?(header_attr = [])
      ?(display_empty_rows = false)
      cols
      data
  =
  match cols with
  | [] -> None
  | _ :: _ ->
    Some
      (Grid.create
         ~spacing
         ~display
         limit_width_to
         header_attr
         cols
         data
         ~display_empty_rows
       |> Grid.draw ~spacing ~display)
;;
