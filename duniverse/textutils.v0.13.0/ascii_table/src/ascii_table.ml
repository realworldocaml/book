open Core
open Poly

module Color = struct
  type t = Console.Ansi.color
end

module Attr = struct
  type t = Console.Ansi.attr
end

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

module Align = struct
  type t =
    | Left
    | Right
    | Center
end

type show =
  [ `Yes
  | `No
  | `If_not_empty
  ]

module Column = struct
  type 'a t =
    { max_text_width : int
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
    { max_text_width = max_width + 1
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
        (t.max_text_width - (1 + (2 * spacing)))
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

module Draw = struct
  type point =
    | Line
    | Blank
    | Char of Attr.t list * char

  type screen =
    { data : point array array
    ; rows : int
    ; cols : int
    }

  let create_screen ~rows ~cols =
    let data = Array.make_matrix ~dimx:rows ~dimy:cols (Char ([], ' ')) in
    { data; rows; cols }
  ;;

  let set_screen_point ~screen ~row ~col ~point =
    let c = screen.data.(row).(col) in
    let new_point =
      match c, point with
      | Blank, _ -> Blank
      | _, Blank -> Blank
      | _, point -> point
    in
    screen.data.(row).(col) <- new_point
  ;;

  let hline ~screen ~row ~col1 ~col2 ?(point = Line) () =
    for col = col1 to col2 do
      set_screen_point ~screen ~row ~col ~point
    done
  ;;

  let vline ~screen ~col ~row1 ~row2 ?(point = Line) () =
    for row = row1 to row2 do
      set_screen_point ~screen ~row ~col ~point
    done
  ;;

  let char ~screen ~row ~col ~char ~attr = screen.data.(row).(col) <- Char (attr, char)

  let string ~screen ~row ~col ~string ~attr =
    for i = 0 to String.length string - 1 do
      char ~screen ~row ~col:(col + i) ~char:string.[i] ~attr
    done
  ;;

  let aligned ~screen ~row ~col ~string:str ~attr ~width ~align =
    let col =
      match align with
      | Align.Left -> col
      | Align.Right -> col + width - String.length str
      | Align.Center -> col + (max 0 (width - String.length str) / 2)
    in
    string ~screen ~row ~col ~string:str ~attr
  ;;

  let get_symbol ~screen ~row ~col =
    let top = row > 0 && screen.data.(row - 1).(col) = Line in
    let bottom = row < screen.rows - 1 && screen.data.(row + 1).(col) = Line in
    let left = col > 0 && screen.data.(row).(col - 1) = Line in
    let right = col < screen.cols - 1 && screen.data.(row).(col + 1) = Line in
    Table_char.connect
      ?top:(Option.some_if top ())
      ?bottom:(Option.some_if bottom ())
      ?left:(Option.some_if left ())
      ?right:(Option.some_if right ())
      ()
  ;;

  let render ~screen ~bars ~output ~close =
    let buf = Buffer.create 1024 in
    let current_attr = ref [] in
    let update_attr attr =
      let attr = List.sort ~compare:Poly.compare attr in
      if attr <> !current_attr
      then (
        if Buffer.length buf > 0 then output !current_attr buf;
        current_attr := attr)
    in
    for row = 0 to screen.rows - 1 do
      for col = 0 to screen.cols - 1 do
        match screen.data.(row).(col) with
        | Char (attr, ch) ->
          update_attr attr;
          Buffer.add_char buf ch
        | Blank -> Buffer.add_char buf ' '
        | Line ->
          update_attr [];
          let { Table_char.ascii; utf8 } = get_symbol ~screen ~row ~col in
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

  let output ~oc ~screen ~bars =
    render ~screen ~bars ~close:ignore ~output:(fun attr buf ->
      Console.Ansi.output_string attr oc (Buffer.contents buf);
      Buffer.clear buf)
  ;;

  let to_string ~screen ~bars =
    let buf = Buffer.create 1024 in
    render
      ~screen
      ~bars
      ~output:(fun attr buf' ->
        Buffer.add_string buf (Console.Ansi.string_with_attr attr (Buffer.contents buf'));
        Buffer.clear buf')
      ~close:(fun _ -> Buffer.contents buf)
  ;;

  let to_string_noattr ~screen ~bars =
    render ~screen ~bars ~output:(fun _ _ -> ()) ~close:Buffer.contents
  ;;
end

module Display = struct
  type t =
    | Short_box
    | Tall_box
    | Line
    | Blank
    | Column_titles

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
      if display = Display.Line
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
    let mid_row = if display = Display.Tall_box then 1 else 0 in
    (* The total width of the table includes the '|'s to the left of elements, so we add 1
       and the spacing on either side when summing. *)
    let cols = list_sum t.widths ~f:(( + ) (1 + (spacing * 2))) + 1 in
    let rows = list_sum t.heights ~f:(( + ) mid_row) + 3 - (2 * mid_row) in
    let screen = Draw.create_screen ~rows ~cols in
    let point = if display = Display.Column_titles then Draw.Blank else Draw.Line in
    Draw.hline ~screen ~row:0 ~col1:0 ~col2:(cols - 1) ~point ();
    Draw.hline ~screen ~row:(rows - 1) ~col1:0 ~col2:(cols - 1) ~point ();
    if display <> Display.Blank
    then (
      Draw.vline ~screen ~col:0 ~row1:0 ~row2:(rows - 1) ~point ();
      ignore
        (List.fold t.widths ~init:0 ~f:(fun col width ->
           let col = col + 1 + width + (spacing * 2) in
           Draw.vline ~screen ~col ~row1:0 ~row2:(rows - 1) ~point ();
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
                if display = Display.Line
                then (
                  match strings with
                  | [] -> ()
                  | [ string ] ->
                    Draw.aligned ~screen ~row ~col ~attr ~string ~align ~width
                  | string :: _ ->
                    Draw.aligned ~screen ~row ~col ~attr ~string ~align ~width;
                    for col = col + max 0 (width - 3) to col + width - 1 do
                      Draw.char ~screen ~row ~col ~char:'.' ~attr:[]
                    done)
                else
                  ignore
                    (List.fold strings ~init:row ~f:(fun row string ->
                       Draw.aligned ~screen ~row ~col ~attr ~string ~align ~width;
                       row + 1)
                     : int);
                col + 1 + (spacing * 2) + width)
            : int);
         let row = row + height in
         if display = Display.Tall_box || header_row
         then (
           if display <> Display.Blank
           then Draw.hline ~screen ~row ~col1:0 ~col2:(cols - 1) ();
           row + 1)
         else row)
       : int);
    screen
  ;;
end

type ('a, 'rest) renderer =
  ?display:Display.t (* Default: short_box *)
  -> ?spacing:int (* Default: 1 *)
  -> ?limit_width_to:int (* defaults to 90 characters *)
  -> ?header_attr:Attr.t list
  -> ?bars:[ `Ascii | `Unicode ]
  -> ?display_empty_rows:bool (* Default: false *)
  -> 'a Column.t list
  -> 'a list
  -> 'rest

let output
      ?(display = Display.short_box)
      ?(spacing = 1)
      ?(limit_width_to = 90)
      ?(header_attr = [])
      ?(bars = `Unicode)
      ?(display_empty_rows = false)
      cols
      data
      ~oc
  =
  if cols = []
  then ()
  else (
    let screen =
      Grid.create
        ~spacing
        ~display
        limit_width_to
        header_attr
        cols
        data
        ~display_empty_rows
      |> Grid.draw ~spacing ~display
    in
    Draw.output ~oc ~screen ~bars)
;;

let to_string_gen
      ?(display = Display.short_box)
      ?(spacing = 1)
      ?(limit_width_to = 90)
      ?(header_attr = [])
      ?(bars = `Unicode)
      ?(display_empty_rows = false)
      cols
      data
      ~use_attr
  =
  if cols = []
  then ""
  else (
    let screen =
      Grid.create
        ~spacing
        ~display
        limit_width_to
        header_attr
        cols
        data
        ~display_empty_rows
      |> Grid.draw ~spacing ~display
    in
    if use_attr
    then Draw.to_string ~screen ~bars
    else Draw.to_string_noattr ~screen ~bars)
;;

let to_string_noattr = to_string_gen ~use_attr:false
let to_string = to_string_gen ~use_attr:true

let simple_list_table
      ?(index = false)
      ?(limit_width_to = 160)
      ?(oc = stdout)
      ?(display = Display.line)
      cols
      data
  =
  let cols, data =
    if index
    then "#" :: cols, List.mapi data ~f:(fun i row -> Int.to_string (i + 1) :: row)
    else cols, data
  in
  let cols =
    List.mapi cols ~f:(fun i col ->
      let col, align =
        match String.chop_prefix col ~prefix:"-" with
        | None -> col, Align.Right
        | Some col -> col, Align.Left
      in
      Column.create col (fun ls -> List.nth_exn ls i) ~align)
  in
  output ~oc ~display ~limit_width_to cols data
;;

let%test_module _ =
  (module struct
    let col1 = Column.create "a" (fun (x, _, _) -> x)
    let col2 = Column.create "b" (fun (_, x, _) -> x)
    let col3 = Column.create "c" (fun (_, _, x) -> x)

    let%expect_test _ =
      let stringify display =
        to_string
          ~bars:`Ascii
          ~display
          [ col1; col2; col3 ]
          [ "a1", "b1", "c1"; "a2", "b2", "c2" ]
      in
      printf "%s" (stringify Display.short_box);
      [%expect
        {|
        |--------------|
        | a  | b  | c  |
        |----+----+----|
        | a1 | b1 | c1 |
        | a2 | b2 | c2 |
        |--------------| |}];
      printf "%s" (stringify Display.blank);
      [%expect
        {|
        ----------------
          a    b    c

          a1   b1   c1
          a2   b2   c2
        ---------------- |}];
      printf "%s" (stringify Display.column_titles);
      [%expect
        {|
         a    b    c
        ---- ---- ----
         a1   b1   c1
         a2   b2   c2 |}]
    ;;

    let%expect_test "we keep empty lines if any" =
      let stringify display =
        to_string
          ~bars:`Ascii
          ~display
          [ col1; col2; col3 ]
          [ "a1", "b_line1\nb_line2\nb_line3", "c_line1\n\nc_line3"; "a2", "b2", "c2" ]
      in
      printf "%s" (stringify Display.short_box);
      [%expect
        {|
        |------------------------|
        | a  | b       | c       |
        |----+---------+---------|
        | a1 | b_line1 | c_line1 |
        |    | b_line2 |         |
        |    | b_line3 | c_line3 |
        | a2 | b2      | c2      |
        |------------------------| |}]
    ;;

    let%expect_test "trailing newline does not result in an empty line" =
      let stringify display =
        to_string ~bars:`Ascii ~display [ col1; col2; col3 ] [ "a\n", "b\n", "c\n" ]
      in
      printf "%s" (stringify Display.short_box);
      [%expect
        {|
        |-----------|
        | a | b | c |
        |---+---+---|
        | a | b | c |
        |-----------| |}]
    ;;

    let%expect_test _ =
      let stringify display =
        to_string
          ~bars:`Ascii
          ~display
          [ col1; col2; col3 ]
          [ "a", "b", "c\n\n\n\n\n\n\nc" ]
      in
      printf "%s" (stringify Display.short_box);
      [%expect
        {|
        |-----------|
        | a | b | c |
        |---+---+---|
        | a | b | c |
        |   |   |   |
        |   |   |   |
        |   |   |   |
        |   |   |   |
        |   |   |   |
        |   |   |   |
        |   |   | c |
        |-----------| |}]
    ;;

    let%expect_test _ =
      List.iter [ true; false ] ~f:(fun display_empty_rows ->
        printf "display empty rows = %b\n" display_empty_rows;
        to_string
          ~bars:`Ascii
          [ col1; col2; col3 ]
          ~display_empty_rows
          [ "a", "b", "c"; "", "", ""; "d", "e", "f" ]
        |> printf "%s\n");
      [%expect
        {|
        display empty rows = true
        |-----------|
        | a | b | c |
        |---+---+---|
        | a | b | c |
        |   |   |   |
        | d | e | f |
        |-----------|

        display empty rows = false
        |-----------|
        | a | b | c |
        |---+---+---|
        | a | b | c |
        | d | e | f |
        |-----------| |}]
    ;;

    (* test for bug where specifying minimum widths on all columns causes a
       Division_by_zero error while calculating generic_min_chars in Column.layout *)
    let%test _ =
      const true (to_string [ Column.create ~min_width:9 "foo" Fn.id ] [ "bar" ])
    ;;
  end)
;;
