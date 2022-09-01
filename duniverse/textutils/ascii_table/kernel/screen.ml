open! Core
open! Import
include Screen_intf

type t =
  { data : Texel.t array array
  ; rows : int
  ; cols : int
  }
[@@deriving compare, sexp_of]

let create ~rows ~cols =
  let data =
    Array.make_matrix ~dimx:rows ~dimy:cols (Char ([], Uchar.of_char ' ') : Texel.t)
  in
  { data; rows; cols }
;;

let set_screen_point t (texel : Texel.t) ~row ~col =
  let prev = t.data.(row).(col) in
  t.data.(row).(col)
  <- (match prev, texel with
    | Blank, _ -> Blank
    | _, Blank -> Blank
    | _, point -> point)
;;

let hline t texel ~row =
  for col = 0 to t.cols - 1 do
    set_screen_point t texel ~row ~col
  done
;;

let vline t texel ~col =
  for row = 0 to t.rows - 1 do
    set_screen_point t texel ~row ~col
  done
;;

let char t attr char ~row ~col = t.data.(row).(col) <- Char (attr, char)

let string t align attr text ~row ~col ~width =
  let col =
    match (align : Column.Align.t) with
    | Left -> col
    | Right -> col + width - Utf8_text.width text
    | Center -> col + (max 0 (width - Utf8_text.width text) / 2)
  in
  Utf8_text.iteri text ~f:(fun i uchar -> char t attr uchar ~row ~col:(col + i))
;;

let get_symbol t ~row ~col =
  let top = row > 0 && [%compare.equal: Texel.t] t.data.(row - 1).(col) Line in
  let bottom =
    row < t.rows - 1 && [%compare.equal: Texel.t] t.data.(row + 1).(col) Line
  in
  let left = col > 0 && [%compare.equal: Texel.t] t.data.(row).(col - 1) Line in
  let right = col < t.cols - 1 && [%compare.equal: Texel.t] t.data.(row).(col + 1) Line in
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
      | Char (attr, uchar) ->
        update_attr attr;
        Uutf.Buffer.add_utf_8 buf uchar
      | Blank -> Buffer.add_char buf ' '
      | Line ->
        update_attr [];
        Table_char.to_buffer (get_symbol t ~row ~col) buf bars
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
