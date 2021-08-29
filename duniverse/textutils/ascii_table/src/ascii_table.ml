open Core
include Ascii_table_kernel

let output_screen ~oc ~screen ~bars =
  Ascii_table_kernel.Screen.render screen ~bars ~close:ignore ~output:(fun attr buf ->
    Console.Ansi.output_string attr oc (Buffer.contents buf);
    Buffer.clear buf)
;;

type ('row, 'rest) renderer =
  ?display:Display.t (* Default: short_box *)
  -> ?spacing:int (* Default: 1 *)
  -> ?limit_width_to:int (* defaults to 90 characters *)
  -> ?header_attr:Attr.t list
  -> ?bars:[ `Ascii | `Unicode ] (* defaults to [`Unicode] *)
  -> ?display_empty_rows:bool (* Default: false *)
  -> 'row Column.t list
  -> 'row list
  -> 'rest

let output
      ?display
      ?spacing
      ?limit_width_to
      ?header_attr
      ?(bars = `Unicode)
      ?display_empty_rows
      cols
      data
      ~oc
  =
  Option.iter
    (Ascii_table_kernel.draw
       ?display
       ?spacing
       ?limit_width_to
       ?header_attr
       ?display_empty_rows
       cols
       data)
    ~f:(fun screen -> output_screen ~screen ~bars ~oc)
;;

let to_string_gen
      ?display
      ?spacing
      ?limit_width_to
      ?header_attr
      ?(bars = `Unicode)
      ?display_empty_rows
      cols
      data
      ~string_with_attr
  =
  match
    Ascii_table_kernel.draw
      ?display
      ?spacing
      ?limit_width_to
      ?header_attr
      ?display_empty_rows
      cols
      data
  with
  | None -> ""
  | Some screen -> Screen.to_string screen ~bars ~string_with_attr
;;

let to_string_noattr = to_string_gen ~string_with_attr:(fun _attrs str -> str)
let to_string = to_string_gen ~string_with_attr:Console.Ansi.string_with_attr

let simple_list_table
      ?(index = false)
      ?(limit_width_to = 160)
      ?(oc = stdout)
      ?(display = Ascii_table_kernel.Display.line)
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
        | None -> col, Ascii_table_kernel.Align.Right
        | Some col -> col, Ascii_table_kernel.Align.Left
      in
      Ascii_table_kernel.Column.create col (fun ls -> List.nth_exn ls i) ~align)
  in
  output ~oc ~display ~limit_width_to cols data
;;
