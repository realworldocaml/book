open! Core
open! Import
include Ascii_table_kernel_intf
module Align = Column.Align
module Attr = Attr
module Column = Column
module Table_char = Table_char

module Display = struct
  type t = Grid.Display.t =
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

module Screen = struct
  (* [Screen] is mostly private stuff, so we explicitly export the public bits instead of
     saying [Private] everywhere. *)

  type t = Screen.t

  let render = Screen.render
  let to_string = Screen.to_string
end

let draw
      ?(display = Display.short_box)
      ?(spacing = 1)
      ?(limit_width_to = 90)
      ?(header_attr = [])
      ?(display_empty_rows = false)
      ~prefer_split_on_spaces
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
         ~max_width:limit_width_to
         ~header_attr
         cols
         data
         ~display_empty_rows
         ~prefer_split_on_spaces
       |> Grid.to_screen ~prefer_split_on_spaces)
;;

let to_string_noattr
      ?display
      ?spacing
      ?limit_width_to
      ?display_empty_rows
      ?(prefer_split_on_spaces = false)
      cols
      data
      ~bars
  =
  draw
    ?display
    ?spacing
    ?limit_width_to
    ?display_empty_rows
    ~header_attr:[]
    cols
    data
    ~prefer_split_on_spaces
  |> Option.map ~f:(Screen.to_string ~bars ~string_with_attr:(fun _attr s -> s))
  |> Option.value ~default:""
;;
