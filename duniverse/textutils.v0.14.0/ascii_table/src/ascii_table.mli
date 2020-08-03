open! Core

include module type of struct
  include Ascii_table_kernel
end

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

(** The idea is that you have a Column.t list and a list of rows, where each
    row contains the data for each column.  So e.g. 'a could be a record type
    {col_1 : int; col_2 : string}, where the first column pulls out col_1 and
    the second column pulls out col_2. **)
val output : (_, oc:Out_channel.t -> unit) renderer

val to_string_noattr : (_, string) renderer
val to_string : (_, string) renderer

val simple_list_table
  :  ?index:bool
  -> ?limit_width_to:int
  -> ?oc:Out_channel.t
  -> ?display:Display.t
  -> string list
  -> string list list
  -> unit
