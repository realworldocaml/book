open Core.Std

(** [render_table header data] returns a string of characters formatted as a table,
    where [header] is the titles of each column, and [data] is a list of rows.  The
    [header] and each element of [data] must be of the same length.  Uses UNIX-style
    newline separators, i.e., `\n`
*)
val render_table
  :  string list         (* header *)
  -> string list list    (* data *)
  -> string

type 'a column
val column : string -> ('a -> string) -> 'a column

val column_render :
  'a column list -> 'a list -> string
