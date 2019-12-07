open! Core_kernel

type t

(** [get_conv_exn t header [%here] conv] extract the cell with column
    [header] from [row] and convert it using [conv].  If there is an
    error the error is raised including [row] [header] [error] and
    the source code position ([%here]). *)
val get_conv_exn : t -> string -> Source_code_position.t -> (string -> 'a) -> 'a

(** [get_exn t header] return the column of the row corresponding to header *)
val get_exn : t -> string -> string

(** [get_conv_opt_exn] is like [get_conv_exn], but empty strings are converted to
    [None].  Missing headers raise exceptions. *)
val get_conv_opt_exn
  :  t
  -> string
  -> Source_code_position.t
  -> (string -> 'a)
  -> 'a option

(** [get t header] same as get_exn, but returns an option when the header
    was not found *)
val get : t -> string -> string option

(** [get_opt_exn] is like [get_exn], but empty strings are converted to [None].  Missing
    headers raise exceptions. *)
val get_opt_exn : t -> string -> string option

(** [nth_exn t i] return the ith column of t (indexed from 0) *)
val nth_exn : t -> int -> string

(** [nth_conv_exn t i [%here] conv] extract the ith column of [t]
    and convert it using [conv].  If there is an
    error the error is raised including [row] [i] [error] and
    the source code position ([%here]). *)
val nth_conv_exn : t -> int -> Source_code_position.t -> (string -> 'a) -> 'a

(** [nth t i] same as nth_exn, but returns an option in the case where t does not have at
    least i - 1 columns *)
val nth : t -> int -> string option

(** [nth_conv] is like [nth_conv_exn], but returns [None] if there is an error.  *)
val nth_conv : t -> int -> (string -> 'a) -> 'a option

(** [to_list t] return all columns in the order they appear in the file *)
val to_list : t -> string list

(** [to_array t] return all columns in the order they appear in the file *)
val to_array : t -> string array

(** [length t] returns number of fields in this row *)
val length : t -> int

(** [headers t] return the header mapping (header -> position) available for the table
    this row is from.

    [list_of_headers] is a list in the same order as in input file.
*)
val headers : t -> int String.Map.t

val list_of_headers : t -> string list

(** [is_empty t] return true if the row contains only empty strings *)
val is_empty : t -> bool

val to_string : t -> string
val sexp_of_t : t -> Sexp.t
val fold : t -> init:'acc -> f:('acc -> header:string -> data:string -> 'acc) -> 'acc
val iter : t -> f:(header:string -> data:string -> unit) -> unit
val create : int String.Table.t -> string array -> t
val create' : int String.Map.t -> string array -> t
val equal : t -> t -> bool
val compare : t -> t -> int

module Expert : sig
  val of_buffer : int String.Map.t -> string Append_only_buffer.t -> t
end
