open Core_kernel

(** Row up to the error, and the field with the error up to the point of failure *)
exception Bad_csv_formatting of string list * string

(** At the lowest level, we model csv parsing as a fold over string arrays, one array
    per row. It is up to you to interpret the header row. *)

type 'a t

(** At any moment, the result of folding over all complete rows seen so far. *)
val acc : 'a t -> 'a

(** Can be used to set or clear the current [acc] *)
val set_acc : 'a t -> 'a -> 'a t

val create
  :  ?strip:bool
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  (** Indices of the fields used. E.g., [~fields_used:(Some [| 0; 3; |])] means every
      row will be presented to [f] as having two fields, the first and fourth fields of
      the csv. This is for performance; pass [None] to store all fields.*)
  -> fields_used:int array option
  -> init:'a
  (** [f i init row] should take the previous accumulator [init] and the next complete
      row [row], and return the next accumulator.

      The index [i] is the zero-indexed position of the next unconsumed byte relative to
      the start of this chunk of input. *)
  -> f:(int -> 'a -> string Append_only_buffer.t -> 'a)
  -> unit
  -> 'a t

(** [input t ?pos ?len s] parses the first [len] characters of [s], starting at position
    [pos].  [pos] defaults to [0] and [len] defaults to reading up to the end of [s]. *)
val input : 'a t -> ?pos:int -> ?len:int -> Bytes.t -> 'a t

val input_string : 'a t -> ?pos:int -> ?len:int -> string -> 'a t

(** [finish t] forces an end-of-row. Raises if end-of-row is not permitted here (e.g.,
    within a quoted field). It is permitted to [input] after a [finish]. *)
val finish : 'a t -> 'a t

(** Returns true if the parser is at the beginning of a row *)
val is_at_beginning_of_row : _ t -> bool
