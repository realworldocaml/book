(** \[not for general use\] *)

(**/**)

type type_id
val dummy_type_id : type_id
val create_type_id : unit -> type_id

module Wr :
sig
  type tbl
  val create : int -> tbl
  val clear : tbl -> unit

  val put : tbl -> ('a * type_id) -> int -> int
    (** [put tbl x pos] returns 0 if [x] is not already in the table
	and adds [x] to the table.  [pos] is the absolute position
	of the first byte of the ref value excluding its tag.
	If [x] is found in the table, then the difference between
	[pos] and the original position is returned.
    *)
end

module Rd :
sig
  type tbl
  val create : int -> tbl
  val clear : tbl -> unit

  val put : tbl -> (int * type_id) -> Obj.t -> unit
    (** [put tbl pos x] puts the position of a new shared value into the
	table.  [pos] is the absolute position of the first byte
	of the ref value excluding its tag. *)

  val get : tbl -> (int * type_id) -> Obj.t
    (** [get tbl pos] returns the value stored at this position
	or raises a {!Bi_util.Error} exception. *)
end
