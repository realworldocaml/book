(** [Read_write] is like [Dirpair], except "buy/sell" has been changed to "read/write". *)

open! Core

module Key : sig
  type t =
    [ `Read
    | `Write
    ]
  [@@deriving sexp]

  val flip : t -> t
end

type ('a, -'z) any = private
  { mutable read : 'a
  ; mutable write : 'a
  }
[@@deriving sexp]

module Immutable : sig
  type 'a t = ('a, immutable) any [@@deriving sexp]
end

module Read_only : sig
  type 'a t = ('a, read) any [@@deriving sexp]
end

module Mutable : sig
  type 'a t = ('a, read_write) any [@@deriving sexp]
end

type 'a t = 'a Immutable.t [@@deriving sexp]

(** {6 creation} *)

val create : read:'a -> write:'a -> ('a, [< _ perms ]) any
val createi : (Key.t -> 'a) -> ('a, [< _ perms ]) any
val create_both : 'a -> ('a, [< _ perms ]) any
val create_fn : (unit -> 'a) -> ('a, [< _ perms ]) any
val create_with : Key.t -> 'a -> zero:'a -> ('a, [< _ perms ]) any
val copy : ('a, [> read ]) any -> ('a, [< _ perms ]) any

(** {6 map-like functions} *)

val exists : ('a, [> read ]) any -> f:('a -> bool) -> bool
val for_all : ('a, [> read ]) any -> f:('a -> bool) -> bool
val iteri : ('a, [> read ]) any -> f:(Key.t -> 'a -> unit) -> unit
val iter : ('a, [> read ]) any -> f:('a -> unit) -> unit
val mapi : ('a, [> read ]) any -> f:(Key.t -> 'a -> 'b) -> ('b, [< _ perms ]) any
val map : ('a, [> read ]) any -> f:('a -> 'b) -> ('b, [< _ perms ]) any
val foldi : ('a, [> read ]) any -> 'b -> f:('b -> Key.t * 'a -> 'b) -> 'b
val fold : ('a, [> read ]) any -> 'b -> f:('b -> 'a -> 'b) -> 'b
val get : ('a, [> read ]) any -> Key.t -> 'a
val replace : 'a Mutable.t -> Key.t -> f:('a -> 'a) -> unit
val replace_all : 'a Mutable.t -> f:(Key.t -> 'a -> 'a) -> unit

(** {6 mutation} *)

val set : 'a Mutable.t -> Key.t -> 'a -> unit

module Export : sig
  type ('a, 'z) read_write_ = ('a, 'z) any = private
    { mutable read : 'a
    ; mutable write : 'a
    }
end
