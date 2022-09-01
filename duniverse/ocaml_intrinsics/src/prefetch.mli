(**  Bring the line of data from memory that contains the specified address.

     The instruction is a hint that is ignored on some targets.
     Some values or [temporal_locality] and [operation] hints are not
     supported on some targets.
*)

(** [temporal locality] is a hint to a location in the cache hierarchy
    where the prefetch data should be placed.

    Higher temporal locality hint means prefetching data closer to the CPU.
    For example, on Intel targets, [High] means prefetch to all levels of cache,
    and Moderate means prefetch to L2 cache and higher, but not L1 cache. *)
type temporal_locality =
  | None
  | Low
  | Moderate
  | High

(** Anticipated operation that the data will used for.
    In preparation for write, the data can be brought into cache
    in exclusive state, whereas for read, shared state is sufficient. *)
type operation =
  | Read
  | Write

val native_pointer
  :  Native_pointer.t
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

val ext_pointer
  :  Ext_pointer.t
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

val bigstring
  :  ( char
     , Stdlib.Bigarray.int8_unsigned_elt
     , Stdlib.Bigarray.c_layout )
       Stdlib.Bigarray.Array1.t
  -> pos:int
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

(** Processor hint that improves performance of spin-wait loops.  *)
external pause : unit -> unit = "caml_pause_hint"
[@@noalloc] [@@builtin]
