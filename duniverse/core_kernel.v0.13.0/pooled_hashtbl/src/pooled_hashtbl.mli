(** A polymorphic hashtbl that uses {!Pool} to avoid allocation.

    This uses the standard linked-chain hashtable algorithm, albeit with links performed
    through a pool and hence avoiding [caml_modify] (for table manipulation), even when
    hashing object keys/values.

    This implementation is worth exploring for your application if profiling demonstrates
    that garbage collection and the [caml_modify] write barrier are a significant part of
    your execution time. *)

open! Core_kernel
open! Import
include Hashtbl_intf.Hashtbl

(** [resize t size] ensures that [t] can hold at least [size] entries without resizing
    (again), provided that [t] has growth enabled.  This is useful for sizing global
    tables during application initialization, to avoid subsequent, expensive growth
    online.  See {!Immediate.String.resize}, for example. *)
val resize : (_, _) t -> int -> unit

(** [on_grow ~before ~after] allows you to connect higher level loggers to the point where
    these hashtbls grow.  [before] is called before the table grows, and [after] after it.
    This permits you to e.g. measure the time elapsed between the two.

    This is only meant for debugging and profiling, e.g. note that once a callback is
    installed, there is no way to remove it. *)
val on_grow
  :  before:(unit -> 'a)
  -> after:('a -> old_capacity:int -> new_capacity:int -> unit)
  -> unit
