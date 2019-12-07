(** A hash-queue is a combination of a queue and a hashtable that
    supports constant-time lookup and removal of queue elements in addition to
    the usual queue operations (enqueue, dequeue). The queue elements are
    key-value pairs. The hashtable has one entry for each element of the queue.

    Calls to functions that would modify a hash-queue (e.g. [enqueue], [dequeue],
    [remove], [replace]) detect if a client is in the middle of iterating over the
    queue (e.g., [iter], [fold], [for_all], [exists]) and if so, raise an exception.
*)

open! Import
open Hash_queue_intf

module type Key = Key
module type S = S

module Make_with_table (Key : Key) (Table : Hashtbl_intf.S_plain with type key = Key.t) :
  S with module Key = Key

module Make (Key : Key) : S with module Key = Key
