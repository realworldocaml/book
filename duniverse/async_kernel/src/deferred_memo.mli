(** Memoization functions like in [Core_kernel.Memo], with re-raising of exceptions
    thrown asynchronously.

    Also see [Lazy_deferred], of which [Deferred.Memo.unit] is a special case. *)

open! Core_kernel
open! Import
module Deferred = Deferred1

(** [general hashable f] returns a memoized version of [f], where the results are stored
    in a hash table indexed according to [hashable].  If [f a] asynchronously raises, then
    the error is stored in the hash table and is reraised when [a] is demanded.

    Unlike [Core_kernel.Memo.general], this [general] does not support
    [cache_size_bound] due to complexities of asynchrony -- even when one has a deferred
    return by the memoized function, there still may be asynchronous jobs working to
    determine it.

    Unlike [Core_kernel.Memo.general], this [general] takes a required [Hashable]
    module argument, to avoid unintentional use of polymorphic comparison. *)
val general
  :  (module Hashable.S_plain with type t = 'a)
  -> ('a -> 'b Deferred.t)
  -> ('a -> 'b Deferred.t) Staged.t

val unit : (unit -> 'a Deferred.t) -> (unit -> 'a Deferred.t) Staged.t
