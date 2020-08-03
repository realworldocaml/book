open! Core_kernel

include Monad.S2 with type ('a, 'b) t = ('a, 'b) Result.t Deferred1.t (** @open *)

val fail : 'err -> (_, 'err) t

(** e.g., [failf "Couldn't find bloogle %s" (Bloogle.to_string b)]. *)
val failf : ('a, unit, string, (_, string) t) format4 -> 'a

val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t

(** [combine] waits on both inputs and combines their results using [Result.combine]. *)
val combine
  :  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok:('ok1 -> 'ok2 -> 'ok3)
  -> err:('err -> 'err -> 'err)
  -> ('ok3, 'err) t
