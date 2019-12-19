(** {b RNG} seeding on {b Lwt/Unix}.

    This module provides RNG seeding from the Unix kernel RNG, typically
    {[/dev/urandom]}. It uses {!Lwt} for periodic background reseeding.

    Calling {{!initialize}initialize} is enough to bring the RNG into a working
    state. In addition, a background task is set up to periodically reseed the
    RNG.

    [initialize] is idempotent as long as the default generator is unchanged.
    It is harmless to call it several times.

    Note that [initialize] returns a thread. While the reseeding task has only
    been created once this thread completes, the initial seeding is done before
    the function returns. Is is safe to use the RNG immediately after the
    invocation.

    {1 Usage}

    Seed during module initialization, not waiting for the background seeding to
    start:
{[let _ = Nocrypto_entropy_lwt.initialize () ]}

    Seed just before the main function, not waiting for the background seeding
    to start:
{[let () =
  ignore (Nocrypto_entropy_lwt.initialize ());
  Lwt_main.run (main ()) ]}

    Seed just before the main function, and wait for the background seeding to
    start before proceeding:
{[let () =
  Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= main) ]}

*)


(** {1 Default generator initialization} *)

val initialize : unit -> unit Lwt.t
(** Immediately seeds the current defalt generator using
    {!Nocrypto_entropy_unix.initialize}. The initial seeding is finished before
    the function returns.

    It then invokes {{!attach}attach}. Once the returned thread completes, a
    background reseeding task has been attached to the defalt generator. *)

(** {1 Background seeding} *)

type t
(** Represents background reseeding task. *)

val attach : period:int -> ?device:string -> Nocrypto.Rng.g -> t Lwt.t
(** [attach ~period ~device g] instruments the lwt event loop to mix in bytes
    from [device] into [g] whenever external events cause the loop to wake up,
    but no more often than once every [period] seconds.

    [device] defaults to {!Nocrypto_entropy_unix.sys_rng}. *)

val stop : t -> unit Lwt.t
(** Stops the reseeding task associated with [t]. Idempotent. *)
