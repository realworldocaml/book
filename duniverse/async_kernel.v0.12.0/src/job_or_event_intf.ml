(** [Job_or_event] is a custom zero-alloc sum type that is like:

    {[
      | Event of Event.t
      | Job   of Job.t
    ]}

    except that it uses the fact that [Event.t] is a pointer and [Job.t] is an
    int to be zero alloc. *)

open! Core_kernel
open! Import
module Event = Types.Event

module type Job_or_event = sig
  type t = Types.Job_or_event.t

  val of_event : Event.t -> t
  val of_job : Job.t -> t
  val is_event : t -> bool
  val is_job : t -> bool

  (** Idiomatic usage of [Match] is:

      {[
        let job_or_event = ... in
        let open Job_or_event.Match in
        let K k = kind job_or_event in
        match k, project k job_or_event with
        | Event , event -> ... use event ...
        | Job   , job   -> ... use job ...
      ]} *)
  module Match : sig
    type _ kind =
      | Event : Event.t kind
      | Job : Job.t kind

    type packed = K : _ kind -> packed

    val kind : t -> packed
    val project : 'a kind -> t -> 'a
  end
end
