open! Core_kernel
open! Import
module Event = Types.Event
module Job = Types.Job
include Types.Job_or_event

(* This redefinition of [Event] is here so the type checks are right next to
   [Obj.magic]s. *)
module Event_is_block : sig end = struct
  open Types
  open Event

  type _t = t =
    { (* must never be immediate *)
      mutable alarm : Job_or_event.t Timing_wheel.Alarm.t
    ; mutable at : Time_ns.t
    ; callback : unit -> unit
    ; execution_context : Execution_context.t
    ; mutable interval : Time_ns.Span.t option
    ; mutable next_fired : t
    ; mutable status : Status.t
    }
end

module Job_is_not_block : sig end = struct
  module Ensure_private_int (M : sig
      type t = private int
    end) =
  struct
    type _t = M.t
  end

  include Ensure_private_int (Job)
end

let of_event event : t = Obj.magic (event : Event.t)
let of_job job : t = Obj.magic (job : Job.t)
let is_event (t : t) = Obj.is_block (Obj.repr t)
let is_job (t : t) = Obj.is_int (Obj.repr t)

module Match = struct
  type _ kind =
    | Event : Event.t kind
    | Job : Job.t kind

  type packed = K : _ kind -> packed

  let kind t = if is_event t then K Event else K Job
  let project (type a) (_ : a kind) job_or_event = (Obj.magic : t -> a) job_or_event
end
