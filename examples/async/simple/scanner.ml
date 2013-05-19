open Core.Std
open Async.Std

module Expect : sig
  type t
  val empty : t
  (* return a deferred that becomes determined as soon as a line is
     read that satisfied the condition in question *)
  val add_condition : t -> (string -> bool) -> t * string Deferred.t
  (* Run the scanner, returning a scanner corresponding to the set of
     unsatisfied conditions *)
  val feed : t -> string -> t
  val run : t -> Reader.t -> t Deferred.t

end = struct
  type t = ((string -> bool) * string Ivar.t) list

  let empty = []

  let add_condition t cond =
    let ivar = Ivar.create () in
    let t' = (cond,ivar) :: t in
    (t',Ivar.read ivar)

  let feed t line =
    List.filter t ~f:(fun (cond,ivar) ->
      if cond line then (Ivar.fill ivar line; false)
      else true)

  let run t r = Pipe.fold (Reader.lines r) ~init:t ~f:feed
end


module Sequencer : sig
  type t
  val create : unit -> t
  val run : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end = struct
  type t = (unit -> unit Deferred.t) Queue.t

  let create () = Queue.create ()

  let rec process_queue t =
    match Queue.peek t with
    | None -> ()
    | Some thunk ->
      thunk ()
      >>> fun () ->
      ignore (Queue.dequeue t);
      if not (Queue.is_empty t) then process_queue t

  let run t thunk =
    Deferred.create (fun ivar ->
      let was_empty = Queue.is_empty t in
      Queue.enqueue t (fun () ->
        thunk () >>| fun x -> Ivar.fill ivar x);
      if was_empty then process_queue t
    )
end

module type Delayer_intf = sig
  type t
  val create : Time.Span.t -> t
  val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer : Delayer_intf = struct
  type t = { delay: Time.Span.t;
             jobs: (unit -> unit) Queue.t;
           }

  let create delay =
    { delay; jobs = Queue.create () }

  let schedule t thunk =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () ->
      upon (thunk ()) (fun x -> Ivar.fill ivar x));
    upon (after t.delay) (fun () ->
      let job = Queue.dequeue_exn t.jobs in
      job ());
    Ivar.read ivar
end
