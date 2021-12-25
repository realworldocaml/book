open Core

type t =
  { events : Time_ns.t Queue.t
  ; mutable now : Time_ns.t
  ; period : Time_ns.Span.t
  ; rate : int
  }

let create ~now ~period ~rate =
  let events = Queue.create () in
  { events; now; period; rate }

let rec drain_old_events t =
  match Queue.peek t.events with
  | None -> ()
  | Some time ->
    if Time_ns.Span.( < ) (Time_ns.diff t.now time) t.period
    then (
      ignore (Queue.dequeue_exn t.events : Time_ns.t);
      drain_old_events t)

let maybe_consume t ~now =
  t.now <- now;
  drain_old_events t;
  if Queue.length t.events < t.rate
  then (
    Queue.enqueue t.events now;
    `Consumed)
  else `No_capacity
