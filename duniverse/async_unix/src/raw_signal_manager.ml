open Core
module Signal = Core.Signal

module Handlers = struct
  type t = { bag : ((Signal.t -> unit)[@sexp.opaque]) Bag.t } [@@deriving sexp_of]

  let create () = { bag = Bag.create () }
  let add t handler = Bag.add t.bag handler
  let remove t handler_elt = Bag.remove t.bag handler_elt

  let deliver t signal =
    Bag.iter t.bag ~f:(fun handler ->
      try handler signal with
      | exn -> raise_s [%message "signal handler unexpectedly raised" (exn : exn)])
  ;;
end

type delivered = (Signal.t * Handlers.t) Thread_safe_queue.t

type t =
  { handlers_by_signal : Handlers.t Signal.Table.t
  ; delivered : (delivered[@sexp.opaque])
  ; thread_safe_notify_signal_delivered : unit -> unit
  }
[@@deriving sexp_of]

let invariant _ = ()

let create ~thread_safe_notify_signal_delivered =
  { handlers_by_signal = Signal.Table.create ()
  ; delivered = Thread_safe_queue.create ()
  ; thread_safe_notify_signal_delivered
  }
;;

let is_managing t signal = Hashtbl.mem t.handlers_by_signal signal

module Handler = struct
  type t = T of (Handlers.t * (Signal.t -> unit) Bag.Elt.t) list
end

type handler = Handler.t

let get_handlers t signal =
  Hashtbl.find_or_add t.handlers_by_signal signal ~default:(fun () ->
    let handlers = Handlers.create () in
    Signal.Expert.handle signal (fun _ ->
      (* Everything in this function body must be thread safe, since it is running in an
         OCaml signal handler. *)
      Thread_safe_queue.enqueue t.delivered (signal, handlers);
      t.thread_safe_notify_signal_delivered ());
    handlers)
;;

let manage t signal = ignore (get_handlers t signal : Handlers.t)

let install_handler t signals handler =
  Handler.T
    (List.map signals ~f:(fun signal ->
       let handlers = get_handlers t signal in
       handlers, Handlers.add handlers handler))
;;

let remove_handler _t (Handler.T handler) =
  List.iter handler ~f:(fun (handlers, handler_elt) ->
    Handlers.remove handlers handler_elt)
;;

let handle_delivered t =
  while Thread_safe_queue.length t.delivered > 0 do
    let signal, handlers = Thread_safe_queue.dequeue_exn t.delivered in
    Handlers.deliver handlers signal
  done
;;
