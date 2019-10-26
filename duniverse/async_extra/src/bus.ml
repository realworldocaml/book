open! Core
open! Import
module Bus = Core_kernel.Bus
include Bus

let pipe1_exn (t : ('a -> unit) Read_only.t) here =
  let r, w = Pipe.create () in
  let subscription =
    subscribe_exn
      t
      here
      ~f:(function
        | v -> Pipe.write_without_pushback_if_open w v)
      ~on_close:(fun () -> Pipe.close w)
  in
  upon (Pipe.closed w) (fun () -> unsubscribe t subscription);
  r
;;

module First_arity = struct
  type (_, _, _) t =
    | Arity1 : ('a -> unit, 'a -> 'r option, 'r) t
    | Arity2 : ('a -> 'b -> unit, 'a -> 'b -> 'r option, 'r) t
    | Arity3 : ('a -> 'b -> 'c -> unit, 'a -> 'b -> 'c -> 'r option, 'r) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit, 'a -> 'b -> 'c -> 'd -> 'r option, 'r) t
  [@@deriving sexp_of]
end

let first_exn (type c f r) ?stop t here (first_arity : (c, f, r) First_arity.t) ~(f : f) =
  let module A = First_arity in
  Deferred.create (fun ivar ->
    let subscriber : c Bus.Subscriber.t option ref = ref None in
    let finish : r option -> unit = function
      | None -> ()
      | Some r ->
        Ivar.fill ivar r;
        Bus.unsubscribe t (Option.value_exn !subscriber)
    in
    (* We define [can_finish] separately from [finish] because we must call [can_finish]
       before we call [f], so that we do not call [f] if [stop] is determined. *)
    let can_finish =
      match stop with
      | None -> fun () -> true
      | Some stop ->
        upon stop (fun () -> Bus.unsubscribe t (Option.value_exn !subscriber));
        fun () -> not (Deferred.is_determined stop)
    in
    let callback : c =
      match first_arity with
      | A.Arity1 -> fun a -> if can_finish () then finish (f a)
      | A.Arity2 -> fun a1 a2 -> if can_finish () then finish (f a1 a2)
      | A.Arity3 -> fun a1 a2 a3 -> if can_finish () then finish (f a1 a2 a3)
      | A.Arity4 -> fun a1 a2 a3 a4 -> if can_finish () then finish (f a1 a2 a3 a4)
    in
    subscriber :=
      Some
        (Bus.subscribe_exn
           t
           here
           ~on_callback_raise:
             (let monitor = Monitor.current () in
              fun error -> Monitor.send_exn monitor (Error.to_exn error))
           ~f:callback))
;;
