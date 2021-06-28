open! Core
open! Import
module Mutex = Error_checking_mutex

type 'a t =
  { mutable value : 'a option
  ; mutable num_waiting : int
  ; mutex :
      (Mutex.t[@sexp.opaque]
      (* Threads that do [read t] when [is_none t.value] block using [Condition.wait t.full].
         When [fill] sets [t.value], it uses [Condition.broadcast] to wake up all the blocked
         threads. *))
  ; full : (Condition.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let create () =
  { value = None; num_waiting = 0; mutex = Mutex.create (); full = Condition.create () }
;;

let critical_section t ~f = Mutex.critical_section t.mutex ~f

let fill t v =
  critical_section t ~f:(fun () ->
    if is_some t.value then raise_s [%message "Thread_safe_ivar.fill of full ivar"];
    t.value <- Some v;
    Condition.broadcast t.full)
;;

let read t =
  match t.value with
  | Some v -> v
  | None ->
    critical_section t ~f:(fun () ->
      match t.value with
      | Some v -> v
      | None ->
        t.num_waiting <- t.num_waiting + 1;
        Condition.wait t.full t.mutex;
        t.num_waiting <- t.num_waiting - 1;
        (match t.value with
         | Some v -> v
         | None -> assert false))
;;
