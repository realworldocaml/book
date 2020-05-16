open Core
open Async

(* Identical in semantics to Deferred.unit but takes a little longer. *)
let tick = Scheduler.schedule' (fun () -> return ())

let bind_upon t f =
  ignore
    (t
     >>= fun a ->
     f a;
     Deferred.unit)
;;

(* So, there /is/ a subtle difference in semantics between the
 * bind and upon versions, as can be seen by the instance of 'ignore'
 * in bind_upon. It should always be possible to fix bind, but not
 * necessarily upon (whose callback patterns are unrestricted). *)

let rec bind_loop = function
  | 0 -> tick
  | n -> tick >>= fun () -> bind_loop (n - 1)
;;

let pure_loop n =
  Deferred.create (fun i ->
    let rec f = function
      | 0 -> Ivar.fill i ()
      | n -> f (n - 1)
    in
    f n)
;;

(* slow_upon and upon use constant space, but meh uses a little more.
 * But this benchmark is a time benchmark... (space influences
 * GC time influences time) *)
let slow_upon_loop n =
  Deferred.create (fun i ->
    let rec f = function
      | 0 -> Ivar.fill i ()
      | n -> bind_upon tick (fun () -> f (n - 1))
    in
    f n)
;;

let upon_loop n =
  Deferred.create (fun i ->
    let rec f = function
      | 0 -> Ivar.fill i ()
      | n -> upon tick (fun () -> f (n - 1))
    in
    f n)
;;

let loop =
  if Array.length (Sys.get_argv ()) < 2
  then bind_loop
  else (
    match (Sys.get_argv ()).(1) with
    | "bind" -> bind_loop
    | "pure" -> pure_loop
    | "slow_upon" -> slow_upon_loop
    | "upon" -> upon_loop
    | _ -> raise (Failure "unknown loop"))
;;

let () = loop 2000000 >>> fun () -> Shutdown.shutdown 0
let () = never_returns (Scheduler.go ())
