open Core
open Async_kernel

module T = struct
  type 'a t =
    | Eager of 'a Deferred.Or_error.t
    | Lazy of 'a Lazy_deferred.t
    | Bind of { mutable state : 'a bind }

  and 'a bind =
    | Ready : 'a t * ('a -> 'b t) -> 'b bind
    | Running : 'a t Deferred.Or_error.t -> 'a bind

  let return x = Eager (Deferred.Or_error.return x)
  let bind t ~f = Bind { state = Ready (t, f) }
  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let of_eager deferred = Eager (Deferred.ok deferred)
let of_lazy lazy_deferred = Lazy lazy_deferred

module On_lazy = struct
  type t = { f : 'a. 'a Lazy_deferred.t -> 'a Deferred.Or_error.t }

  let wait = { f = Lazy_deferred.wait }
  let force = { f = Lazy_deferred.force }
end

let rec run : 'a. 'a t -> on_lazy:On_lazy.t -> 'a Deferred.Or_error.t =
  fun t ~on_lazy ->
  let open Deferred.Or_error.Let_syntax in
  match t with
  | Eager deferred -> deferred
  | Lazy lazy_deferred -> on_lazy.f lazy_deferred
  | Bind bind ->
    (match bind.state with
     | Running result -> result >>= run ~on_lazy
     | Ready (t, f) ->
       let%bind x = run t ~on_lazy in
       (* At this point there may be multiple readers of [run t]. The first of these to
          continue here invokes [f] and sets the state to [Running]. Then the others just
          run on the same result of that [f] call. The goal here is to avoid calling [f]
          multiple times. *)
       let result =
         match bind.state with
         | Running result -> result
         | Ready _ ->
           let result =
             Monitor.try_with_or_error ~extract_exn:true ~rest:`Raise (fun () ->
               Deferred.return (f x))
           in
           bind.state <- Running result;
           result
       in
       result >>= run ~on_lazy)
;;

let force t = run t ~on_lazy:On_lazy.force
let weak_run t = run t ~on_lazy:On_lazy.wait
