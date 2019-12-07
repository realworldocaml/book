open Core_kernel
open Deferred_std
module Deferred = Deferred1

let reraise = function
  | Ok x -> x
  | Error exn -> Exn.reraise exn "caught exception in memoized function"
;;

let general (type a) (hashable : (module Hashable.S_plain with type t = a)) f =
  let module Hashable = (val hashable) in
  let f =
    Memo.general ~hashable:Hashable.hashable (fun a ->
      Monitor.try_with ~run:`Now (fun () -> f a))
  in
  Staged.stage (fun a -> f a >>| reraise)
;;

let unit f =
  let f = Memo.unit (fun () -> Monitor.try_with ~run:`Now f) in
  Staged.stage (fun () -> f () >>| reraise)
;;
