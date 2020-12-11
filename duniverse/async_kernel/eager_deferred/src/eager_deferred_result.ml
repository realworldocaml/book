open! Core_kernel
open! Async_kernel
open! Import
module Deferred = Eager_deferred0
open Deferred.Let_syntax

(* Copied from [deferred_result.ml].  There should be no diffs below this line. *)

module T = struct
  type ('a, 'error) t = ('a, 'error) Result.t Deferred.t
end

include T

let combine t1 t2 ~ok ~err =
  let%map t1 = t1
  and t2 = t2 in
  Result.combine t1 t2 ~ok ~err
;;

include Monad.Make2 (struct
    include T

    let return a = Deferred.return (Ok a)

    let bind t ~f =
      Deferred.bind t ~f:(function
        | Ok a -> f a
        | Error _ as error -> Deferred.return error)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

let fail x = Deferred.return (Error x)
let failf format = Printf.ksprintf fail format
let map_error t ~f = Deferred.map t ~f:(fun r -> Result.map_error r ~f)
