open Core_kernel
open Deferred_std

module T = struct
  type 'a t =
    { start : unit Ivar.t
    ; result : 'a Or_error.t Deferred.t
    }

  let create f =
    let start = Ivar.create () in
    { start
    ; result =
        (let%bind () = Ivar.read start in
         Monitor.try_with_or_error f)
    }
  ;;

  let wait t = t.result
  let wait_exn t = wait t >>| ok_exn
  let start t = Ivar.fill_if_empty t.start ()

  let force t =
    start t;
    wait t
  ;;

  let force_exn t = force t >>| ok_exn
  let return a = create (fun () -> return a)

  let bind t ~f =
    create (fun () ->
      let%bind a = force_exn t in
      force_exn (f a))
  ;;

  let map t ~f = create (fun () -> force_exn t >>| f)
  let map = `Custom map
end

include T
include Monad.Make (T)

let bind' t f = bind t ~f:(fun a -> create (fun () -> f a))
let is_forced t = Ivar.is_full t.start
let is_determined t = Deferred.is_determined t.result
let peek t = Deferred.peek t.result
let peek_exn t = Option.map (peek t) ~f:ok_exn
