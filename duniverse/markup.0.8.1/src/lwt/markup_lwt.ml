(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

let ensure_tail_calls ?hook:_hook = ignore

let to_cps thread =
  fun throw k ->
    let thread = thread () in
    match Lwt.state thread with
    | Lwt.Return x -> k x
    | Lwt.Fail e -> throw e
    | Lwt.Sleep -> Lwt.on_any thread k throw

module Adapter =
struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return

  let of_cps f =
    let thread, wake = Lwt.wait () in
    f (Lwt.wakeup_later_exn wake) (Lwt.wakeup_later wake);
    thread

  let to_cps = to_cps
end

include Markup.Asynchronous (Adapter)

let lwt_stream s = (fun () -> Lwt_stream.get s) |> stream

let to_lwt_stream s = (fun () -> next s) |> Lwt_stream.from
