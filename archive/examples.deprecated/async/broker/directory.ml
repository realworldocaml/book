open Core.Std
open Async.Std
open Protocol

type t = (Topic.t, Topic_pub.t) Hashtbl.t

let create () = Topic.Table.create ()

let publish t message =
  let s =
    Hashtbl.find_or_add t message.Message.topic
      ~default:(fun () -> Topic_pub.create message)
  in
  Topic_pub.publish s message

let subscribe t topic =
  match Hashtbl.find t topic with
  | None -> None
  | Some s -> Some (Topic_pub.subscribe s)

let subscribe t topic =
  Option.map (Hashtbl.find t topic) ~f:Topic_pub.subscribe

let subscribe t topic =
  let open Option.Monad_infix in
  Hashtbl.find t topic
  >>| fun s ->
  Topic_pub.subscribe s

let dump t =
  Hashtbl.to_alist t
  |> List.map ~f:(fun (topic,tpub) ->
    let num_subscribers = Topic_pub.num_subscribers tpub in
    let message = Topic_pub.last_message tpub in
    {Dump. topic; num_subscribers; message })
