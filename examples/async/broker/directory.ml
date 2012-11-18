open Core.Std
open Async.Std
open Protocol

(* A publisher for a single topic *)
module Topic_pub : sig
  type t
  val create : Message.t -> t
  val publish : t -> Message.t -> unit
  val subscribe : t -> Message.t Pipe.Reader.t
  val num_subscribers : t -> int
  val last_message : t -> Message.t
end = struct
  type t = { mutable last_message: Message.t;
             mutable subscribers: Message.t Pipe.Writer.t list;
           }
  with fields

  let create last_message =
    { last_message; subscribers = [] }

  let clear_closed t =
    t.subscribers <-
      List.filter t.subscribers ~f:(fun pipe ->
        not (Pipe.is_closed pipe))

  let publish t msg =
    clear_closed t;
    t.last_message <- msg;
    List.iter t.subscribers ~f:(fun pipe ->
      don't_wait_for (Pipe.write pipe msg))

  let subscribe t =
    let (r,w) = Pipe.create () in
    don't_wait_for (Pipe.write w t.last_message);
    t.subscribers <- w :: t.subscribers;
    r

  let num_subscribers t = List.length t.subscribers
end

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
  |! List.map ~f:(fun (topic,tpub) ->
    let num_subscribers = Topic_pub.num_subscribers tpub in
    let message = Topic_pub.last_message tpub in
    {Dump. topic; num_subscribers; message })
