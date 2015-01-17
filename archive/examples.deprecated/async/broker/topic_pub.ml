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
