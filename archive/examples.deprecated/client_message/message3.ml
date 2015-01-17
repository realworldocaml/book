open Core.Std

module Log_entry = struct
  type t =
    { session_id: string;
      time: Time.t;
      important: bool;
      message: string;
    }
end
module Heartbeat = struct
  type t =
    { session_id: string;
      time: Time.t;
      status_message: string;
    }
end
module Logon = struct
  type t =
    { session_id: string;
      time: Time.t;
      user: string;
      credentials: string;
    }
end

type client_message =
| Logon of Logon.t
| Heartbeat of Heartbeat.t
| Log_entry of Log_entry.t

let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) message ->
        let maybe_add_message session_id =
          if Set.mem user_sessions session_id then
            (message::messages,user_sessions)
          else acc
        in
        match message with
        | Logon m ->
          (* from a logon, we may get a new message and and a new user session *)
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions m.Logon.session_id)
          else acc
        | Heartbeat message -> maybe_add_message message.Heartbeat.session_id
        | Log_entry message -> maybe_add_message message.Log_entry.session_id
      )
  in
  List.rev user_messages
