open Core.Std

module Log_entry = struct
  type t =
    { important: bool;
      message: string;
    }
end

module Heartbeat = struct
  type t =
    { status_message: string; }
end

module Logon = struct
  type t =
    { user: string;
      credentials: string;
    }
end

type details =
| Logon of Logon.t
| Heartbeat of Heartbeat.t
| Log_entry of Log_entry.t

module Common = struct
  type t = { session_id: string;
             time: Time.t;
           }
end

let messages_for_user user messages =
  let (user_messages,_) =
    List.fold messages ~init:([],String.Set.empty)
      ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
        let session_id = common.Common.session_id in
        match details with
        | Logon m ->
          if m.Logon.user = user then
            (message::messages, Set.add user_sessions session_id)
          else acc
        | Heartbeat _ | Log_entry _ ->
          if Set.mem user_sessions session_id then
            (message::messages,user_sessions)
          else acc
      )
  in
  List.rev user_messages
