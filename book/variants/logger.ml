open Base

module Time_ns = Core_kernel.Time_ns

module Heartbeat = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      status_message: string;
    }
end

module Logon = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      user: string;
      credentials: string;
    }
end

type server_state
let handle_log_entry (_:server_state) _ = ()
let handle_logon (_:server_state) _ = ()
let handle_heartbeat (_:server_state) _ = ()
