open Core.Std

module Host_info = struct
  type t = { hostname: string;
             os_name: string;
             os_release: string;
             cpu_arch: string;
             num_cores: int;
           }
end

type logon_event =
  { session_id: string;
    time: Time.t;
    client_addr: Unix.Inet_addr.t;
    client: int;
  } with fields

type heartbeat =
  { session_id: string;
    time: Time.t;
    status_message: string;
  } with fields
