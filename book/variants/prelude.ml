#require "core,core.top";;

type server_state
let handle_log_entry (_:server_state) _ = ()
let handle_logon (_:server_state) _ = ()
let handle_heartbeat (_:server_state) _ = ()

let () = Printexc.record_backtrace false
