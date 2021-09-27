#require "base,core.top,async";;

let () = Printexc.record_backtrace false

let handle_state_changes _ _ = Async.Deferred.return ()

module User_id : sig
  type t
end = struct type t = unit end

module User_name : sig
  type t
end = struct type t = unit end

module Permissions : sig
  type t
  val check : t -> User_id.t -> bool
end = struct
  type t = unit
  let check _ _ = false
end
