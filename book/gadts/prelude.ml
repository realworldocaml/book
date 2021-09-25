#require "base";;
#require "core.top";;

let () = Printexc.record_backtrace false

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
