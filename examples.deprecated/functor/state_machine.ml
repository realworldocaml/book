open Core.Std
open Async.Std

module type Machine = sig

  module Event : Sexpable

  module Action : sig
    type t
    val deliver : t -> unit
  end

  module State : sig
    type t
    val create : Config.t -> t Or_error.t
    val update : t -> Event.t -> Action.t Or_error.t
  end
end

module type Run_transactional_machine (State:State) : sig
  val command : Command.t
end = struct

  val start_server logname =
    ....

end
