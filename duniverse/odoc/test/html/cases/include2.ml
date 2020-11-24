(** Comment about X that should not appear when including X below. *)
module X = struct
  type t = int
end

include X

