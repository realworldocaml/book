(** This module is for use by ppx_hash, and is thus not in the interface of Base. *)
module Std = struct
  module Hash = Hash (** @canonical Base.Hash *)
end
