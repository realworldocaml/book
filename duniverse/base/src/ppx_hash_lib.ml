(** This module is for use by ppx_hash, and is thus not in the interface of Base. *)
module Std = struct

  (** @canonical Base.Hash *)
  module Hash = Hash
end
