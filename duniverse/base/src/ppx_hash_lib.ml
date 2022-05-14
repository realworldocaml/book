(** This module is for use by ppx_hash, and is thus not in the interface of Base. *)
module Std = struct
  module Hash = Hash (** @canonical Base.Hash *)
end

type 'a hash_fold = Std.Hash.state -> 'a -> Std.Hash.state

module Hashable = struct
  module type S = sig
    type t

    val hash_fold_t : t hash_fold
    val hash : t -> Std.Hash.hash_value
  end

  module type S1 = sig
    type 'a t

    val hash_fold_t : 'a hash_fold -> 'a t hash_fold
  end

  module type S2 = sig
    type ('a, 'b) t

    val hash_fold_t : 'a hash_fold -> 'b hash_fold -> ('a, 'b) t hash_fold
  end

  module type S3 = sig
    type ('a, 'b, 'c) t

    val hash_fold_t
      :  'a hash_fold
      -> 'b hash_fold
      -> 'c hash_fold
      -> ('a, 'b, 'c) t hash_fold
  end
end
