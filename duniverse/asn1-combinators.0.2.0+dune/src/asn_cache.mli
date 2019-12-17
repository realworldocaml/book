(* Copyright (c) 2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type 'a var

val variant : unit -> 'a var

module Make (KV: sig
  type 'a k and 'a v
  val mapv : ('a -> 'b) -> 'a v -> 'b v
end): sig
  type t
  val create : unit -> t
  val intern : t -> 'a var -> 'a KV.k -> 'a KV.v -> 'a KV.v
end
