(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

module Set (X : sig
  type t
  val empty: t
  val equal: t -> t -> bool
end) = struct

  type property =
    X.t (* a set *)

  let bottom =
    X.empty

  let equal =
    X.equal

  let is_maximal _s =
    false
      (* We do not know what the full set is. We could take it as
         a functor argument, but the comparison would be costly
         anyway, so that seems pointless. *)

end
