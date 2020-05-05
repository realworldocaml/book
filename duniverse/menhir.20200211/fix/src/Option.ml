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

module Option (X : sig type t end) = struct

  open X

  type property =
    t option

  let bottom =
    None

  let equal (o1 : property) (o2 : property) =
    (* It is permitted to assume that [o1 <= o2] holds. This implies that
       when [o1] is [Some x1] and [o2] is [Some x2] we may return [true]
       without actually comparing [x1] and [x2]. *)
    match o1, o2 with
    | Some _, None ->
        (* Because [o1 <= o2] holds, this cannot happen. *)
        let msg = Printf.sprintf "\n  Fix.Prop.Option says: \
          please check that your \"rhs\" function is \
          monotone.\n  %s\n" __LOC__ in
        raise (Invalid_argument msg)
    | None, Some _ ->
        false
    | None, None
    | Some _, Some _ ->
        true

  let is_maximal o =
    match o with
    | None ->
        false
    | Some _ ->
        true

end
