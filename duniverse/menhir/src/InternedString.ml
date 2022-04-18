(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Make () = struct

  (* Create a new service for assigning unique integer codes to strings. *)

  type t =
    int

  let equal (x : t) (y : t) = (x = y)
  let compare (x : t) (y : t) = x - y
  let hash (x : t) = x

  let import, export, _verbose =
    Misc.new_encode_decode 2048

  module Int = struct
    type t = int
    let compare = compare
  end

  module Set = struct

    include Set.Make(Int)

    let import (ss : StringSet.t) : t =
      StringSet.fold (fun s accu ->
        add (import s) accu
      ) ss empty

    let export (ss : t) : StringSet.t =
      fold (fun s accu ->
        StringSet.add (export s) accu
      ) ss StringSet.empty

    let print ss =
      StringSet.print (export ss)

  end

  module Map = struct

    include Map.Make(Int)

    let domain m =
      fold (fun key _ accu -> Set.add key accu) m Set.empty

    let filter p m =
      fold (fun key v m ->
        if p key v then
          add key v m
        else
          m
      ) m empty

    let restrict domain m =
      filter (fun k _ -> Set.mem k domain) m

  end

end
