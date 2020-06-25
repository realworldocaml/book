(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

module Run
(G : sig
  type node
  type label
  val foreach_outgoing_edge: node -> (label -> node -> unit) -> unit
  val foreach_root: (node -> unit) -> unit
end)
(M : sig
  val mark: G.node -> unit
  val is_marked: G.node -> bool
end)
(D : sig
  val discover: G.node -> unit
  val traverse: G.node -> G.label -> G.node -> unit
end)
= struct

  open G
  open M
  open D

  let rec visit node =
    if not (is_marked node) then begin
      mark node;
      discover node;
      foreach_outgoing_edge node (fun label target ->
        traverse node label target;
        visit target
      )
    end

  let () =
    foreach_root visit

end

module MarkSet (S : Set.S) = struct
  let marked =
    ref S.empty
  let is_marked x =
    S.mem x !marked
  let mark x =
    marked := S.add x !marked
  let marked () =
    !marked
end

module MarkArray (G : sig
  type node
  val n: int
  val number: node -> int
end) = struct
  let marked =
    Array.make G.n false
  let is_marked x =
    marked.(G.number x)
  let mark x =
    marked.(G.number x) <- true
  let marked () =
    marked
end
