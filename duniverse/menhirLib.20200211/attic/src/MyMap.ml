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

module Make (Ord: Map.OrderedType) =
  struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    (* [add x data f t] returns [t] (physically unchanged) if [x] is already a
       member of [t] and the function [f] does not modify the data associated
       with [x]. *)

    let rec add x data f = function
      | Empty ->
          (* New binding of [x] to [f data]. *)
          Node (Empty, x, f data, Empty, 1)
      | Node(l, v, d, r, h) as t ->
          let c = Ord.compare x v in
          if c = 0 then
            (* Existing key. Call [f]. *)
            let d' = f d in
            if d == d' then
              (* [f] requests no change. Return the tree unchanged. *)
              t
            else
              (* [f] requests a change. Build a new tree node. *)
              Node(l, v, d', r, h)
          else if c < 0 then
            let l' = add x data f l in
            if l == l' then t
            else bal l' v d r
          else
            let r' = add x data f r in
            if r == r' then t
            else bal l v d r'

    let empty = Empty

    let rec find x = function
        Empty -> raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) -> iter f l; f v d; iter f r

end
