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

(** Monad type which represents a list of results. *)
type 'a m = 'a list

(** [bind x f] applies [f] to a list of results, returning
    a list of results. *)
val bind: 'a m -> ('a -> 'b m) -> 'b m
val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m

(** [return x] is the left and right unit of [bind]. *)
val return: 'a -> 'a m


