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

(** This module implements a very simple notion of ``mark''. *)

(** The type of marks. *)
type t

(** [fresh()] generates a fresh mark, that is, a mark that is guaranteed
    to be distinct from all existing marks. *)
val fresh: unit -> t

(** [same mark1 mark2] tells whether [mark1] and [mark2] are the same
    mark, that is, were created by the same call to [fresh]. *)
val same: t -> t -> bool

(** [none] is a distinguished mark, created via an initial call to
    [fresh()]. *)
val none: t

