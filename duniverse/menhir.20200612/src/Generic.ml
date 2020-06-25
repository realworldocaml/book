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

(* Because the generic comparison function is named [Pervasives.compare] in
   early versions of OCaml and [Stdlib.compare] in recent versions, we cannot
   refer to it under either name. The following definition allows us to refer
   to it under the name [Generic.compare]. *)

let compare =
  compare
