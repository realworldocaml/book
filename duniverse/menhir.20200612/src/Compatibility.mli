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

(* The standard library function [String.escaped] in OCaml 4.02.3 depends
   on the operating system function [isprint] and therefore can have OS-
   dependent, locale-dependent behavior. This issue has been fixed in OCaml
   4.03. We use a copy of the code found in OCaml 4.03 and higher, so as to
   avoid this issue. *)

module Bytes : sig

  val escaped: bytes -> bytes

end

module String : sig

  val escaped: string -> string

end
