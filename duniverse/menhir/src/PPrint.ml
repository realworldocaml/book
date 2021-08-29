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

(* The library pprint, which is found in the subdirectory pprint/, has been
   renamed vendored_pprint so as to prevent Dune from complaining about
   a conflict with a copy of pprint that might be installed on the user's
   system. *)

(* As a result, the library is now accessible under the name Vendored_pprint.
   Because we do not want to pollute Menhir's sources with this name, we
   define the module PPrint as an alias for Vendored_pprint. *)

include Vendored_pprint
