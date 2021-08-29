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

(**The option type, equipped with the ordering is [None <= Some x]. *)

(**This ordering is not a lattice. *)

(**Although the code is polymorphic in the type of elements, it must still
   be packaged as a functor, because [property] cannot be a parameterized
   type. *)

open Sigs

module Option (X : sig type t end) : PROPERTY with type property = X.t option
