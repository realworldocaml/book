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

(* The module [Fix] that we present to the final user is obtained as a
   combination of several modules, as follows. *)

(* Define all signatures in the toplevel structure. We expect the user to
   declare [open Fix] and to have direct access to all of these signatures,
   under unqualified names. *)

include Sigs

(* Give access to the following modules as submodules. Thus, if the user has
   declared [open Fix], then she can use [Glue], [Memoize], etc. If she
   hasn't, then she must use [Fix.Glue], [Fix.Memoize], etc. *)

module Glue           = Glue
module Memoize        = Memoize
module Numbering      = Numbering
module GraphNumbering = GraphNumbering
module Tabulate       = Tabulate
module Gensym         = Gensym
module HashCons       = HashCons
module DataFlow       = DataFlow

module Prop = struct
  (* A number of ready-made implementations of the signature [PROPERTY]. *)
  module Boolean = Boolean
  (* These declarations are set up so that the user sees [Prop.Option] and
     [Prop.Set] as functors. *)
  include Option
  include Set
end

(* As a special case, [Core] is renamed [Fix]. Thus, if the user has declared
   [open Fix], then she can still use [Fix.Make], [Fix.ForHashedType], etc.
   (This seems nice.) If she hasn't, then she can still use [Fix.Make],
   because we define an alias for [Make] one level up. This is required for
   compatibility with earlier versions of [Fix] (2013-2018), where [Fix.Make]
   was the sole entry point. *)

module Fix     = Core
module Make    = Core.Make
