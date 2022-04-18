(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module provides a number of tiny functions that help produce
   [IL] code. *)

open Positions
open IL

(** A smart constructor for [PVarLocated]. *)
val pvarlocated: string located -> pattern

(** A type name. *)
val tname: string -> typ

(* Tuples. *)

val etuple: expr list -> expr
val ptuple: pattern list -> pattern

(* Standard types. *)

val tunit: typ
val tbool: typ
val tint: typ
val tstring: typ
val texn: typ
val tposition: typ
val tlocation: typ
val tlexbuf: typ
val tobj : typ

(** Building a type variable. *)
val tvar: string -> typ

(** Building a type scheme. *)
val scheme: string list -> typ -> typescheme

(** Building a locally abstract type scheme. *)
val local_scheme: string list -> typ -> typescheme

(** Converting a type to a type scheme. *)
val type2scheme: typ -> typescheme

(** Constraining an expression to have a (monomorphic) type. *)
val annotate: expr -> typ -> expr

(** Projecting out of a [PVar] pattern. *)
val pat2var: pattern -> string

(** This function constructs a nested sequence of [let] definitions.
   Certain simplifications are performed on the fly. *)
val blet: (pattern * expr) list * expr -> expr

(** [mlet] is like [blet], but takes separate lists of patterns and
    expressions *)
val mlet: pattern list -> expr list -> expr -> expr

(** Simulating a [let/and] construct. *)
val eletand: (pattern * expr) list * expr -> expr

(** [eraisenotfound] is an expression that raises [Not_found]. *)
val eraisenotfound: expr

(** [eassert] builds a runtime assertion [assert e]. *)
val eassert: expr -> expr

(** [bottom] is an expression that has every type. Its semantics is
    irrelevant. *)
val bottom: expr

(* Boolean constants. *)

val etrue: expr
val efalse: expr
val eboolconst: bool -> expr

(* Option constructors. *)

val enone: expr
val esome: expr -> expr

(* List constructors. *)

val elist: expr list -> expr

(* Integer constants as patterns. *)

val pint: int -> pattern

(* These help build function types. *)

val arrow: typ -> typ -> typ
val arrowif: bool -> typ -> typ -> typ
val marrow: typ list -> typ -> typ

(* Tracing. *)

(**[eprintf format args] constructs a call to [Printf.eprintf], which
   log a tracing message onto [stderr]. *)
val eprintf: string -> expr list -> expr

(**[trace format args] returns a list of (effectful) bindings which,
   when [--trace] is enabled, log a tracing message onto [stderr]. *)
val trace: string -> expr list -> (pattern * expr) list

(**[tracecomment c e] emits either a comment whose content is the string
   [c] or a tracing message whose content is also [c]. *)
val tracecomment: string -> expr -> expr

(* These functions are used to generate names in menhir's namespace. *)
val prefix: string -> string
val dataprefix: string -> string
val tvprefix: string -> string

(** Converting an interface to a structure. Only exception and type definitions
    go through. *)
val interface_to_structure: interface -> structure

(** Constructing a named module type together with a list of "with type"
    constraints. *)
val with_types: with_kind -> string -> (string list * string * typ) list -> module_type

(** Functor applications. *)
val mapp: modexpr -> modexpr list -> modexpr

(** Record fields. *)
val field: bool -> string -> typ -> fielddef

(** Branches. *)
val branch: pattern -> expr -> branch

(** A variable as an expression. *)
val evar: string -> expr

(** A list of variables as expressions. *)
val evars: string list -> expr list

(** A variable as a pattern. *)
val pvar: string -> pattern

(** A list of variables as patterns. *)
val pvars: string list -> pattern list

(** A private value definition. *)
val def: string -> expr -> valdef

(** A public value definition. *)
val defpublic: string -> expr -> valdef

(** A single non-recursive value definition. *)
val valdef: valdef -> structure_item

(** A list of non-recursive value definitions. *)
val valdefs: valdef list -> structure_item list
