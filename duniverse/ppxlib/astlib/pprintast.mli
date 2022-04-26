(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Hongbo Zhang (University of Pennsylvania)                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Ast_412

type space_formatter = (unit, Format.formatter, unit) format

val toplevel_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit
val expression : Format.formatter -> Parsetree.expression -> unit
val string_of_expression : Parsetree.expression -> string
val top_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit
val core_type : Format.formatter -> Parsetree.core_type -> unit
val pattern : Format.formatter -> Parsetree.pattern -> unit
val signature : Format.formatter -> Parsetree.signature -> unit
val structure : Format.formatter -> Parsetree.structure -> unit
val string_of_structure : Parsetree.structure -> string

(* Added in the ppxlib copy *)
val class_expr : Format.formatter -> Parsetree.class_expr -> unit
val class_field : Format.formatter -> Parsetree.class_field -> unit
val class_type : Format.formatter -> Parsetree.class_type -> unit
val class_signature : Format.formatter -> Parsetree.class_signature -> unit
val class_type_field : Format.formatter -> Parsetree.class_type_field -> unit
val module_expr : Format.formatter -> Parsetree.module_expr -> unit
val module_type : Format.formatter -> Parsetree.module_type -> unit
val signature_item : Format.formatter -> Parsetree.signature_item -> unit
val structure_item : Format.formatter -> Parsetree.structure_item -> unit
val type_declaration : Format.formatter -> Parsetree.type_declaration -> unit
