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

(* A pretty-printer for [IL]. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* [locate_stretches] controls the way we print OCaml stretches (types and
     semantic actions). If it is [Some dstfilename], where [dstfilename] is
     the name of the file that is being written, then we surround stretches
     with OCaml line number directives of the form # <line number> <filename>.
     If it is [None], then we don't. *)

  (* Providing line number directives allows the OCaml typechecker to report
     type errors in the .mly file, instead of in the generated .ml / .mli
     files. Line number directives also affect the dynamic semantics of any
     [assert] statements contained in semantic actions: when they are provided,
     the [Assert_failure] exception carries a location in the .mly file. As a
     general rule of thumb, line number directives should always be provided,
     except perhaps where we think that they decrease readability (e.g., in a
     generated .mli file). *)

  val locate_stretches: string option

end) : sig

  val program: IL.program -> unit

  val expr: IL.expr -> unit

  val interface: IL.interface -> unit

end

(* Common instantiations. In the following two functions, [locate_stretches]
   is [None], so no line number directives are printed. *)

val     print_expr: out_channel -> IL.expr -> unit
val string_of_expr:                IL.expr -> string
