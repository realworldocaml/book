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

open Keyword

(** Semantic action's type. *)
type t

(** [compose x a1 a2] builds the action [let x = a1 in a2]. This
    feature is used during the processing of the %inline keyword. *)
val compose : string -> t -> t -> t

(** [bind p x a] binds the OCaml pattern [p] to the OCaml variable [x] in the
   semantic action [a]. Therefore, it builds the action [let p = x in a]. *)
val bind: IL.pattern -> string -> t -> t

(* [define keyword keywords f action] defines away the keyword [keyword].
   It is removed from the set of keywords of this semantic action; the
   set [keywords] is added in its place. The body of the semantic action
   is transformed by the function [f], which typically wraps it in some
   new [let] bindings. *)

val define: keyword -> KeywordSet.t -> (IL.expr -> IL.expr) -> t -> t

(* Variable-to-variable substitutions, used by [rename], below. *)

type subst =
  (string * string) list

(* [Subject/where] pairs, as defined in [Keyword], encode a position
   keyword. *)

type sw =
  subject * where

(** [rename f phi a] applies to the semantic action [a] the renaming [phi] as
    well as the transformations decided by the function [f]. The function [f] is
    applied to each (not-yet-renamed) keyword and may decide to transform it, by
    returning [Some _], or to not transform it, by returning [None]. (In the
    latter case, [phi] still applies to the keyword.) *)
val rename:
  (sw -> sw option) ->
  subst ->
  t ->
  t

(** Semantic actions are translated into [IL] code using the
    [IL.ETextual] and [IL.ELet] constructors. *)
val to_il_expr: t -> IL.expr

(** A semantic action might be the inlining of several others. The
    filenames of the different parts are given by [filenames a]. This
    can be used, for instance, to check whether all parts come from
    the standard library. *)
val filenames: t -> string list

(** [keywords a] is the set of keywords used in the semantic action [a]. *)
val keywords: t -> KeywordSet.t

(** [from_stretch s] builds an action out of a textual piece of code. *)
val from_stretch: Stretch.t -> t

(** [from_il_expr] converts an [IL] expression into a semantic action. *)
val from_il_expr: IL.expr -> t

(** Test whether the keyword [$syntaxerror] is used in the action. *)
val has_syntaxerror: t -> bool

(** Test whether the keyword [$endpos($0)] is used in the action. *)
val has_beforeend: t -> bool
