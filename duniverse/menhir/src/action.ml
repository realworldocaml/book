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

type t = {

  (* The code for this semantic action. *)
  expr: IL.expr;

  (* The files where this semantic action originates. Via inlining,
     several semantic actions can be combined into one, so there can
     be several files. *)
  filenames: string list;

  (* The set of keywords that appear in this semantic action. They can be thought
     of as free variables that refer to positions. They must be renamed during
     inlining. *)
  keywords  : KeywordSet.t;

}

(* Creation. *)

let from_stretch s = {
  expr      = IL.ETextual s;
  filenames = [ s.Stretch.stretch_filename ];
  keywords  = KeywordSet.of_list s.Stretch.stretch_keywords
}

let from_il_expr e = {
  expr      = e;
  filenames = [];
  keywords  = KeywordSet.empty;
}

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordSet.mem keyword action.keywords);
  { action with
    expr     = f action.expr;
    keywords = KeywordSet.union keywords (KeywordSet.remove keyword action.keywords)
  }

(* Composition, used during inlining. *)

let compose x a1 a2 =
  (* 2015/07/20: there used to be a call to [parenthesize_stretch] here,
     which would insert parentheses around every stretch in [a1]. This is
     not necessary, as far as I can see, since every stretch that represents
     a semantic action is already parenthesized by the lexer. *)
  {
    expr      = CodeBits.blet ([ IL.PVar x, a1.expr ], a2.expr);
    keywords  = KeywordSet.union a1.keywords a2.keywords;
    filenames = a1.filenames @ a2.filenames;
  }

(* Binding an OCaml pattern to an OCaml variable in a semantic action. *)

let bind p x a =
  {
    expr      = CodeBits.blet ([ p, IL.EVar x ], a.expr);
    keywords  = a.keywords;
    filenames = a.filenames;
  }

(* Substitutions, represented as association lists.
   In principle, no name appears twice in the domain. *)

type subst =
  (string * string) list

let apply (phi : subst) (s : string) : string =
  try
    List.assoc s phi
  with Not_found ->
    s

let apply_subject (phi : subst) (subject : subject) : subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed s ->
      RightNamed (apply phi s)

let extend x y (phi : subst ref) =
  assert (not (List.mem_assoc x !phi));
  if x <> y then
    phi := (x, y) :: !phi

(* Renaming of keywords, used during inlining. *)

type sw =
  Keyword.subject * Keyword.where

(* [rename_keyword f phi keyword] applies the function [f] to possibly change
   the keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword (f : sw -> sw option) (phi : subst ref) keyword : keyword =
  match keyword with
  | SyntaxError ->
      SyntaxError
  | Position (subject, where, flavor) ->
      let subject', where' =
        match f (subject, where) with
        | Some (subject', where') ->
            subject', where'
        | None ->
            apply_subject !phi subject, where
      in
      extend
        (Keyword.posvar subject where flavor)
        (Keyword.posvar subject' where' flavor)
        phi;
      Position (subject', where', flavor)

(* [rename f phi a] applies to the semantic action [a] the renaming [phi] as
   well as the transformations decided by the function [f]. The function [f] is
   applied to each (not-yet-renamed) keyword and may decide to transform it, by
   returning [Some _], or to not transform it, by returning [None]. (In the
   latter case, [phi] still applies to the keyword.) *)

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordSet.map (rename_keyword f phi) keywords in
  let phi = !phi in

  (* Construct a new semantic action, where [phi] is translated into
     a set of *simultaneous* [let] bindings. (We cannot use a series
     of nested [let] bindings, as that would cause a capture if the
     domain and codomain of [phi] have a nonempty intersection.) *)
  let phi = List.map (fun (x, y) -> IL.PVar x, IL.EVar y) phi in
  let expr = CodeBits.eletand (phi, a.expr) in

  {
    expr      = expr;
    filenames = a.filenames;
    keywords  = keywords;
  }

let to_il_expr action =
  action.expr

let filenames action =
  action.filenames

let keywords action =
  action.keywords

let has_syntaxerror action =
  KeywordSet.mem SyntaxError (keywords action)

let has_beforeend action =
  KeywordSet.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords
