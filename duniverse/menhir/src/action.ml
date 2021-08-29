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

  (* This Boolean flag indicates whether this semantic action originates from
     Menhir's standard library. Via inlining, several semantic actions can be
     combined into one; in that case, we take a conjunction. *)
  standard: bool;

  (* A superset of the free variables that this semantic action can use in
     order to refer to a semantic value. *)
  semvars: StringSet.t;

  (* The set of keywords that appear in this semantic action. They can be
     thought of as free variables that refer to positions. They must be
     renamed during inlining. *)
  keywords  : KeywordSet.t;

}

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

let from_stretch xs s =
  {
    expr      = IL.ETextual s;
    standard  = s.Stretch.stretch_filename = Settings.stdlib_filename;
    semvars   = xs;
    keywords  = KeywordSet.of_list s.Stretch.stretch_keywords
  }

let rec from_il_expr e =
  {
    expr      = e;
    standard  = true;
    semvars   = fv e;
    keywords  = KeywordSet.empty;
  }

(* We are lazy and write an incomplete and somewhat ad hoc [fv] function for
   IL expressions. *)

and fv e =
  fv_aux StringSet.empty e

and fv_aux accu e =
  IL.(match e with
  | EVar x ->
      StringSet.add x accu
  | ETextual _ ->
      (* Considering the manner in which [Action.from_il_expr] is used in the
         module NewRuleSyntax, this piece of text is expected to stand for a
         data constructor, not a semantic value, so we can take its set of
         free variables to be empty. This is rather ad hoc and fragile. *)
      accu
  | ETuple es ->
      List.fold_left fv_aux accu es
  | EApp (e, es) ->
      List.fold_left fv_aux accu (e :: es)
  | _ ->
      assert false (* unsupported *))

(* -------------------------------------------------------------------------- *)

(* Building [let x = a1 in a2]. *)

let compose x a1 a2 =
  (* 2015/07/20: there used to be a call to [parenthesize_stretch] here,
     which would insert parentheses around every stretch in [a1]. This is
     not necessary, as far as I can see, since every stretch that represents
     a semantic action is already parenthesized by the lexer. *)
  {
    expr      = CodeBits.blet ([ IL.PVar x, a1.expr ], a2.expr);
    semvars   = StringSet.union a1.semvars (StringSet.remove x a2.semvars);
    keywords  = KeywordSet.union a1.keywords a2.keywords;
    standard  = a1.standard && a2.standard;
  }

(* Building [let p = x in a]. *)

let rec bind p x a =
  {
    expr      = CodeBits.blet ([ p, IL.EVar x ], a.expr);
    semvars   = StringSet.add x (StringSet.diff a.semvars (bv p));
    keywords  = a.keywords;
    standard  = a.standard;
  }

(* We are lazy and write an incomplete and somewhat ad hoc [bv] function for
   IL patterns. See also [NewRuleSyntax.bv]. *)

and bv p =
  bv_aux StringSet.empty p

and bv_aux accu p =
  IL.(match p with
  | PWildcard
  | PUnit ->
      accu
  | PVar x ->
      StringSet.add x accu
  | PTuple ps ->
      List.fold_left bv_aux accu ps
  | _ ->
      assert false (* unsupported *))

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let to_il_expr action =
  action.expr

let is_standard action =
  action.standard

let semvars action =
  action.semvars

let keywords action =
  action.keywords

let has_syntaxerror action =
  KeywordSet.mem SyntaxError action.keywords

let has_beforeend action =
  KeywordSet.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords

let posvars action =
  KeywordSet.fold (fun keyword accu ->
    match keyword with
    | SyntaxError ->
        accu
    | Position (s, w, f) ->
        let x = Keyword.posvar s w f in
        StringSet.add x accu
  ) (keywords action) StringSet.empty

let vars action =
  StringSet.union (semvars action) (posvars action)

(* -------------------------------------------------------------------------- *)

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordSet.mem keyword action.keywords);
  {
    expr      = f action.expr;
    standard  = action.standard;
    semvars   = action.semvars;
    keywords  = KeywordSet.union
                  keywords (KeywordSet.remove keyword action.keywords)
  }

(* -------------------------------------------------------------------------- *)

(* Simultaneous substitutions are represented as association lists, where no
   name appears twice in the domain. We distinguish renamings that affect
   semantic-value variables and renamings that affect position-keyword
   variables. The two are unfortunately somewhat linked because our
   position-keyword variables are named after semantic-value variables;
   this is quite a mess. *)

type subst =
  {
    semvar: (string * string) list;
    posvar: (string * string) list;
  }

let empty =
  { semvar = []; posvar = [] }

let extend1 x y var =
  assert (not (List.mem_assoc x var));
  if x <> y then (x, y) :: var else var

let extend_semvar x y { semvar; posvar } =
  { semvar = extend1 x y semvar; posvar }

let extend_posvar x y { semvar; posvar } =
  { semvar; posvar = extend1 x y posvar }

let extend = extend_semvar

let apply1 var x =
  try List.assoc x var with Not_found -> x

let apply_semvar phi x =
  apply1 phi.semvar x

let apply_subject phi subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed x ->
      RightNamed (apply_semvar phi x)

let bindings phi =
  phi.posvar @ phi.semvar

let restrict1 xs var =
  List.filter (fun (x, _y) -> StringSet.mem x xs) var

let restrict_semvar xs { semvar; posvar } =
  { semvar = restrict1 xs semvar; posvar }

(* -------------------------------------------------------------------------- *)

(* [rename_keyword f phi keyword] applies [f] to possibly transform the
   keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword f (phi : subst ref) keyword : keyword =
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
      phi :=
        extend_posvar
          (Keyword.posvar subject where flavor)
          (Keyword.posvar subject' where' flavor)
          !phi;
      Position (subject', where', flavor)

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordSet.map (rename_keyword f phi) keywords in
  let phi = !phi in

  let standard = a.standard in

  (* Restrict [phi.semvar] to the set [a.semvars], in order to avoid
     generating [let] bindings that would be both useless and harmful:
     if [x] is not free in [e], then we do not want to generate
     [let x = x' in e], as suddenly [x'] would be free in this new
     expression. *)
  let phi = restrict_semvar a.semvars phi in

  (* Apply [phi.semvar] to the set of free variables. *)
  let semvars = StringSet.map (apply_semvar phi) a.semvars in

  (* Construct a new semantic action, where [phi] is translated into
     a set of *simultaneous* [let] bindings. (We cannot use a series
     of nested [let] bindings, as that would cause a capture if the
     domain and codomain of [phi] have a nonempty intersection.) *)
  let phi = List.map (fun (x, y) -> IL.PVar x, IL.EVar y) (bindings phi) in
  let expr = CodeBits.eletand (phi, a.expr) in

  { expr; standard; semvars; keywords }
