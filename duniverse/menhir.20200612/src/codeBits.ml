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

(* This module provides a number of tiny functions that help produce
   [IL] code. *)

open IL

(* A smart constructor for [PVarLocated]. *)

let pvarlocated id =
  let x, pos = Positions.decompose id in
  let pos1 = Positions.start_of_position pos
  and pos2 = Positions.end_of_position pos in
  if pos1 == Lexing.dummy_pos || pos2 == Lexing.dummy_pos then
    PVar x
  else
    PVarLocated id

(* Tuples. *)

let etuple = function
  | [] ->
      EUnit
  | [ e ] ->
      e
  | es ->
      ETuple es

let ptuple = function
  | [] ->
      PUnit
  | [ p ] ->
      p
  | ps ->
      PTuple ps

(* A list subject to a condition. *)

let listif condition xs =
  if condition then
    xs
  else
    []

let elementif condition x =
  if condition then
    [ x ]
  else
    []

let listiflazy condition xs =
  if condition then
    xs()
  else
    []

(* The unit type. *)

let tunit =
  TypApp ("unit", [])

(* The Boolean type. *)

let tbool =
  TypApp ("bool", [])

(* The integer type. *)

let tint =
  TypApp ("int", [])

(* The string type. *)

let tstring =
  TypApp ("string", [])

(* The exception type. *)

let texn =
  TypApp ("exn", [])

(* The type of pairs. *)

let tpair typ1 typ2 =
  TypTuple [typ1; typ2]

(* The type of lexer positions. *)

let tposition =
  TypApp ("Lexing.position", [])

(* The type of the $loc and $sloc keywords. *)

(* A location is a pair of positions. This might change in the future. *)

let tlocation =
  tpair tposition tposition

(* The type of lexer buffers. *)

let tlexbuf =
  TypApp ("Lexing.lexbuf", [])

(* The type of untyped semantic values. *)

let tobj =
  TypApp ("Obj.t", [])

(* Building a type variable. *)

let tvar x : typ =
  TypVar x

(* Building a type scheme. *)

let scheme qs t =
  {
    quantifiers = qs;
    body = t
  }

(* Building a type scheme with no quantifiers out of a type. *)

let type2scheme t =
  scheme [] t

(* Constraining an expression to have a (monomorphic) type. *)

let annotate e t =
  EAnnot (e, type2scheme t)

let pat2var = function
  | PVar x ->
      x
  | _ ->
      assert false

(* [simplify] removes bindings of the form [let v = v in ...] and
   [let _ = v in ...]. *)

let rec simplify = function
  | [] ->
      []
  | (PVar v1, EVar v2) :: bindings when v1 = v2 ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | (PWildcard, EVar _) :: bindings ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | binding :: bindings ->
      binding :: simplify bindings

(* Building a [let] construct, with on-the-fly simplification. *)

let blet (bindings, body) =
  let bindings = simplify bindings in
  match bindings, body with
  | [], _ ->
      body
  | [ PVar x1, e ], EVar x2 when x1 = x2 ->
      (* Reduce [let x = e in x] to just [e]. *)
      e
  | _, _ ->
      ELet (bindings, body)

let mlet formals actuals body =
  blet (List.combine formals actuals, body)

(* Simulating a [let/and] construct using tuples. *)

let eletand (bindings, body) =
  let bindings = simplify bindings in
  match bindings, body with
  | [], _ ->
      (* special case: zero bindings *)
      body
  | [ PVar x1, e ], EVar x2 when x1 = x2 ->
      (* Reduce [let x = e in x] to just [e]. *)
      e
  | [ _ ], _ ->
      (* special case: one binding *)
      ELet (bindings, body)
  | _ :: _ :: _, _ ->
      (* general case: at least two bindings *)
      let pats, exprs = List.split bindings in
      ELet ([ PTuple pats, ETuple exprs ], body)

(* [eraisenotfound] is an expression that raises [Not_found]. *)

let eraisenotfound =
  ERaise (EData ("Not_found", []))

(* [bottom] is an expression that has every type. Its semantics is
   irrelevant. *)

let bottom =
  eraisenotfound

(* Boolean constants. *)

let efalse : expr =
  EData ("false", [])

let etrue : expr =
  EData ("true", [])

let eboolconst b =
  if b then etrue else efalse

(* Option constructors. *)

let enone =
  EData ("None", [])

let esome e =
  EData ("Some", [e])

(* List constructors. *)

let rec elist xs =
  match xs with
  | [] ->
      EData ("[]", [])
  | x :: xs ->
      EData ("::", [ x; elist xs ])

(* Integer constants as patterns. *)

let pint k : pattern =
  PData (string_of_int k, [])

(* These help build function types. *)

let arrow typ body : typ =
  TypArrow (typ, body)

let arrowif flag typ body : typ =
  if flag then
    arrow typ body
  else
    body

let marrow typs body : typ =
  List.fold_right arrow typs body

(* ------------------------------------------------------------------------ *)
(* Here is a bunch of naming conventions. Our names are chosen to minimize
   the likelihood that a name in a semantic action is captured. In other
   words, all global definitions as well as the parameters to [reduce]
   are given far-fetched names, unless [--no-prefix] was specified. Note
   that the prefix must begin with '_'. This allows avoiding warnings
   about unused variables with ocaml 3.09 and later. *)

let prefix name =
  if Settings.noprefix then
    name
  else
    "_menhir_" ^ name

let dataprefix name =
  if Settings.noprefix then
    name
  else
    "Menhir" ^ name

let tvprefix name =
  if Settings.noprefix then
    name
  else
    "ttv_" ^ name

(* ------------------------------------------------------------------------ *)

(* Converting an interface to a structure. Only exception and type definitions
   go through. *)

let interface_item_to_structure_item = function
  | IIExcDecls defs ->
      [ SIExcDefs defs ]
  | IITypeDecls defs ->
      [ SITypeDefs defs ]
  | IIFunctor (_, _)
  | IIValDecls _
  | IIInclude _
  | IIModule (_, _)
  | IIComment _ ->
      []

let interface_to_structure i =
  List.flatten (List.map interface_item_to_structure_item i)

(* Constructing a named module type together with a list of "with type"
   constraints. *)

let with_types wk name tys =
  List.fold_left (fun mt (params, name, ty) ->
    MTWithType (mt, params, name, wk, ty)
  ) (MTNamedModuleType name) tys

let mapp me1 me2 =
  MApp (me1, me2)

let mapp me1 mes2 =
  List.fold_left mapp me1 mes2
