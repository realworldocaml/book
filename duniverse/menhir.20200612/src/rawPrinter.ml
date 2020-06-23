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

(* A debugging pretty-printer for [IL]. Newlines are used liberally, so as to
   facilitate diffs. *)

open IL

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

end) = struct

(* ------------------------------------------------------------------------- *)
(* XML-style trees. *)

type tree =
  | Node of string * tree list

let node label ts =
  Node (label, ts)

(* ------------------------------------------------------------------------- *)
(* Dealing with newlines and indentation. *)

let maxindent =
  120

let whitespace =
  Bytes.make maxindent ' '

let indentation =
  ref 0

let line =
  ref 1

(* [rawnl] is, in principle, the only place where writing a newline
   character to the output channel is permitted. This ensures that the
   line counter remains correct. But see also [stretch] and [typ0]. *)

let rawnl f =
  incr line;
  output_char f '\n'

let nl f =
  rawnl f;
  output f whitespace 0 !indentation

let indent ofs producer f x =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + ofs in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  nl f;
  producer f x;
  indentation := old_indentation

(* ------------------------------------------------------------------------- *)
(* Tree printers. *)

let rec print_tree f = function
  | Node (label, []) ->
      output_char f '<';
      output_string f label;
      output_char f '/';
      output_char f '>';
      nl f
  | Node (label, ts) ->
      output_char f '<';
      output_string f label;
      output_char f '>';
      indent 2 print_trees f ts;
      output_char f '<';
      output_char f '/';
      output_string f label;
      output_char f '>';
      nl f

and print_trees f = function
  | [] ->
      ()
  | t :: ts ->
      print_tree f t;
      print_trees f ts

(* ------------------------------------------------------------------------- *)
(* Expression-to-tree converter. *)

let rec expr e =
  match e with
  | EComment (c, e) ->
      node "comment" [ string c; expr e ]
  | EPatComment (s, p, e) ->
      node "patcomment" [ string s; pat p; expr e ]
  | ELet (pes, e2) ->
      node "let" ( patexprs pes @ [ expr e2 ])
  | ERecordWrite (e1, field, e2) ->
      node "recordwrite" [ expr e1; string field; expr e2 ]
  | EMatch (e, brs) ->
      node "match" ( expr e :: branches brs )
  | ETry (e, brs) ->
      node "try" ( expr e :: branches brs )
  | EIfThen (e1, e2) ->
      node "ifthen" [ expr e1; expr e2 ]
  | EIfThenElse (e0, e1, e2) ->
      node "ifthenelse" [ expr e0; expr e1; expr e2 ]
  | EFun (ps, e) ->
      node "fun" ( pats ps @ [ expr e ])
  | EApp (e, args) ->
      node "app" ( expr e :: exprs args )
  | ERaise e ->
      node "raise" [ expr e ]
  | EMagic e ->
      node "magic" [ expr e ]
  | ERepr e ->
      node "repr" [ expr e ]
  | EData (d, args) ->
      node "data" ( string d :: exprs args )
  | EVar v ->
      node "var" [ string v ]
  | ETextual action ->
      node "text" [ stretch action ]
  | EUnit ->
      node "unit" []
  | EIntConst k ->
      node "int" [ int k ]
  | EStringConst s ->
      node "string" [ string s ]
  | ETuple es ->
      node "tuple" ( exprs es )
  | EAnnot (e, s) ->
      node "annot" [ expr e; scheme s ]
  | ERecordAccess (e, field) ->
      node "recordaccess" [ expr e; string field ]
  | ERecord fs ->
      node "record" (fields fs)
  | EArray fs ->
      node "array" (exprs fs)
  | EArrayAccess (e1, e2) ->
      node "arrayaccess" [ expr e1; expr e2 ]

and exprs es =
  List.map expr es

and stretch stretch =
  string stretch.Stretch.stretch_content

and branches brs =
  List.map branch brs

and branch br =
  node "branch" [ pat br.branchpat; expr br.branchbody ]

and fields fs =
  List.map field fs

and field (label, e) =
  node "field" [ string label; expr e ]

and pats ps =
  List.map pat ps

and pat = function
  | PUnit ->
      node "punit" []
  | PWildcard ->
      node "pwildcard" []
  | PVar x ->
      node "pvar" [ string x ]
  | PVarLocated x ->
      let x = Positions.value x in
      node "pvar" [ string x ]
  | PTuple ps ->
      node "ptuple" (pats ps)
  | PAnnot (p, t) ->
      node "pannot" [ pat p; typ t ]
  | PData (d, args) ->
      node "pdata" (string d :: pats args)
  | PRecord fps ->
      node "precord" (fpats fps)
  | POr ps ->
      node "por" (pats ps)

and fpats fps =
  List.map fpat fps

and fpat (_, p) =
  pat p

and patexprs pes =
  List.map patexpr pes

and patexpr (p, e) =
  node "patexpr" [ pat p; expr e ]

and string s =
  node s []

and int k =
  node (string_of_int k) []

and scheme _s =
  string "omitted" (* TEMPORARY to be completed, someday *)

and typ _t =
  string "omitted" (* TEMPORARY to be completed, someday *)

(* ------------------------------------------------------------------------- *)
(* Convert to a tree, then print the tree. *)

let expr e =
  print_tree X.f (expr e)

end
