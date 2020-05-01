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

open Grammar

(* -------------------------------------------------------------------------- *)

(* This is a data structure for linear derivation trees. These are derivation
   trees that are list-like (that is, they do not branch), because a single
   path is of interest.

   A tree is either empty or formed of a non-terminal symbol at the root and a
   forest below the root.

   A forest is an ordered list of elements. However, its elements are not
   trees, as one would perhaps expect. Because we are interested in *linear*
   derivation trees, only one element of the forest receives focus and is a
   tree. All other elements remain un-expanded, so they are just symbols.

   In other words, a linear derivation tree is roughly just a list of levels,
   where each forest corresponds to one level. *)

type 'focus level = {
    prefix: Symbol.t list;
    focus: 'focus;
    suffix: Symbol.t list;
    comment: string
  }

type tree =
  | TEmpty
  | TRooted of Symbol.t * forest

and forest =
    tree level

(* We make use of contexts with a forest-shaped hole. We have tree contexts
   and forest contexts.

   Tree contexts do not have a case for holes, since we work with
   forest-shaped holes only. Forest contexts have one. *)

type ctree =
  | CRooted of Symbol.t * cforest

and cforest =
  | CHole
  | CCons of ctree level

(* Make a few types visible to clients. *)

type t =
    forest

type context =
    cforest

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let rec array_to_list a i j =
  if i = j then
    []
  else
    a.(i) :: array_to_list a (i + 1) j

let empty =
  {
    prefix = [];
    focus = TEmpty;
    suffix = [];
    comment = ""
  }

let tail pos rhs =
  let length = Array.length rhs in
  assert (pos < length);
  {
    prefix = [];
    focus = TEmpty;
    suffix = array_to_list rhs pos length;
    comment = ""
  }

let build pos rhs forest comment =
  let length = Array.length rhs in
  assert (pos < length);
  match rhs.(pos) with
  | Symbol.T _ ->
      assert false
  | Symbol.N _ as symbol ->
      {
        prefix = [];
        focus = TRooted (symbol, forest);
        suffix = array_to_list rhs (pos + 1) length;
        comment = (match comment with None -> "" | Some comment -> comment)
      }

let prepend symbol forest =
  { forest with
    prefix = symbol :: forest.prefix }

(* -------------------------------------------------------------------------- *)

(* Display. *)

let buffer =
  Buffer.create 32768

let rec print_blank k =
  if k > 0 then begin
    Buffer.add_char buffer ' ';
    print_blank (k - 1)
  end

let print_symbol symbol =
  let word = Symbol.print symbol in
  Buffer.add_string buffer word;
  Buffer.add_char buffer ' ';
  String.length word + 1

let print_symbols symbols =
  List.fold_left (fun offset symbol ->
    offset + print_symbol symbol
  ) 0 symbols

let print_level print_focus_root print_focus_remainder offset forest =
  print_blank offset;
  let offset = offset + print_symbols forest.prefix in
  print_focus_root forest.focus;
  let (_ : int) = print_symbols forest.suffix in
  if String.length forest.comment > 0 then begin
    Buffer.add_string buffer "// ";
    Buffer.add_string buffer forest.comment
  end;
  Buffer.add_char buffer '\n';
  print_focus_remainder offset forest.focus

let print_tree_root = function
  | TEmpty ->
      Buffer.add_string buffer ". "
  | TRooted (symbol, _) ->
      let (_ : int) = print_symbol symbol in
      ()

let rec print_forest offset forest =
  print_level print_tree_root print_tree_remainder offset forest

and print_tree_remainder offset = function
  | TEmpty ->
      ()
  | TRooted (_, forest) ->
      print_forest offset forest

let print_ctree_root = function
  | CRooted (symbol, _) ->
      let (_ : int) = print_symbol symbol in
      ()

let rec print_cforest offset cforest =
  match cforest with
  | CHole ->
      print_blank offset;
      Buffer.add_string buffer "(?)\n"
  | CCons forest ->
      print_level print_ctree_root print_ctree_remainder offset forest

and print_ctree_remainder offset = function
  | CRooted (_, cforest) ->
      print_cforest offset cforest

let wrap print channel x =
  Buffer.clear buffer;
  print 0 x;
  Buffer.output_buffer channel buffer

let print =
  wrap print_forest

let printc =
  wrap print_cforest

(* -------------------------------------------------------------------------- *)

(* [punch] turns a (tree or forest) into a pair of a (tree or forest) context
   and a residual forest, where the context is chosen maximal. In other words,
   the residual forest consists of a single level -- its focus is [TEmpty]. *)

let rec punch_tree tree : (ctree * forest) option =
  match tree with
  | TEmpty ->
      None
  | TRooted (symbol, forest) ->
      let forest1, forest2 = punch_forest forest in
      Some (CRooted (symbol, forest1), forest2)

and punch_forest forest : cforest * forest =
  match punch_tree forest.focus with
  | None ->
      CHole, forest
  | Some (ctree1, forest2) ->
      CCons {
        prefix = forest.prefix;
        focus = ctree1;
        suffix = forest.suffix;
        comment = forest.comment
      }, forest2

(* [fill] fills a (tree or forest) context with a forest so as to produce
   a new (tree or forest). *)

let rec fill_tree ctree1 forest2 : tree =
  match ctree1 with
  | CRooted (symbol1, cforest1) ->
      TRooted (symbol1, fill_forest cforest1 forest2)

and fill_forest cforest1 forest2 : forest =
  match cforest1 with
  | CHole ->
      forest2
  | CCons level1 ->
      {
        prefix = level1.prefix;
        focus = fill_tree level1.focus forest2;
        suffix = level1.suffix;
        comment = level1.comment
      }

(* [common] factors the maximal common (tree or forest) context out of a pair
   of a (tree or forest) context and a (tree or forest). It returns the (tree
   or forest) context as well as the residuals of the two parameters. *)

let rec common_tree ctree1 tree2 : (ctree * cforest * forest) option =
  match ctree1, tree2 with
  | CRooted _, TEmpty ->
      None
  | CRooted (symbol1, cforest1), TRooted (symbol2, forest2) ->
      if Symbol.equal symbol1 symbol2 then
        let cforest, cforest1, forest2 =
          common_forest cforest1 forest2
        in
        Some (CRooted (symbol1, cforest), cforest1, forest2)
      else
        None

and common_forest cforest1 forest2 : cforest * cforest * forest =
  match cforest1 with
  | CHole ->
      CHole, cforest1, forest2
  | CCons forest1 ->
      if Symbol.lequal forest1.prefix forest2.prefix
      && Symbol.lequal forest1.suffix forest2.suffix
      && forest1.comment = forest2.comment
      then begin
        match common_tree forest1.focus forest2.focus with
        | None ->
            CHole, cforest1, forest2
        | Some (ctree, csubforest1, subforest2) ->
            let cforest = {
              prefix = forest1.prefix;
              focus = ctree;
              suffix = forest1.suffix;
              comment = forest1.comment
            } in
            CCons cforest, csubforest1, subforest2
      end
      else
        CHole, cforest1, forest2

(* [factor] factors the maximal common forest context out of a nonempty family
   of forests. We assume that the family is represented as a map indexed by
   items, because this is convenient for the application that we have in mind,
   but this assumption is really irrelevant. *)

let factor forests =
  match
    Item.Map.fold (fun item forest accu ->
      match accu with
      | None ->

          (* First time through the loop, so [forest] is the first forest
             that we examine. Punch it, so as to produce a maximal forest
             context and a residual forest. *)

          let context, residual = punch_forest forest in
          Some (context, Item.Map.singleton item residual)

      | Some (context, residuals) ->

          (* Another iteration through the loop. [context] and [residuals] are
             the maximal common context and the residuals of the forests
             examined so far. *)

          (* Combine the common context obtained so far with the forest at hand.
             This yields a new, smaller common context, as well as residuals for
             the previous common context and for the forest at hand. *)

          let context, contextr, forestr = common_forest context forest in

          (* The residual forests are now: (i) the residual forest [forestr];
             and (ii) the previous residual forests [residuals], each of which
             must be placed with the residual context [contextr]. *)

          let residuals =
            Item.Map.add item forestr (Item.Map.map (fill_forest contextr) residuals)
          in

          Some (context, residuals)

    ) forests None
  with
  | None ->
      assert false (* parameter [forests] was an empty map *)
  | Some (context, residuals) ->
      context, residuals

