(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2016 Anton Bachin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

open Asttypes
open Parsetree

type assembler =
  lang:Common.lang ->
  loc:Location.t ->
  name:string ->
  Parsetree.expression Common.value list ->
  (Common.Label.t * Parsetree.expression) list



(* Helpers. *)

(* Given a parse tree [e], if [e] represents [_.txt s], where [s] is a string
   constant, evaluates to [Some s]. Otherwise, evaluates to [None]. *)
let to_txt = function
  | [%expr[%e? {pexp_desc = Pexp_ident f; _}]
      ( [%e? {pexp_desc = Pexp_ident f2; _}] [%e? arg])] -> begin
      match Longident.last f.txt, Longident.last f2.txt, Ast_convenience.get_str arg with
      | "txt", "return", Some s -> Some s
      | _ -> None
    end
  | _ -> None

(** Test if the expression is a txt containing only whitespaces. *)
let is_whitespace = function
  | Common.Val e -> begin
      match to_txt e with
      | Some s when String.trim s = "" -> true
      | _ -> false
    end
  | _ -> false

(* Given a list of parse trees representing children of an element, filters out
   all children that consist of applications of [txt] to strings containing
   only whitespace. *)
let filter_whitespace = List.filter (fun e -> not @@ is_whitespace e)

let filter_surrounding_whitespace children =
  let rec aux = function
    | [] -> []
    | h :: t when is_whitespace h -> aux t
    | l -> List.rev l
  in
  aux @@ aux children

(** Improve an assembler by first applying [filter_whitespace] on children
  Used by the [[@@reflect.filter_whitespace]] annotation *)
let comp_filter_whitespace assembler ~lang ~loc ~name children =
  assembler ~lang ~loc ~name (filter_whitespace children)

(* Given a parse tree and a string [name], checks whether the parse tree is an
   application of a function with name [name]. *)
let is_element_with_name name = function
  | Common.Val {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt}}, _)}
      when txt = name -> true
  | _ -> false

(* Partitions a list of elements according to [is_element_with_name name]. *)
let partition name children =
  List.partition (is_element_with_name name) children

(* Given the name [n] of a function in [Html_sigs.T], evaluates to
   ["Html." ^ n]. *)
let html local_name =
  Longident.Ldot (Lident Common.(implementation Html), local_name)



(* Generic. *)

let nullary ~lang:_ ~loc ~name children =
  if children <> [] then
    Common.error loc "%s should have no content" name;
  [Common.Label.nolabel, [%expr ()] [@metaloc loc]]

let unary ~lang ~loc ~name children =
  match children with
  | [child] ->
    let child = Common.wrap_value lang loc child in
    [Common.Label.nolabel, child]
  | _ -> Common.error loc "%s should have exactly one child" name

let star ~lang ~loc ~name:_ children =
  [Common.Label.nolabel, Common.list_wrap_value lang loc children]



(* Special-cased. *)

let head ~lang ~loc ~name children =
  let title, others = partition (html "title") children in

  match title with
  | [title] ->
    (Common.Label.nolabel, Common.wrap_value lang loc title) :: star ~lang ~loc ~name others
  | _ ->
    Common.error loc
      "%s element must have exactly one title child element" name

let figure ~lang ~loc ~name children =
  let caption, children =
    let rec is_first_figcaption = function
      | [] -> is_last_figcaption (List.rev children)
      | h :: t ->
        if is_whitespace h then is_first_figcaption t
        else if is_element_with_name (html "figcaption") h then
          `Top h,t
        else is_last_figcaption (List.rev children)
    and is_last_figcaption = function
      | [] -> `No, children
      | h :: t ->
        if is_whitespace h then is_last_figcaption t
        else if is_element_with_name (html "figcaption") h then
          `Bottom h, (List.rev t)
        else `No, children
    in
    is_first_figcaption children
  in
  begin match caption with
    | `No -> star ~lang ~loc ~name children
    | `Top elt -> 
      (Common.Label.labelled "figcaption",
       [%expr `Top [%e Common.wrap_value lang loc elt]])::
      (star ~lang ~loc ~name children)
    | `Bottom elt ->
      (Common.Label.labelled "figcaption",
       [%expr `Bottom [%e Common.wrap_value lang loc elt]])::
      (star ~lang ~loc ~name children)
  end [@metaloc loc]

let object_ ~lang ~loc ~name children =
  let params, others = partition (html "param") children in

  if params <> [] then
    (Common.Label.labelled "params", Common.list_wrap_value lang loc params) ::
    star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let audio_video ~lang ~loc ~name children =
  let sources, others = partition (html "source") children in

  if sources <> [] then
    (Common.Label.labelled "srcs", Common.list_wrap_value lang loc sources) ::
    star ~lang ~loc ~name others
  else
    star ~lang ~loc ~name others

let table ~lang ~loc ~name children =
  let caption, others = partition (html "caption") children in
  let columns, others = partition (html "colgroup") others in
  let thead, others = partition (html "thead") others in
  let tfoot, others = partition (html "tfoot") others in

  let one label = function
    | [] -> []
    | [child] -> [Common.Label.labelled label, Common.wrap_value lang loc child]
    | _ -> Common.error loc "%s cannot have more than one %s" name label
  in

  let columns =
    if columns = [] then []
    else [Common.Label.labelled "columns", Common.list_wrap_value lang loc columns]
  in

  (one "caption" caption) @
    columns @
    (one "thead" thead) @
    (one "tfoot" tfoot) @
    (star ~lang ~loc ~name others)

let fieldset ~lang ~loc ~name children =
  let legend, others = partition (html "legend") children in

  match legend with
  | [] -> star ~lang ~loc ~name others
  | [legend] ->
    (Common.Label.labelled "legend", Common.wrap_value lang loc legend)::
      (star ~lang ~loc ~name others)
  | _ -> Common.error loc "%s cannot have more than one legend" name

let datalist ~lang ~loc ~name children =
  let options, others = partition (html "option") children in

  let children =
    begin match others with
    | [] ->
      Common.Label.labelled "children",
      [%expr `Options [%e Common.list_wrap_value lang loc options]]

    | _ ->
      Common.Label.labelled "children",
      [%expr `Phras [%e Common.list_wrap_value lang loc children]]
    end [@metaloc loc]
  in

  children::(nullary ~lang ~loc ~name [])


let script ~lang ~loc ~name children =
  match children with
  | [] ->
    let child = Common.txt ~loc ~lang "" in
    [Common.Label.Nolabel, child]
  | [child] ->
    let child = Common.wrap_value lang loc child in
    [Common.Label.nolabel, child]
  | _ -> Common.error loc "%s can have at most one child" name

let details ~lang ~loc ~name children =
  let summary, others = partition (html "summary") children in

  match summary with
  | [summary] ->
    (Common.Label.nolabel, Common.wrap_value lang loc summary)::
      (star ~lang ~loc ~name others)
  | _ -> Common.error loc "%s must have exactly one summary child" name

let menu ~lang ~loc ~name children =
  let children =
    Common.Label.labelled "child",
    [%expr `Flows [%e Common.list_wrap_value lang loc children]]
      [@metaloc loc]
  in
  children::(nullary ~lang ~loc ~name [])

let html ~lang ~loc ~name children =
  let head, others = partition (html "head") children in
  let body, others = partition (html "body") others in

  match head, body, others with
  | [head], [body], [] ->
    [Common.Label.nolabel, Common.wrap_value lang loc head;
     Common.Label.nolabel, Common.wrap_value lang loc body]
  | _ ->
    Common.error loc
      "%s element must have exactly head and body child elements" name
