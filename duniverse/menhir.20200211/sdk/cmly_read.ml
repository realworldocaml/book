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

open Cmly_format
open Cmly_api

(* ------------------------------------------------------------------------ *)

(* Reading a .cmly file. *)

exception Error of string

let read (ic : in_channel) : grammar =
  (* .cmly file format: CMLY ++ version string ++ grammar *)
  let magic = "CMLY" ^ Version.version in
  try
    let m = really_input_string ic (String.length magic) in
    if m <> magic then
      raise (Error (Printf.sprintf "Invalid magic string in .cmly file.\n\
                 Expecting %S, but got %S." magic m))
    else
      (input_value ic : grammar)
  with
  | End_of_file  (* [really_input_string], [input_value] *)
  | Failure _ -> (* [input_value] *)
      raise (Error (Printf.sprintf "Invalid or damaged .cmly file."))

let read (filename : string) : grammar =
  let ic = open_in_bin filename in
  match read ic with
  | x ->
      close_in_noerr ic;
      x
  | exception exn ->
      close_in_noerr ic;
      raise exn

(* ------------------------------------------------------------------------ *)

(* Packaging the interval [0..count) as a module of type [INDEXED]. *)

module Index (P : sig
  val name: string (* for error messages only *)
  val count: int
end)
: INDEXED with type t = int
= struct

  type t = int

  let count = P.count

  let of_int n =
    if 0 <= n && n < count then n
    else invalid_arg (P.name ^ ".of_int: index out of bounds")

  let to_int n = n

  let iter f =
    for i = 0 to count - 1 do
      f i
    done

  let fold f x =
    let r = ref x in
    for i = 0 to count - 1 do
      r := f i !r
    done;
    !r

  let tabulate f =
    let a = Array.init count f in
    Array.get a

end

(* ------------------------------------------------------------------------ *)

(* Packaging a data structure of type [Cmly_format.grammar] as a module
   of type [Cmly_api.GRAMMAR]. *)

module Make (G : sig val grammar : grammar end) : GRAMMAR = struct
  open G

  type terminal    = int
  type nonterminal = int
  type production  = int
  type lr0         = int
  type lr1         = int
  type item        = production * int
  type ocamltype   = string
  type ocamlexpr   = string

  module Range = struct

    type t =
      Cmly_format.range

    let startp range =
      range.r_start

    let endp range =
      range.r_end

  end

  module Attribute = struct

    type t =
      Cmly_format.attribute

    let label attr =
      attr.a_label

    let has_label label attr =
      label = attr.a_label

    let payload attr =
      attr.a_payload

    let position attr =
      attr.a_position

  end

  module Grammar = struct
    let basename     = grammar.g_basename
    let preludes     = grammar.g_preludes
    let postludes    = grammar.g_postludes
    let entry_points = grammar.g_entry_points
    let attributes   = grammar.g_attributes
    let parameters   = grammar.g_parameters
  end

  module Terminal = struct
    let table = grammar.g_terminals
    let name       i = table.(i).t_name
    let kind       i = table.(i).t_kind
    let typ        i = table.(i).t_type
    let attributes i = table.(i).t_attributes
    include Index(struct
      let name = "Terminal"
      let count = Array.length table
    end)
  end

  module Nonterminal = struct
    let table = grammar.g_nonterminals
    let name         i = table.(i).n_name
    let mangled_name i = table.(i).n_mangled_name
    let kind         i = table.(i).n_kind
    let typ          i = table.(i).n_type
    let positions    i = table.(i).n_positions
    let nullable     i = table.(i).n_nullable
    let first        i = table.(i).n_first
    let attributes   i = table.(i).n_attributes
    include Index(struct
      let name = "Nonterminal"
      let count = Array.length table
    end)
  end

  type symbol = Cmly_format.symbol =
    | T of terminal
    | N of nonterminal

  let symbol_name ?(mangled=false) = function
    | T t ->
        Terminal.name t
    | N n ->
        if mangled then Nonterminal.mangled_name n
        else Nonterminal.name n

  type identifier = string

  module Action = struct
    type t = action
    let expr      t = t.a_expr
    let keywords  t = t.a_keywords
  end

  module Production = struct
    let table = grammar.g_productions
    let kind       i = table.(i).p_kind
    let lhs        i = table.(i).p_lhs
    let rhs        i = table.(i).p_rhs
    let positions  i = table.(i).p_positions
    let action     i = table.(i).p_action
    let attributes i = table.(i).p_attributes
    include Index(struct
      let name = "Production"
      let count = Array.length table
    end)
  end

  module Lr0 = struct
    let table = grammar.g_lr0_states
    let incoming i = table.(i).lr0_incoming
    let items    i = table.(i).lr0_items
    include Index(struct
      let name = "Lr0"
      let count = Array.length table
    end)
  end

  module Lr1 = struct
    let table = grammar.g_lr1_states
    let lr0         i = table.(i).lr1_lr0
    let transitions i = table.(i).lr1_transitions
    let reductions  i = table.(i).lr1_reductions
    include Index(struct
      let name = "Lr1"
      let count = Array.length table
    end)
  end

  module Print = struct

    let terminal ppf t =
      Format.pp_print_string ppf (Terminal.name t)

    let nonterminal ppf t =
      Format.pp_print_string ppf (Nonterminal.name t)

    let symbol ppf = function
      | T t -> terminal ppf t
      | N n -> nonterminal ppf n

    let mangled_nonterminal ppf t =
      Format.pp_print_string ppf (Nonterminal.name t)

    let mangled_symbol ppf = function
      | T t -> terminal ppf t
      | N n -> mangled_nonterminal ppf n

    let rec lengths l acc = function
      | [] ->
          if l = -1 then []
          else l :: lengths (-1) [] acc
      | [] :: rows ->
          lengths l acc rows
      | (col :: cols) :: rows ->
          lengths (max l (String.length col)) (cols :: acc) rows

    let rec adjust_length lengths cols =
      match lengths, cols with
      | l :: ls, c :: cs ->
          let pad = l - String.length c in
          let c =
            if pad = 0 then c
            else c ^ String.make pad ' '
          in
          c :: adjust_length ls cs
      | _, [] -> []
      | [], _ -> assert false

    let align_tabular rows =
      let lengths = lengths (-1) [] rows in
      List.map (adjust_length lengths) rows

    let print_line ppf = function
      | [] -> ()
      | x :: xs ->
          Format.fprintf ppf "%s" x;
          List.iter (Format.fprintf ppf " %s") xs

    let print_table ppf table =
      let table = align_tabular table in
      List.iter (Format.fprintf ppf "%a\n" print_line) table

    let annot_itemset annots ppf items =
      let last_lhs = ref (-1) in
      let prepare (p, pos) annot =
        let rhs =
          Array.map (fun (sym, id, _) ->
            if id <> "" && id.[0] <> '_' then
              "(" ^ id ^ " = " ^ symbol_name sym ^ ")"
            else symbol_name sym
          ) (Production.rhs p)
        in
        if pos >= 0 && pos < Array.length rhs then
          rhs.(pos) <- ". " ^ rhs.(pos)
        else if pos > 0 && pos = Array.length rhs then
          rhs.(pos - 1) <- rhs.(pos - 1) ^ " .";
        let lhs = Production.lhs p in
        let rhs = Array.to_list rhs in
        let rhs =
          if !last_lhs = lhs then
            "" :: "  |" :: rhs
          else begin
            last_lhs := lhs;
            Nonterminal.name lhs :: "::=" :: rhs
          end
        in
        if annot = [] then
          [rhs]
        else
          [rhs; ("" :: "" :: annot)]
      in
      let rec prepare_all xs ys =
        match xs, ys with
        | [], _ ->
            []
        | (x :: xs), (y :: ys) ->
            let z = prepare x y in
            z :: prepare_all xs ys
        | (x :: xs), [] ->
            let z = prepare x [] in
            z :: prepare_all xs []
      in
      print_table ppf (List.concat (prepare_all items annots))

    let itemset ppf t =
      annot_itemset [] ppf t

    let annot_item annot ppf item =
      annot_itemset [annot] ppf [item]

    let item ppf t =
      annot_item [] ppf t

    let production ppf t =
      item ppf (t, -1)

  end

end

module Read (X : sig val filename : string end) =
  Make (struct let grammar = read X.filename end)
