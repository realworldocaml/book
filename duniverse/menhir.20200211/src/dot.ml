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

open Printf

(* ------------------------------------------------------------------------- *)

(* Type definitions. *)

type size =
    float * float (* in inches *)

type orientation =
  | Portrait
  | Landscape

type rankdir =
  | LeftToRight
  | TopToBottom

type ratio =
  | Compress
  | Fill
  | Auto

type style =

    (* Both nodes and edges. *)

  | Solid
  | Dashed
  | Dotted
  | Bold
  | Invisible

    (* Nodes only. *)

  | Filled
  | Diagonals
  | Rounded

type shape =
  | Box
  | Oval
  | Circle
  | DoubleCircle
      (* there are many others, let's stop here *)

(* ------------------------------------------------------------------------- *)

(* Basic printers. *)

let print_style = function
  | None ->
      ""
  | Some style ->
      let style =
        match style with
        | Solid ->
            "solid"
        | Dashed ->
            "dashed"
        | Dotted ->
            "dotted"
        | Bold ->
            "bold"
        | Invisible ->
            "invis"
        | Filled ->
            "filled"
        | Diagonals ->
            "diagonals"
        | Rounded ->
            "rounded"
      in
      sprintf ", style = %s" style

let print_shape = function
  | None ->
      ""
  | Some shape ->
      let shape =
        match shape with
        | Box ->
            "box"
        | Oval ->
            "oval"
        | Circle ->
            "circle"
        | DoubleCircle ->
            "doublecircle"
      in
      sprintf ", shape = %s" shape

(* ------------------------------------------------------------------------- *)

(* The graph printer. *)

module Print (G : sig

  type vertex

  val name: vertex -> string

  val successors: (?style:style -> label:string -> vertex -> unit) -> vertex -> unit

  val iter: (?shape:shape -> ?style:style -> label:string -> vertex -> unit) -> unit

end) = struct

  let print
      ?(directed = true)
      ?size
      ?(orientation = Landscape)
      ?(rankdir = LeftToRight)
      ?(ratio = Compress)
      (f : out_channel)
      =

    fprintf f "%s G {\n" (if directed then "digraph" else "graph");
    Option.iter (fun (hsize, vsize) ->
      fprintf f "size=\"%f, %f\";\n" hsize vsize
    ) size;
    begin match orientation with
      | Portrait ->
          fprintf f "orientation = portrait;\n"
      | Landscape ->
          fprintf f "orientation = landscape;\n"
    end;
    begin match rankdir with
      | LeftToRight ->
          fprintf f "rankdir = LR;\n"
      | TopToBottom ->
          fprintf f "rankdir = TB;\n"
    end;
    begin match ratio with
      | Compress ->
          fprintf f "ratio = compress;\n"
      | Fill ->
          fprintf f "ratio = fill;\n"
      | Auto ->
          fprintf f "ratio = auto;\n"
    end;

    G.iter (fun ?shape ?style ~label vertex ->
      fprintf f "%s [ label=\"%s\"%s%s ] ;\n"
        (G.name vertex)
        label
        (print_style style)
        (print_shape shape)
    );

    G.iter (fun ?shape ?style ~label source ->
      ignore shape; (* avoid unused variable warnings *)
      ignore style;
      ignore label;
      G.successors (fun ?style ~label destination ->
        fprintf f "%s %s %s [ label=\"%s\"%s ] ;\n"
          (G.name source)
          (if directed then "->" else "--")
          (G.name destination)
          label
          (print_style style)
      ) source
    );

    fprintf f "\n}\n"

end

