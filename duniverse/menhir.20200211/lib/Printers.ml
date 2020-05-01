(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

module Make
  (I : IncrementalEngine.EVERYTHING)
  (User : sig
    val print: string -> unit
    val print_symbol: I.xsymbol -> unit
    val print_element: (I.element -> unit) option
  end)
= struct

  let arrow = " -> "
  let dot = "."
  let space = " "
  let newline = "\n"

  open User
  open I

  (* Printing a list of symbols. An optional dot is printed at offset
     [i] into the list [symbols], if this offset lies between [0] and
     the length of the list (included). *)

  let rec print_symbols i symbols =
    if i = 0 then begin
      print dot;
      print space;
      print_symbols (-1) symbols
    end
    else begin
      match symbols with
      | [] ->
          ()
      | symbol :: symbols ->
          print_symbol symbol;
          print space;
          print_symbols (i - 1) symbols
    end

  (* Printing an element as a symbol. *)

  let print_element_as_symbol element =
    match element with
    | Element (s, _, _, _) ->
        print_symbol (X (incoming_symbol s))

  (* Some of the functions that follow need an element printer. They use
     [print_element] if provided by the user; otherwise they use
     [print_element_as_symbol]. *)

  let print_element =
    match print_element with
    | Some print_element ->
        print_element
    | None ->
        print_element_as_symbol

  (* Printing a stack as a list of symbols. Stack bottom on the left,
     stack top on the right. *)

  let rec print_stack env =
    match top env, pop env with
    | Some element, Some env ->
        print_stack env;
        print space;
        print_element element
    | _, _ ->
        ()

  let print_stack env =
    print_stack env;
    print newline

  (* Printing an item. *)

  let print_item (prod, i) =
    print_symbol (lhs prod);
    print arrow;
    print_symbols i (rhs prod);
    print newline

  (* Printing a list of symbols (public version). *)

  let print_symbols symbols =
    print_symbols (-1) symbols

  (* Printing a production (without a dot). *)

  let print_production prod =
    print_item (prod, -1)

  (* Printing the current LR(1) state. *)

  let print_current_state env =
    print "Current LR(1) state: ";
    match top env with
    | None ->
        print "<some initial state>"; (* TEMPORARY unsatisfactory *)
        print newline
    | Some (Element (current, _, _, _)) ->
        print (string_of_int (number current));
        print newline;
        List.iter print_item (items current)

  let print_env env =
    print_stack env;
    print_current_state env;
    print newline

end
