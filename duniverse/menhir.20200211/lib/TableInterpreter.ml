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

module MakeEngineTable (T : TableFormat.TABLES) = struct

  type state =
      int

  let number s = s

  type token =
      T.token

  type terminal =
      int

  type nonterminal =
      int

  type semantic_value =
      Obj.t

  let token2terminal =
    T.token2terminal

  let token2value =
    T.token2value

  let error_terminal =
    T.error_terminal

  let error_value =
    Obj.repr ()

  (* The function [foreach_terminal] exploits the fact that the
     first component of [T.error] is [Terminal.n - 1], i.e., the
     number of terminal symbols, including [error] but not [#]. *)

  (* There is similar code in [InspectionTableInterpreter]. The
     code there contains an additional conversion of the type
     [terminal] to the type [xsymbol]. *)

  let rec foldij i j f accu =
    if i = j then
      accu
    else
      foldij (i + 1) j f (f i accu)

  let foreach_terminal f accu =
    let n, _ = T.error in
    foldij 0 n (fun i accu ->
      f i accu
    ) accu

  type production =
      int

  (* In principle, only non-start productions are exposed to the user,
     at type [production] or at type [int]. This is checked dynamically. *)
  let non_start_production i =
    assert (T.start <= i && i - T.start < Array.length T.semantic_action)

  let production_index i =
    non_start_production i;
    i

  let find_production i =
    non_start_production i;
    i

  let default_reduction state defred nodefred env =
    let code = PackedIntArray.get T.default_reduction state in
    if code = 0 then
      nodefred env
    else
      defred env (code - 1)

  let is_start prod =
    prod < T.start

  (* This auxiliary function helps access a compressed, two-dimensional
     matrix, like the action and goto tables. *)

  let unmarshal2 table i j =
    RowDisplacement.getget
      PackedIntArray.get
      PackedIntArray.get
      table
      i j

  let action state terminal value shift reduce fail env =
    match PackedIntArray.unflatten1 T.error state terminal with
    | 1 ->
        let action = unmarshal2 T.action state terminal in
        let opcode = action land 0b11
        and param = action lsr 2 in
        if opcode >= 0b10 then
          (* 0b10 : shift/discard *)
          (* 0b11 : shift/nodiscard *)
          let please_discard = (opcode = 0b10) in
          shift env please_discard terminal value param
        else
          (* 0b01 : reduce *)
          (* 0b00 : cannot happen *)
          reduce env param
    | c ->
        assert (c = 0);
        fail env

  let goto_nt state nt =
    let code = unmarshal2 T.goto state nt in
    (* code = 1 + state *)
    code - 1

  let goto_prod state prod =
    goto_nt state (PackedIntArray.get T.lhs prod)

  let maybe_goto_nt state nt =
    let code = unmarshal2 T.goto state nt in
    (* If [code] is 0, there is no outgoing transition.
       If [code] is [1 + state], there is a transition towards [state]. *)
    assert (0 <= code);
    if code = 0 then None else Some (code - 1)

  exception Error =
        T.Error

  type semantic_action =
      (state, semantic_value, token) EngineTypes.env ->
      (state, semantic_value)        EngineTypes.stack

  let semantic_action prod =
    (* Indexing into the array [T.semantic_action] is off by [T.start],
       because the start productions do not have entries in this array. *)
    T.semantic_action.(prod - T.start)

  (* [may_reduce state prod] tests whether the state [state] is capable of
     reducing the production [prod]. This information could be determined
     in constant time if we were willing to create a bitmap for it, but
     that would take up a lot of space. Instead, we obtain this information
     by iterating over a line in the action table. This is costly, but this
     function is not normally used by the LR engine anyway; it is supposed
     to be used only by programmers who wish to develop error recovery
     strategies. *)

  (* In the future, if desired, we could memoize this function, so as
     to pay the cost in (memory) space only if and where this function
     is actually used. We could also replace [foreach_terminal] with a
     function [exists_terminal] which stops as soon as the accumulator
     is [true]. *)

  let may_reduce state prod =
    (* Test if there is a default reduction of [prod]. *)
    default_reduction state
      (fun () prod' -> prod = prod')
      (fun () ->
        (* If not, then for each terminal [t], ... *)
        foreach_terminal (fun t accu ->
          accu ||
          (* ... test if there is a reduction of [prod] on [t]. *)
          action state t ()
            (* shift:  *) (fun () _ _ () _ -> false)
            (* reduce: *) (fun () prod' -> prod = prod')
            (* fail:   *) (fun () -> false)
            ()
        ) false
      )
      ()

  (* If [T.trace] is [None], then the logging functions do nothing. *)

  let log =
    match T.trace with Some _ -> true | None -> false

  module Log = struct

    open Printf

    let state state =
      match T.trace with
      | Some _ ->
          fprintf stderr "State %d:\n%!" state
      | None ->
          ()

    let shift terminal state =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Shifting (%s) to state %d\n%!" terminals.(terminal) state
      | None ->
          ()

    let reduce_or_accept prod =
      match T.trace with
      | Some (_, productions) ->
          fprintf stderr "%s\n%!" productions.(prod)
      | None ->
          ()

    let lookahead_token token startp endp =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Lookahead token is now %s (%d-%d)\n%!"
            terminals.(token)
            startp.Lexing.pos_cnum
            endp.Lexing.pos_cnum
      | None ->
          ()

    let initiating_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Initiating error handling\n%!"
      | None ->
          ()

    let resuming_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Resuming error handling\n%!"
      | None ->
          ()

    let handling_error state =
      match T.trace with
      | Some _ ->
          fprintf stderr "Handling error in state %d\n%!" state
      | None ->
          ()

  end

end
