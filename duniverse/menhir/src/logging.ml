(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Grammar

module type LOG =
  MenhirLib.EngineTypes.LOG
    with type state := Lr1.node
     and type terminal := Terminal.t
     and type production := Production.index

type log =
  (module LOG)

module Make (X : sig

  (* [show] determines whether the messages must be sent to [stderr]
     or suppressed. *)
  val show : bool

  (* The reference [count] is used to count the number of messages,
     regardless of whether these messages are visible. Each message
     occupies one line. *)
  val count : int ref

end) = struct
  open X

  (* [emit] does nothing if [show] is false. *)

  let emit format =
    ksprintf (fun s -> if show then eprintf "%s%!" s) format

  (* [newline] increments [count], and performs the bounds check,
      even if [show] is false. *)

  let newline() =
    incr count;
    emit "\n"

  let state s =
    emit "State %d:" (Lr1.number s);
    newline()

  let shift tok s' =
    emit "Shifting (%s) to state %d" (Terminal.print tok) (Lr1.number s');
    newline()

  let reduce_or_accept prod =
    match Production.classify prod with
    | Some _ ->
       emit "Accepting";
       newline()
    | None ->
       emit "Reducing production %s" (Production.print prod);
       newline()

  let lookahead_token tok startp endp =
    emit "Lookahead token is now %s (%d-%d)"
      (Terminal.print tok)
      startp.Lexing.pos_cnum
      endp.Lexing.pos_cnum;
    newline()

  let initiating_error_handling () =
    emit "Initiating error handling";
    newline()

  let resuming_error_handling () =
    emit "Resuming error handling";
    newline()

  let handling_error s =
    emit "Handling error in state %d" (Lr1.number s);
    newline()

end

module Always =
  Make(struct
    let show = true
    let count = ref 0 (* inaccessible *)
  end)

module Never =
  Make(struct
    let show = false
    let count = ref 0 (* inaccessible *)
  end)

let always = (module Always : LOG)

let never  = (module Never  : LOG)

let maybe b =
  if b then always else never
