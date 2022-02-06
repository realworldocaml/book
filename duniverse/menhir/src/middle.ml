(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* If [--random-sentence] was specified on the command line, obey it. *)

let () =
  Settings.random_sentence |> Option.iter begin fun (nt, goal, style) ->
    match Nonterminal.lookup nt with
    | exception Not_found ->
        Error.error [] "the nonterminal symbol %s does not exist." nt
    | nt ->
        let sentence = RandomSentenceGenerator.nonterminal nt goal in
        print_endline (Sentence.print style (Some nt, sentence));
        exit 0
  end

(* -------------------------------------------------------------------------- *)

end (* Run *)
