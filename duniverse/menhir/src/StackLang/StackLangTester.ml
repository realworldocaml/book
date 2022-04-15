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
open StackLang

let debug = false

(* -------------------------------------------------------------------------- *)

(* The possible outcomes of an execution. *)

type outcome =
  | Accepted
  | Rejected
  | Overshoot
  | Crash of exn
  | TraceTooLong of int (* bound *)

let print_outcome = function
  | Accepted ->
      "accepts this sentence"
  | Rejected ->
      "rejects this sentence"
  | Overshoot ->
      "reads past the end of the input stream"
  | Crash e ->
      sprintf "fails (%s)" (Printexc.to_string e)
  | TraceTooLong bound ->
      sprintf "logs more than %d lines" bound

(* -------------------------------------------------------------------------- *)

(* [run_reference sentence] uses the reference interpreter to parse the
   sentence [sentence]. It returns a pair of an outcome and the length
   of the trace (regardless of whether the trace is actually visible). *)

let run_reference nt sentence : outcome * int =
  let lexer, lexbuf = Interpret.stream sentence in
  let count = ref 0 in
  let module Log =
    Logging.Make(struct
      let show = Settings.trace
      let count = count
    end)
  in
  let log = (module Log : Logging.LOG) in
  let outcome =
    match ReferenceInterpreter.interpret nt log lexer lexbuf with
    | Some _cst ->
        Accepted
    | None ->
        Rejected
    | exception Interpret.EndOfStream ->
        Overshoot
    | exception e ->
        Crash e
  in
  outcome, !count

(* [run_candidate candidate bound sentence] uses the interpreter [candidate]
   to parse the sentence [sentence]. The parameter [bound] bounds the number
   of log messages that the candidate is allowed to emit. *)

exception BoundExceeded

let run_candidate candidate bound sentence : outcome * int =
  let lexer, lexbuf = Interpret.stream sentence in
  let count = ref 0 in
  let log s =
    if Settings.trace then prerr_string s;
    incr count;
    if bound < !count then
      raise BoundExceeded
  in
  let outcome =
    match candidate log lexer lexbuf with
    | StackLangInterpreter.Accept ->
        Accepted
    | StackLangInterpreter.Reject _s ->
        Rejected
    | StackLangInterpreter.AbortBySemanticAction ->
        Rejected
    | exception BoundExceeded ->
        TraceTooLong bound
    | exception Interpret.EndOfStream ->
        Overshoot
    | exception e ->
        Crash e
  in
  outcome, !count

(* -------------------------------------------------------------------------- *)

(* [test m program nt sentence] tests the program [program] with the start
   symbol [nt] and the sentence [sentence]. Dynamic execution counts are
   added to the record [m]. *)

let test m program nt sentence =
  (* In debug mode, show the sentence that we are about to test. *)
  if debug then
    eprintf "StackLangTester: About to test this sentence:\n  %s%!"
      (Sentence.print `Abstract (Some nt, sentence));
  (* Run the reference interpreter. Measure the length of its trace. *)
  if debug then eprintf "StackLangTester: Running the reference interpreter...\n%!";
  let reference, bound = run_reference nt sentence in
  (* Construct the candidate interpreter. *)
  let name = Grammar.Nonterminal.print true nt in
  let label = StringMap.find name program.entry in
  let candidate = StackLangInterpreter.interpret m program label in
  (* Run the candidate interpreter. Use [count] as a bound on the number
     of log messages that it is allowed to emit. This way, a bug where
     the candidate interpreter diverges (or emits an incorrect number
     of trace messages) is detected. *)
  if debug then eprintf "StackLangTester: Running the StackLang program...\n%!";
  let candidate, count = run_candidate candidate bound sentence in
  (* Compare the outcomes and the lengths of the traces. *)
  if reference <> candidate || bound <> count then begin
    eprintf "StackLangTester: Error: reference and candidate disagree.\n%s"
      (Sentence.print `Abstract (Some nt, sentence));
    if reference <> candidate then begin
      eprintf "The reference interpreter %s, whereas\n"
        (print_outcome reference);
      eprintf "the StackLang program %s.\n"
        (print_outcome candidate);
    end
    else begin
      eprintf "The reference interpreter emits %d log messages, whereas\n"
        bound;
      eprintf "the StackLang program emits %d messages.\n"
        count;
    end;
    exit 1
  end
  else if debug then eprintf "The interpreters agree.\n%!"
  (* We do not construct or compare the actual traces, so it could be the
     case that the trace produced by the candidate has the correct length
     yet is incorrect. *)

(* -------------------------------------------------------------------------- *)

(* [test program nt sentence] tests the program [program] with the start
   symbol [nt] and with a number of sentences. *)

(* We sample sentences of increasing sizes, picking at most [k] sentences of
   each size, until a certain size threshold is reached. The current settings
   lead to testing 156 sentences of length up to 9535. *)

let k, threshold = (2, 10000)

let test program nt =
  if debug then
    eprintf
      "StackLangTester: about to test start symbol %s...\n%!"
      (Nonterminal.print false nt);
  (* Sample sentences of increasing sizes, picking at most [m] sentences
     of each size, until a total of [n] sentences is reached or the size
     threshold is reached. *)
  let m = StackLangMeasure.zero() in
  let count, size = ref 0, ref 10 in
  let prev = ref !size in
  let total = ref 0 in
  while !size < threshold do
    for _ = 1 to k do
      let sentence = RandomSentenceGenerator.nonterminal nt !size in
      test m program nt sentence;
      total := !total + List.length sentence;
    done;
    count := !count + k;
    prev := !size;
    size := !size + !size / 10
  done;
  (* Log execution counts. *)
  if true then begin
    let name = sprintf "%s.dcount" Settings.base in
    let f = open_out name in
    fprintf f "Dynamic execution counts (%d tokens read).\n" !total;
    StackLangMeasure.adjust_total m;
    StackLangMeasure.print f m;
    close_out f
  end;
  (* Log a success message. *)
  if debug then
    eprintf
      "StackLangTester: Tested %d sentences of length up to %d.\n"
      !count !prev

(* -------------------------------------------------------------------------- *)

(* [test program nt sentence] tests the program [program]. *)

let test program =
  (* For each start symbol [nt], test this entry point. *)
  Lr1.entry |> ProductionMap.iter begin fun _prod s ->
    let nt = Lr1.nt_of_entry s in
    test program nt
  end;
  Time.tick "Testing the StackLang program"
