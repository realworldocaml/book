(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* --------------------------------------------------------------------------- *)

open Printf
open Grammar
open SentenceParserAux

(* A delimiter. *)

type delimiter =
  string

(* An error message. *)

type message =
  string

(* A target tells us which state a sentence leads to, as well
   as which spurious reductions are performed at the end. *)

type target =
  ReferenceInterpreter.target

let target2state (s, _spurious) =
  s

(* A targeted sentence is a located sentence together with the target into
   which it leads.  *)

type targeted_sentence =
  located_sentence * target

(* A run is a series of targeted sentences or comments, followed with a
   delimiter (composed at least one blank line and possibly comments),
   followed with an error message. *)

type run = {
  (* A list of sentences. *)
  elements: targeted_sentence or_comment list;
  (* A delimiter. *)
  delimiter: delimiter;
  (* A message. *)
  message: message;
}

(* --------------------------------------------------------------------------- *)

(* [stream] turns a finite list of terminals into a stream of terminals,
   represented as a pair of a lexer and a lexing buffer, so as to be usable
   with Menhir's traditional API. *)

(* A traditional lexer returns a token and updates the fields of the lexing
   buffer with new positions. Here, we have no position information, so we
   keep the dummy positions that exist at initialization time. *)

(* When the finite list is exhausted, two plausible behaviors come to mind.

   One behavior consists in raising an exception. In that case, we are
   creating a finite stream, and it is up to the parser to not read past its
   end.

   Another behavior consists in returning a designated token. In that case, we
   are creating an infinite, eventually constant, stream.

   The choice between these two behaviors is somewhat arbitrary; furthermore,
   in the second case, the choice of the designated token is arbitrary as
   well. Here, we adopt the second behavior if and only if the grammar has an
   EOF token, and we use EOF as the designated token. Again, this is
   arbitrary, and could be changed in the future. *)

exception EndOfStream

include struct open Lexing

let stream (toks : Terminal.t list) : (lexbuf -> Terminal.t) * lexbuf =
  let toks = ref toks in
  let lexbuf = from_string "" in
  lexbuf.lex_start_p <- dummy_pos;
  lexbuf.lex_curr_p <- dummy_pos;
  let lexer _lexbuf =
    match !toks with
    | tok :: more ->
        toks := more;
        tok
    | [] ->
        match Terminal.eof with
        | Some eof ->
            eof
        | None ->
            raise EndOfStream
  in
  lexer, lexbuf

end

(* --------------------------------------------------------------------------- *)

(* [start sentence] returns the start symbol that we should use to interpret
   the sentence [sentence]. *)

(* If a start symbol was explicitly provided as part of the sentence, we use
   it. Otherwise, we use the grammar's unique start symbol, if there is
   one. *)

let start poss ((nto, _) : sentence) : Nonterminal.t =
  match nto with
  | Some nt ->
      nt
  | None ->
      match ProductionMap.is_singleton Lr1.entry with
      | None ->
          Error.error poss
            "because the grammar has multiple start symbols, each of the\n\
             sentences provided on the standard input channel must be of the\n\
             form: <start symbol>: <token>*"
      | Some (prod, _) ->
          match Production.classify prod with
          | Some nt ->
              nt
          | None ->
              assert false

(* --------------------------------------------------------------------------- *)

(* [interpret] interprets a sentence. *)

let interpret ((_, toks) as sentence) : unit =

  let nt = start [] sentence in

  (* Run the reference interpreter. This can produce a concrete syntax tree
     ([Some cst]), fail with a parser error ([None]), or fail with a lexer error
     ([EndOfStream]). *)

  (* In either case, we produce just one line of output, so it should be clear
     to the user which outcomes correspond to which sentences (should multiple
     sentences be supplied). *)

  let lexer, lexbuf = stream toks in
  let log = Logging.maybe Settings.trace in
  begin match
    ReferenceInterpreter.interpret nt log lexer lexbuf
  with

  | Some cst ->
      (* Success. *)
      printf "ACCEPT";
      if Settings.interpret_show_cst then begin
        print_newline();
        Cst.show stdout cst
      end

  | None ->
      (* Parser failure. *)
      printf "REJECT"

  | exception EndOfStream ->
      (* Lexer failure. *)
      printf "OVERSHOOT"

  end;
  print_newline()

(* --------------------------------------------------------------------------- *)

(* [interpret_error_aux] interprets a sentence, expecting it to end in an
   error. Failure or success is reported via two continuations. *)

let interpret_error_aux log poss ((_, terminals) as sentence) fail succeed =
  let nt = start poss sentence in
  let open ReferenceInterpreter in
  match check_error_path log nt terminals with
  | OInputReadPastEnd ->
      fail "no syntax error occurs."
  | OInputNotFullyConsumed ->
      fail "a syntax error occurs before the last token is reached."
  | OUnexpectedAccept ->
      fail "no syntax error occurs; in fact, this input is accepted."
  | OK target ->
      succeed nt terminals target

(* --------------------------------------------------------------------------- *)

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

let default_message =
  "<YOUR SYNTAX ERROR MESSAGE HERE>\n"

(* [print_messages_auto (nt, sentence, target)] displays the sentence
   defined by [nt] and [sentence], leading to the state [target]. It
   then displays a bunch of auto-generated comments. *)

let print_messages_auto (nt, sentence, target) : unit =

  (* Print the sentence. *)
  print_string (Sentence.print `Abstract (Some nt, sentence));

  (* If a token alias has been defined for every terminal symbol, then
     we can convert this sentence into concrete syntax. Do so. We make
     a few assumptions about the concrete syntax of the language:
       1. It is permitted to insert one space between two tokens;
       2. No token contains a newline character.
          (Our lexer enforces this assumption.)
     The name of the start symbol cannot be printed in a meaningful
     manner, so it is omitted. *)
  if Terminal.every_token_has_an_alias then
    printf
      "##\n\
       ## Concrete syntax: %s\n"
      (Sentence.print `Concrete (Some nt, sentence))
  ;

  (* Show which state this sentence leads to. *)
  let (s', spurious) = target in
  printf
    "##\n\
     ## Ends in an error in state: %d.\n\
     ##\n\
     %s##\n"
    (Lr1.number s')
    (* [Lr0.print] or [Lr0.print_closure] could be used here. The latter
       could sometimes be helpful, but is usually intolerably verbose. *)
    (Lr0.print "## " (Lr1.state s'))
  ;

  (* Show the known suffix of the stack in this state. *)
  printf
    "## The known suffix of the stack is as follows:\n\
     ##%s\n\
     ##\n"
    (StackSymbols.print_symbols (StackSymbolsShort.stack_symbols s'))
  ;

  (* If interpreting this sentence causes spurious reductions (that is,
     reductions that take place after the last terminal symbol has been
     shifted), say so, and show them. *)
  if spurious <> [] then begin
    printf
      "## WARNING: This example involves spurious reductions.\n\
       ## This implies that, although the LR(1) items shown above provide an\n\
       ## accurate view of the past (what has been recognized so far), they\n\
       ## may provide an INCOMPLETE view of the future (what was expected next).\n"
    ;
    List.iter (fun (s, prod) ->
      printf
        "## In state %d, spurious reduction of production %s\n"
        (Lr1.number s)
        (Production.print prod)
    ) spurious;
    printf "##\n"
  end

(* [print_messages_item] displays one data item. The item is of the form [nt,
   sentence, target], which means that beginning at the start symbol [nt], the
   sentence [sentence] ends in an error in the target state given by [target].
   [target] also contains information about which spurious reductions are
   performed at the end. The display obeys the [.messages] file format. *)

let print_messages_item (nt, sentence, target) : unit =
  (* Print the sentence, followed with auto-generated comments. *)
  print_messages_auto (nt, sentence, target);
  (* Then, print a proposed error message, between two blank lines. *)
  printf "\n%s\n" default_message

(* --------------------------------------------------------------------------- *)

(* [write_run run] writes a run into a new [.messages] file. Manually-written
   comments are preserved. New auto-generated comments are produced. *)

let write_run : run or_comment -> unit =
  function
  | Thing run ->
      (* First, print every sentence and human comment. *)
      List.iter (fun sentence_or_comment ->
        match sentence_or_comment with
        | Thing ((poss, ((_, toks) as sentence)), target) ->
            let nt = start poss sentence in
            (* Every sentence is followed with newly generated auto-comments. *)
            print_messages_auto (nt, toks, target)
        | Comment c ->
            print_string c
      ) run.elements;
      (* Then, print the delimiter, which must begin with a blank line
         and may include comments. *)
      print_string run.delimiter;
      (* Then, print the error message. *)
      print_string run.message
      (* No need for another blank line. It will be printed as part of a
         separate [Comment]. *)
  | Comment comments ->
      (* Must begin with a blank line. *)
      print_string comments

(* --------------------------------------------------------------------------- *)

(* [interpret_error] interprets a sentence, expecting it to end in an error.
   Failure or success is reported on the standard output channel. This is
   used by [--interpret-error]. *)

let fail msg =
  Error.error [] "%s" msg

let succeed nt terminals target =
  print_messages_item (nt, terminals, target);
  exit 0

let interpret_error sentence =
  let log = Logging.maybe Settings.trace in
  interpret_error_aux log [] sentence fail succeed

(* --------------------------------------------------------------------------- *)

(* The lexer [SentenceLexer] produces sentences that contain raw symbols,
   that is, strings that are not yet known to represent valid nonterminal
   or terminal symbols. This check is performed here. It either succeeds
   or signals an error in the category [c] and raises [Invalid]. *)

(* We also check that every sentence leads to an error state. *)

exception Invalid

let validate_nonterminal_symbol c (lid, startpos, endpos) =
  match Nonterminal.lookup lid with
  | exception Not_found ->
      Error.signal c [Positions.import (startpos, endpos)]
        "\"%s\" is not a known non-terminal symbol." lid;
      raise Invalid
  | nt ->
      if Nonterminal.is_user_start nt then
        nt
      else begin
        Error.signal c [Positions.import (startpos, endpos)]
          "\"%s\" is not a start symbol." lid;
        raise Invalid
      end

let validate_terminal_symbol c (uid, startpos, endpos) =
  try
    Terminal.lookup uid
  with Not_found ->
    Error.signal c [Positions.import (startpos, endpos)]
      "\"%s\" is not a known terminal symbol." uid;
    raise Invalid

let validate_sentence c (sentence : raw_sentence) : sentence =
  let (nto, terminals) = sentence in
  Option.map (validate_nonterminal_symbol c) nto,
  List.map (validate_terminal_symbol c) terminals

let validate_optional_sentence c =
  Option.map (validate_sentence c)

let validate_located_sentence c (poss, sentence) : targeted_sentence =
  (* First, validate every symbol. *)
  let sentence = validate_sentence c sentence in
  (* Then, check that this sentence leads to an error state. *)
  let log = Logging.never in
  interpret_error_aux log poss sentence
    (* failure: *)
    (fun msg ->
       Error.signal c poss
         "this sentence does not end with a syntax error, as it should:\n%s"
         msg;
       raise Invalid)
    (* success: *)
    (fun _nt _terminals target -> (poss, sentence), target)

(* [validate_entry] validates a list of located sentences or comments,
   as returned by [SentenceParser.entry]. If a sentence contains an
   error, then an error message is emitted, this sentence is removed
   from the list, and the validation process continues. *)

let validate_entry c entry : targeted_sentence or_comment list =
  Misc.filter_map (function
  | Thing sentence ->
      begin try
        Some (Thing (validate_located_sentence c sentence))
      with Invalid ->
        None
      end
  | Comment c ->
      Some (Comment c)
  ) entry

(* This wrapper causes Menhir to exit if at least one error was signaled
   during validation of [x] by [validate]. *)

let strictly validate x =
  Error.with_new_category (fun c -> validate c x)

(* --------------------------------------------------------------------------- *)

(* [setup()] returns a function [read] which reads one sentence from the
   standard input channel and immediately validates it. *)

let setup () : unit -> sentence option =

  let open Lexing in
  let lexbuf = from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

  let read () =
    match SentenceParser.optional_sentence SentenceLexer.lex lexbuf with
    | exception Parsing.Parse_error ->
        Error.error (Positions.lexbuf lexbuf) "ill-formed input sentence."
    | osentence ->
        strictly validate_optional_sentence osentence
  in

  read

(* --------------------------------------------------------------------------- *)

(* Display an informational message about the contents of a [.messages] file.  *)

let stats (runs : run or_comment list) =
  (* [s] counts the sample input sentences. [m] counts the error messages. *)
  let s = ref 0
  and m = ref 0 in
  List.iter (function
  | Thing { elements; _ } ->
      incr m;
      List.iter (function
      | Thing _ ->
          incr s
      | Comment _ ->
          ()
      ) elements
  | Comment _ ->
      ()
  ) runs;
  eprintf
    "Read %d sample input sentences and %d error messages.\n%!"
    !s !m;
  runs

(* --------------------------------------------------------------------------- *)

(* Reading a [.messages] file. *)

(* Our life is slightly complicated by the fact that the whitespace between
   two runs can contain comments, which we wish to preserve when performing
   [--update-errors]. *)

(* Sentences that do not pass validation are removed (and error messages are
   emitted). If one or more validation errors have occurred and if [strict]
   is [true], then we stop at the end. *)

let mkcomment c accu =
  if String.length c = 0 then accu else Comment c :: accu

let read_messages strict filename : run or_comment list =
  let open Segment in
  let c = Error.new_category() in
  (* Read and segment the file. *)
  let segments : (tag * string * Lexing.lexbuf) list = segment filename in
  (* Process the segments, two by two. We expect one segment to contain
     a non-empty series of sentences, and the next segment to contain
     free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] ->
        List.rev accu
    | (Whitespace, comments, _) :: segments ->
        loop (mkcomment comments accu) segments
    | (Segment, _, lexbuf) :: segments ->
        (* Read a series of located sentences. *)
        match SentenceParser.entry SentenceLexer.lex lexbuf with
        | exception Parsing.Parse_error ->
            Error.error
              [Positions.cpos lexbuf]
              "ill-formed sentence."
        | elements ->
            (* [elements] is a list of located raw sentences or comments.
               Validate it. Any sentences that do not pass validation are
               removed (and error messages are emitted). In an effort to
               be robust, we continue. If there remain zero sentences,
               then this entry is removed entirely. *)
            let elements = validate_entry c elements in
            (* In principle, we should now find a segment of whitespace
               followed with a segment of text. By construction, the two
               kinds of segments alternate. *)
            match segments with
            | (Whitespace, delimiter, _) ::
              (Segment, message, _) ::
              segments ->
                if count_things elements = 0 then
                  (* There remain zero sentences. Skip this entry. *)
                  loop accu segments
                else
                  (* Accumulate this entry. *)
                  let run = { elements; delimiter; message } in
                  loop (Thing run :: accu) segments
            | []
            | [ _ ] ->
                Error.error
                  (Positions.one (Lexing.lexeme_end_p lexbuf))
                  "missing a final message. I may be desynchronized."
            | (Segment, _, _) :: _
            | (Whitespace, _, _) :: (Whitespace, _, _) :: _ ->
                (* Should not happen, thanks to the alternation between the
                   two kinds of segments. *)
                assert false
  in
  let runs = stats (loop [] segments) in
  if strict then Error.exit_if c;
  runs

(* --------------------------------------------------------------------------- *)

(* [foreach_targeted_sentence f accu runs] iterates over the targeted
   sentences in the list [runs]. The function [f] receives the current
   accumulator, a targeted sentence, and the corresponding message, and
   must return an updated accumulator. *)

let foreach_targeted_sentence f accu (runs : run or_comment list) =
  List.fold_left (or_comment_fold (fun accu run ->
    List.fold_left (or_comment_fold (fun accu sentence ->
      f accu sentence run.message
    )) accu run.elements
  )) accu runs

(* --------------------------------------------------------------------------- *)

(* [message_table] converts a list of targeted runs to a table (a mapping) of
   states to located sentences and messages. Optionally, it can detect that
   two sentences lead to the same state, and report an error. *)

let message_table
    (detect_redundancy : bool)
    (runs : run or_comment list)
  : (located_sentence * message) Lr1.NodeMap.t =

  Error.with_new_category (fun c ->
    foreach_targeted_sentence (fun table (sentence2, target) message ->
      let s = target2state target in
      match Lr1.NodeMap.find s table with
      | sentence1, _ ->
          if detect_redundancy then
            Error.signal c (fst sentence1 @ fst sentence2)
                 "these sentences both cause an error in state %d."
                 (Lr1.number s);
          table
      | exception Not_found ->
          Lr1.NodeMap.add s (sentence2, message) table
    ) Lr1.NodeMap.empty runs
  )

(* --------------------------------------------------------------------------- *)

(* [compile_runs] converts a list of targeted runs to OCaml code that encodes
   a mapping of state numbers to error messages. The code is sent to the
   standard output channel. *)

let compile_runs filename (runs : run or_comment list) : unit =

  (* We wish to produce a function that maps a state number to a message.
     By convention, we call this function [message]. *)

  let name = "message" in

  let open IL in
  let open CodeBits in
  let default = {
    branchpat  = PWildcard;
    branchbody = eraisenotfound
  (* The default branch raises an exception, which can be caught by
     the user, who can then produce a generic error message. *)
  } in
  let branches =
    List.fold_left (or_comment_fold (fun branches run ->
      (* Create an or-pattern for these states. *)
      let states = Misc.filter_map (or_comment_filter_map (fun (_, target) ->
        let s = target2state target in
        pint (Lr1.number s)
      )) run.elements in
      (* Map all these states to this message. *)
      { branchpat = POr states;
        branchbody = EStringConst run.message } :: branches
    )) [ default ] runs
  in
  let messagedef = {
    valpublic = true;
    valpat = PVar name;
    valval = EFun ([ PVar "s" ], EMatch (EVar "s", branches))
  } in
  let program = [
    SIComment (sprintf
      "This file was auto-generated based on \"%s\"." filename);
    SIComment (sprintf
      "Please note that the function [%s] can raise [Not_found]." name);
    SIValDefs (false,
      [ messagedef ]);
  ] in

  (* Write this program to the standard output channel. *)

  let module P = Printer.Make (struct
    let f = stdout
    let locate_stretches = None
  end) in
  P.program program

(* --------------------------------------------------------------------------- *)

(* The rest of this file is the function [run], internally written as a functor
   [Run] for syntactic convenience. *)

module Run (X : sig end) = struct

(* --------------------------------------------------------------------------- *)

(* If [--interpret] is set, interpret the sentences found on the standard
   input channel, then stop, without generating a parser. *)

(* We read a series of sentences from the standard input channel. To allow
   interactive use, we interpret each sentence as soon as it is read. *)

let () =
  if Settings.interpret then
    let read = setup() in
    printf "Ready!\n%!";
    while true do
      match read() with
      | None ->
          exit 0
      | Some sentence ->
          interpret sentence
    done

(* --------------------------------------------------------------------------- *)

(* If [--interpret-error] is set, interpret one sentence found on the standard
   input channel, then stop, without generating a parser. *)

(* We read just one sentence, confirm that this sentence ends in an error, and
   (if that is the case) display the number of the state that is reached. *)

let () =
  if Settings.interpret_error then
    let read = setup() in
    match read() with
    | None ->
      exit 1 (* abnormal: no input *)
    | Some sentence ->
        interpret_error sentence (* never returns *)

(* --------------------------------------------------------------------------- *)

(* If [--compile-errors <filename>] is set, compile the error message
   descriptions found in file [filename] down to OCaml code, then stop. *)

let () =
  Settings.compile_errors |> Option.iter (fun filename ->

    (* Read the file. Compute the target state of every sentence. Stop if a
       sentence does not end in an error state, as expected. *)
    let strict = true in
    let runs : run or_comment list = read_messages strict filename in

    (* Build a mapping of states to located sentences. This allows us to
       detect if two sentences lead to the same state. *)
    let _ = message_table true runs in

    (* In principle, we would like to check whether this set of sentences is
       complete (i.e., covers all states where an error can arise), but this
       may be costly -- it requires running [LRijkstra]. Instead, we offer a
       separate facility for comparing two [.messages] files, one of which can
       be produced via [--list-errors]. This can be used to ensure
       completeness. *)

    (* Now, compile this information down to OCaml code. We wish to
       produce a function that maps a state number to a message. By
       convention, we call this function [message]. *)
    compile_runs filename runs;

    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* If two [--compare-errors <filename>] directives are provided, compare the
   two message descriptions files, and stop. We wish to make sure that every
   state that appears on the left-hand side appears on the right-hand side as
   well. *)

let compare_errors filename1 filename2 =

  (* Read both files. *)

  let strict = false in
  let runs1 = read_messages strict filename1
  and runs2 = read_messages strict filename2 in

  (* Convert the right-hand file to a table for quick lookup. *)

  let table2 = message_table false runs2 in

  (* There is no need to convert the left-hand file. In fact, not
     converting it to a table allows us to produce error messages
     in an order that respects the left-hand file. Indeed, the
     left-hand file is processed by the following loop: *)

  Error.with_new_category begin fun c ->
    foreach_targeted_sentence begin fun () (sentence1, target1) message1 ->

      let s = target2state target1 in

      (* 1. Check that the target state [s] appears in [table2]. *)

      match Lr1.NodeMap.find s table2 with

      | exception Not_found ->
          let poss1 = fst sentence1 in
          Error.signal c poss1
            "this sentence leads to an error in state %d.\n\
             No sentence that leads to this state exists in \"%s\"."
            (Lr1.number s) filename2

      (* 2. Check that [s] is mapped by [table1] and [table2] to the same
         error message. As an exception, if the message found in [table1] is
         the default message, then no comparison takes place. This allows
         using [--list-errors] and [--compare-errors] in conjunction to ensure
         that a [.messages] file is complete, without seeing warnings about
         different messages. *)

      | sentence2, message2 ->
          if message1 <> default_message && message1 <> message2 then begin
            let poss1 = fst sentence1
            and poss2 = fst sentence2 in
            Error.warning (poss1 @ poss2)
              "these sentences lead to an error in state %d.\n\
               The corresponding messages in \"%s\" and \"%s\" differ."
              (Lr1.number s) filename1 filename2
          end

    end () runs1
  end

let () =
  Settings.compare_errors |> Option.iter (fun (filename1, filename2) ->
    compare_errors filename1 filename2;
    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* Auxiliary functions for [merge_errors]. *)

(* [is_blank c] determines whether the comment [c] is blank. *)

let is_blank_char c =
  match c with
  | ' ' | '\n' | '\r' | '\t' ->
      true
  | _ ->
      false

let rec is_blank c i n =
  i = n || is_blank_char c.[i] && is_blank c (i+1) n

let is_blank c =
  is_blank c 0 (String.length c)

(* [remove_leading_blank_comment] removes a leading blank comment
   from a list. *)

let remove_leading_blank_comment xs =
  match xs with
  | [] ->
      []
  | Comment c :: xs when is_blank c ->
      xs
  | _ :: xs ->
      xs

(* A simple queue where [emit] inserts an element at the end and [elements]
   returns the current list of all elements and clears the queue. *)

module Q = struct

  let create () =
    let q = ref [] in
    let emit x =
      q := x :: !q
    and elements () =
      let xs = List.rev !q in
      q := [];
      xs
    in
    emit, elements

end

let conflict_comment filename =
  sprintf
    "#@ WARNING:\n\
     #@ The following sentence has been copied from \"%s\".\n\
     #@ It is redundant with a sentence that appears earlier in this file,\n\
     #@ so one of them must be removed.\n"
    filename

let toplevel_comment filename =
  sprintf
    "#@ WARNING:\n\
     #@ The following comment has been copied from \"%s\".\n\
     #@ It may need to be proofread, updated, moved, or removed.\n"
    filename

(* [is_default_run p run] tests whether [run] is a default run, that is, a
   run that consists of a single sentence and a default message. If so, it
   additionally tests whether the sentence's target state satisfies [p]. *)

let is_default_run (p : Lr1.node -> bool) (run : run) =
  run.message = default_message &&
  let sentences : targeted_sentence list =
    List.fold_left (or_comment_fold (fun xs x -> x :: xs)) [] run.elements
  in
  match sentences with
  | [ (_sentence, target) ] ->
      let s = target2state target in
      p s
  | _ ->
      false

(* [remove_default_runs] removes from the list [runs] the default runs
   whose target state satisfies [p]. *)

(* We make the assumption that a default run does not contain interesting
   comments, so it is not a problem to lose these comments when the run
   is removed. *)

let rec remove_default_runs p (runs : run or_comment list) =
  match runs with
  | [] ->
      []
  | Comment c :: runs ->
      Comment c :: remove_default_runs p runs
  | Thing run :: runs ->
      if is_default_run p run then
        remove_default_runs p (remove_leading_blank_comment runs)
      else
        Thing run :: remove_default_runs p runs

(* [keep_default_runs] keeps from the list [runs] just the default runs. *)

let keep_default_runs (runs : run or_comment list) =
  List.flatten (List.map (function
  | Comment _ ->
      []
  | Thing run ->
      if is_default_run (fun _ -> true) run then
        [ Thing run ]
      else
        []
  ) runs)

(* [targets run] is the set of target states of a run. *)

let targets (run : run) : Lr1.NodeSet.t =
  List.fold_left (or_comment_fold (fun states (_, target) ->
    let s = target2state target in
    Lr1.NodeSet.add s states
  )) Lr1.NodeSet.empty run.elements

(* [insert_runs inserts runs] inserts the content of the table [insert] into
   the list [runs] at appropriate points that are determined by the target
   states. *)

let insert_runs
    (inserts : run or_comment list Lr1.NodeMap.t)
    (runs : run or_comment list)
  : run or_comment list =

  let emit, emitted = Q.create() in
  runs |> List.iter begin function
  | Thing run ->
      (* Emit this run. *)
      emit (Thing run);
      (* Then, check if the states reached by the sentences in this run appear
         in the table [inserts]. If so, emit the corresponding data. *)
      targets run |> Lr1.NodeSet.iter begin fun s ->
        match Lr1.NodeMap.find s inserts with
        | data ->
            List.iter emit data
        | exception Not_found ->
            ()
      end
  | Comment c ->
      emit (Comment c)
  end;
  emitted()

(* [gather_followers] turns a list of things and comments into a list of
   things-followed-with-comments. Any leading comments are silently lost. *)

let rec gather_followers (xs : 'a or_comment list) : ('a * comment list) list =
  match xs with
  | Comment _ :: xs ->
      (* If there is a leading comment, ignore it. I believe that in a list
         of sentences, our current lexer never produces a leading comment.
         Indeed, a leading comment would be considered part of the previous
         toplevel comment. *)
      gather_followers xs
  | Thing x :: xs ->
      gather_followers_thing x [] xs
  | [] ->
      []

and gather_followers_thing x cs xs =
  match xs with
  | Comment c :: xs ->
      gather_followers_thing x (c :: cs) xs
  | _ ->
      (x, List.rev cs) :: gather_followers xs

(* [space xs] ensures that every thing is followed with a least one newline.
   If that is not the case, a blank line is inserted. This is unpleasant, but
   I have difficulty dealing with my own baroque file format. *)

let has_leading_newline = function
  | Comment c ->
      assert (c <> "");
      c.[0] = '\n'
  | Thing _ ->
      false

let rec space (xs : 'a or_comment list) : 'a or_comment list =
  match xs with
  | [] ->
      []
  | Thing x1 :: x2 :: xs when not (has_leading_newline x2) ->
      Thing x1 :: Comment "\n" :: space (x2 :: xs)
  | x :: xs ->
      x :: space xs

(* --------------------------------------------------------------------------- *)

(* If two [--merge-errors <filename>] directives are provided, compare the two
   message descriptions files and produce a merged .messages file. *)

(* The code is modeled after [compare_errors] above. When we find that an
   entry exists on the left-hand side yet is missing on the right-hand side,
   we note that it should be added. *)

(* If multiple sentences on the left-hand side share an error message, we
   attempt to preserve this feature when these sentences are copied to the
   right-hand side. This prevents us from using [foreach_targeted_sentence];
   we use two nested loops instead. *)

(* If the target state of a sentence on the left-hand side does not exist on
   the right-hand side, then this sentence/message pair is inserted at the end
   of the right-hand side.

   If the target state of a sentence on the left-hand side exists also on the
   right-hand side, albeit with a different message, then the left-hand
   sentence/message pair must be inserted into the right-hand side at a
   suitable position (that is, after the sentence/message pair that already
   exists on the right-hand side). Furthermore, if the sentence/message pair
   on the right-hand side involves the default message, then it should be
   removed and replaced. *)

let merge_errors filename1 filename2 =

  let strict = false in
  let runs1 = read_messages strict filename1
  and runs2 = read_messages strict filename2 in

  (* Remove the default runs on the right-hand side whose target state also
     appears on the left-hand side. We lose no information in doing so. *)
  let table1 = message_table false runs1 in
  let covered1 s = Lr1.NodeMap.mem s table1 in
  let runs2 = remove_default_runs covered1 runs2 in

  (* Remove the default runs on the left-hand side whose target state also
     appears on the right-hand side. Again, we lose nothing in doing so. *)
  let table2 = message_table false runs2 in
  let covered2 s = Lr1.NodeMap.mem s table2 in
  let runs1 = remove_default_runs covered2 runs1 in

  (* The default runs that remain on either side are unique. Set them aside,
     to be copied at the end. *)
  let default1 = keep_default_runs runs1
  and default2 = keep_default_runs runs2
  and runs1 = remove_default_runs (fun _ -> true) runs1
  and runs2 = remove_default_runs (fun _ -> true) runs2 in

  (* Use [append] when a run must be appended at the end. *)
  let (append : run or_comment -> unit), appended =
    Q.create()
  in

  (* Use [insert] when a run must be inserted at a specific point. *)
  let inserts : run or_comment list Lr1.NodeMap.t ref =
    ref Lr1.NodeMap.empty in

  let insert (s : Lr1.node) (newer : run or_comment list) =
    let earlier =  try Lr1.NodeMap.find s !inserts with Not_found -> [] in
    inserts := Lr1.NodeMap.add s (earlier @ newer) !inserts
  in

  runs1 |> List.iter begin fun entry ->
  match entry with

  | Comment c ->
      (* We do not want to lose the toplevel comments in the left-hand
         file, so we append them. This is not great, as they may become
         badly placed. We cannot really do better, though, as we do not
         know with what sentence they should be attached. (It may even
         be the case that they should be split and attached partly with
         the previous sentence and partly with the next one.) *)
      if not (is_blank c) then begin
        append (Comment (toplevel_comment filename1));
        append entry
      end

  | Thing run1 ->

    let message1 = run1.message in
    assert (message1 <> default_message);

    (* The sentences in the queue [retained] are to be associated with
       [message1], forming a run, which is to be inserted at the end. *)
    let retain, retained = Q.create() in

    (* The fact that [run1.elements] is a mixture of sentences and comments is
       problematic. We do not know which comments are intended to be paired
       with which sentences. We adopt the convention that a comment is
       associated with the sentence that precedes it. The auxiliary
       function [gather_followers] helps us follow this convention. *)

    run1.elements
    |> gather_followers
    |> List.iter begin fun ((sentence1, target1), comments) ->

        let comments = List.map (fun c -> Comment c) comments in
        let s = target2state target1 in
        match Lr1.NodeMap.find s table2 with

        | exception Not_found ->

            (* This sentence is missing on the right-hand side, so this pair
               of a sentence and message must be retained. The accompanying
               comments are preserved. *)
            retain (Thing (sentence1, target1));
            List.iter retain comments

        | _sentence2, message2 ->
            assert (message2 <> default_message);
            if message1 <> message2 then begin

              (* This sentence exists on the right-hand side, with a different
                 message, so this sentence and message must be inserted in the
                 right-hand side. We construct a singleton run (consisting of
                 just one sentence and one message) and schedule it for
                 insertion. If this sentence was part of a group of several
                 sentences that share a message, then this sharing is lost.
                 Preserving it would be difficult. The user can manually
                 recreate it if desired. *)

              let c = conflict_comment filename1 in
              let elements = Thing (sentence1, target1) :: comments in
              let run = { run1 with elements } in
              insert s [Comment c; Thing run]

            end

    end; (* end of the loop over the elements of this run *)

    (* If the queue [retained] is nonempty, then all of the sentences in it
       must be associated with [message1], forming a run, which must be
       inserted at the end. *)

    let retained = retained() in
    if retained <> [] then begin
      let elements = retained in
      let run = { run1 with elements } in
      append (Thing run)
    end

  end; (* end of the loop over runs *)

  (* The new data is constructed as follows: *)

  let runs =
    (* The non-default runs in [runs2], into which we insert some runs
       from [run1]. *)
    insert_runs !inserts runs2 @
    (* The non-default runs from [runs1] that we have decided to append
       at the end. *)
    appended() @
    (* The default runs from both sides. *)
    default1 @
    default2
  in

  (* Print. *)

  List.iter write_run (space runs)

let () =
  Settings.merge_errors |> Option.iter (fun (filename1, filename2) ->
    merge_errors filename1 filename2;
    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* If [--update-errors <filename>] is set, update the error message
   descriptions found in file [filename]. The idea is to re-generate
   the auto-comments, which are marked with ##, while leaving the
   rest untouched. *)

let () =
  Settings.update_errors |> Option.iter (fun filename ->

    (* Read the file. *)
    let strict = false in
    let runs : run or_comment list = read_messages strict filename in

    (* We might wish to detect if two sentences lead to the same state. We
       might also wish to detect if this set of sentences is incomplete,
       and complete it automatically. However, the first task is carried
       out by [--compile-errors] already, and the second task is carried
       out by [--list-errors] and [--compare-errors] together. For now,
       let's try and keep things as simple as possible. The task of
       [--update-errors] should be to update the auto-generated comments,
       without failing, and without adding or removing sentences. *)

    (* Now, write a new [.messages] to the standard output channel, with
       new auto-generated comments. *)
    List.iter write_run runs;

    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* If [--echo-errors <filename>] is set, echo the error sentences found in file
   [filename]. Do not echo the error messages or the comments. *)

(* In principle, we should able to run this command without even giving an .mly
   file name on the command line, and without building the automaton. This is
   not possible at the moment, because our code is organized in too rigid a
   manner. *)

let () =
  Settings.echo_errors |> Option.iter (fun filename ->

    (* Read the file. *)
    let strict = false in
    let runs : run or_comment list = read_messages strict filename in

    (* Echo. *)
    List.iter (or_comment_iter (fun run ->
      List.iter (or_comment_iter (fun ((_, sentence), _target) ->
        print_string (Sentence.print `Abstract sentence)
      )) run.elements
    )) runs;

    exit 0
  )

(* [--echo-errors-concrete] works like [--echo-errors], except every sentence
   is followed with an auto-generated comment that shows its concrete syntax. *)

let () =
  Settings.echo_errors_concrete |> Option.iter (fun filename ->

    (* Read the file. *)
    let strict = false in
    let runs : run or_comment list = read_messages strict filename in

    (* Echo. *)
    List.iter (or_comment_iter (fun run ->
      List.iter (or_comment_iter (fun ((_, sentence), _target) ->
        print_string (Sentence.print `Abstract sentence);
        if Terminal.every_token_has_an_alias then
          printf
            "## Concrete syntax: %s\n"
            (Sentence.print `Concrete sentence)
      )) run.elements
    )) runs;

    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* End of the functor [Run]. *)

end

let run () =
  let module R = Run(struct end) in
  ()
