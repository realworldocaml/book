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

module I = Invariant (* artificial dependency *)
module D = Default   (* artificial dependency *)

(* --------------------------------------------------------------------------- *)

open Grammar
open SentenceParserAux

(* A delimiter. *)

type delimiter =
  string

(* An error message. *)

type message =
  string

(* A run is a series of sentences or comments,
   followed with a delimiter (at least one blank line; comments),
   followed with an error message. *)

type run =
  located_sentence or_comment list *
  delimiter *
  message

(* A targeted sentence is a located sentence together with the target into
   which it leads. A target tells us which state a sentence leads to, as well
   as which spurious reductions are performed at the end. *)

type target =
  ReferenceInterpreter.target

let target2state (s, _spurious) =
  s

type maybe_targeted_sentence =
  located_sentence * target option

type targeted_sentence =
  located_sentence * target

(* A targeted run is a series of targeted sentences or comments together with
   an error message. *)

type maybe_targeted_run =
  maybe_targeted_sentence or_comment list *
  delimiter *
  message

type targeted_run =
  targeted_sentence or_comment list *
  delimiter *
  message

(* A filtered targeted run is a series of targeted sentences together with an
   error message. (The comments have been filtered out.) *)

type filtered_targeted_run =
  targeted_sentence list *
  message

(* --------------------------------------------------------------------------- *)

(* Display and debugging. *)

let print_sentence (nto, terminals) : string =
  let b = Buffer.create 128 in
  Option.iter (fun nt ->
    Printf.bprintf b "%s: " (Nonterminal.print false nt)
  ) nto;
  let separator = Misc.once "" " " in
  List.iter (fun t ->
    Printf.bprintf b "%s%s" (separator()) (Terminal.print t)
  ) terminals;
  Printf.bprintf b "\n";
  Buffer.contents b

(* --------------------------------------------------------------------------- *)

(* [stream] turns a finite list of terminals into a stream of terminals. *)

exception EndOfStream

let stream (toks : Terminal.t list) : unit -> Terminal.t * Lexing.position * Lexing.position =
  let toks = ref toks in
  fun () ->

    let tok =
      match !toks with
      | tok :: more ->

          (* Take a token off the list, and return it. *)

          toks := more;
          tok

      | [] ->

          (* The finite list has been exhausted. Here, two plausible behaviors
             come to mind.

             The first behavior consists in raising an exception. In that case,
             we are creating a finite stream, and it is up to the parser to not
             read past its end.

             The second behavior consists in returning a designated token. In
             that case, we are creating an infinite, eventually constant,
             stream.

             The choice between these two behaviors is somewhat arbitrary;
             furthermore, in the second case, the choice of the designated
             token is arbitrary as well. Here, we adopt the second behavior if
             and only if the grammar has an EOF token, and we use EOF as the
             designated token. Again, this is arbitrary, and could be changed
             in the future. *)

          match Terminal.eof with
          | Some eof ->
              eof
          | None ->
              raise EndOfStream

    in

    (* For now, return dummy positions. *)

    tok, Lexing.dummy_pos, Lexing.dummy_pos

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

  begin try
    match
      MenhirLib.Convert.Simplified.traditional2revised
        (ReferenceInterpreter.interpret Settings.trace nt)
        (stream toks)
    with

    | Some cst ->

        (* Success. *)

        Printf.printf "ACCEPT";
        if Settings.interpret_show_cst then begin
          print_newline();
          Cst.show stdout cst
        end

    | None ->

        (* Parser failure. *)

        Printf.printf "REJECT"

  with EndOfStream ->

    (* Lexer failure. *)

    Printf.printf "OVERSHOOT"

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

(* [print_messages_auto] displays just the sentence and the auto-generated
   comments. [otarget] may be [None], in which case the auto-generated comment
   is just a warning that this sentence does not end in an error. *)

let print_messages_auto (nt, sentence, otarget) : unit =
  (* Print the sentence, followed with auto-generated comments. *)
  print_string (print_sentence (Some nt, sentence));
  match (otarget : target option) with
  | None ->
      Printf.printf
        "##\n\
         ## WARNING: This sentence does NOT end with a syntax error, as it should.\n\
         ##\n"
  | Some (s', spurious) ->
      Printf.printf
        "##\n\
         ## Ends in an error in state: %d.\n\
         ##\n\
         %s##\n"
        (Lr1.number s')
        (* [Lr0.print] or [Lr0.print_closure] could be used here. The latter
           could sometimes be helpful, but is usually intolerably verbose. *)
        (Lr0.print "## " (Lr1.state s'))
      ;
      Printf.printf
        "## The known suffix of the stack is as follows:\n\
         ##%s\n\
         ##\n"
        (Invariant.print (Invariant.stack s'))
      ;
      if spurious <> [] then begin
        Printf.printf
          "## WARNING: This example involves spurious reductions.\n\
           ## This implies that, although the LR(1) items shown above provide an\n\
           ## accurate view of the past (what has been recognized so far), they\n\
           ## may provide an INCOMPLETE view of the future (what was expected next).\n"
        ;
        List.iter (fun (s, prod) ->
          Printf.printf
            "## In state %d, spurious reduction of production %s\n"
            (Lr1.number s)
            (Production.print prod)
        ) spurious;
        Printf.printf "##\n"
      end

(* [print_messages_item] displays one data item. The item is of the form [nt,
   sentence, target], which means that beginning at the start symbol [nt], the
   sentence [sentence] ends in an error in the target state given by [target].
   [target] also contains information about which spurious reductions are
   performed at the end. The display obeys the [.messages] file format. *)

let print_messages_item (nt, sentence, target) : unit =
  (* Print the sentence, followed with auto-generated comments. *)
  print_messages_auto (nt, sentence, Some target);
  (* Then, print a proposed error message, between two blank lines. *)
  Printf.printf "\n%s\n" default_message

(* --------------------------------------------------------------------------- *)

(* [write_run run] writes a run into a new [.messages] file. Manually-written
   comments are preserved. New auto-generated comments are produced. *)

let write_run : maybe_targeted_run or_comment -> unit =
  function
  | Thing (sentences_or_comments, delimiter, message) ->
      (* First, print every sentence and human comment. *)
      List.iter (fun sentence_or_comment ->
        match sentence_or_comment with
        | Thing ((poss, ((_, toks) as sentence)), target) ->
            let nt = start poss sentence in
            (* Every sentence is followed with newly generated auto-comments. *)
            print_messages_auto (nt, toks, target)
        | Comment c ->
            print_string c
      ) sentences_or_comments;
      (* Then, print the delimiter, which must begin with a blank line
         and may include comments. *)
      print_string delimiter;
      (* Then, print the error message. *)
      print_string message
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
  interpret_error_aux Settings.trace [] sentence fail succeed

(* --------------------------------------------------------------------------- *)

(* [target_sentence] interprets a (located) sentence, expecting it to end in
   an error, computes the state in which the error is obtained, and constructs
   a targeted sentence. *)

let target_sentence
    (signal : Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a)
  : located_sentence -> maybe_targeted_sentence =
  fun (poss, sentence) ->
    (poss, sentence),
    interpret_error_aux false poss sentence
      (* failure: *)
      (fun msg ->
        signal poss
          "this sentence does not end with a syntax error, as it should.\n%s"
          msg
        ;
        None
      )
      (* success: *)
      (fun _nt _terminals target -> Some target)

let target_run_1 signal : run -> maybe_targeted_run =
  fun (sentences, delimiter, message) ->
    List.map (or_comment_map (target_sentence signal)) sentences,
    delimiter,
    message

let target_run_2 : maybe_targeted_run -> targeted_run =
  fun (sentences, delimiter, message) ->
    let aux (x, y) = (x, Misc.unSome y) in
    List.map (or_comment_map aux) sentences,
    delimiter,
    message

let target_runs : run list -> targeted_run list =
  fun runs ->
    let c = Error.new_category() in
    let signal = Error.signal c in
    (* Interpret all sentences, possibly displaying multiple errors. *)
    let runs = List.map (target_run_1 signal) runs in
    (* Abort if an error occurred. *)
    Error.exit_if c;
    (* Remove the options introduced by the first phase above. *)
    let runs = List.map target_run_2 runs in
    runs

(* --------------------------------------------------------------------------- *)

(* [filter_things] filters out the comments in a list of things or comments. *)

let filter_things : 'a or_comment list -> 'a list =
  fun things -> List.flatten (List.map unThing things)

(* [filter_run] filters out the comments within a run. *)

let filter_run : targeted_run -> filtered_targeted_run =
  fun (sentences, _, message) ->
    filter_things sentences, message

(* --------------------------------------------------------------------------- *)

(* [setup()] returns a function [read] which reads one sentence from the
   standard input channel. *)

let setup () : unit -> sentence option =

  let open Lexing in
  let lexbuf = from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

  let read () =
    try
      SentenceParser.optional_sentence SentenceLexer.lex lexbuf
    with Parsing.Parse_error ->
      Error.error (Positions.lexbuf lexbuf) "ill-formed input sentence."
  in

  read

(* --------------------------------------------------------------------------- *)

(* Display an informational message about the contents of a [.messages] file.  *)

let stats (runs : run or_comment list) =
  (* [s] counts the sample input sentences. [m] counts the error messages. *)
  let s = ref 0
  and m = ref 0 in
  List.iter (function
  | Thing (sentences, _, _) ->
      incr m;
      List.iter (function
      | Thing _ ->
          incr s
      | Comment _ ->
          ()
      ) sentences
  | Comment _ ->
      ()
  ) runs;
  Printf.eprintf
    "Read %d sample input sentences and %d error messages.\n%!"
    !s !m;
  runs

(* --------------------------------------------------------------------------- *)

(* Reading a [.messages] file. *)

(* Our life is slightly complicated by the fact that the whitespace between
   two runs can contain comments, which we wish to preserve when performing
   [--update-errors]. *)

let read_messages filename : run or_comment list =
  let open Segment in
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
         loop (Comment comments :: accu) segments
    | (Segment, _, lexbuf) :: segments ->
        (* Read a series of located sentences. *)
        match SentenceParser.entry SentenceLexer.lex lexbuf with
        | exception Parsing.Parse_error ->
            Error.error
              [Positions.cpos lexbuf]
              "ill-formed sentence."
        | sentences ->
            (* In principle, we should now find a segment of whitespace
               followed with a segment of text. By construction, the two
               kinds of segments alternate. *)
            match segments with
            | (Whitespace, comments, _) ::
              (Segment, message, _) ::
              segments ->
                let run : run = sentences, comments, message in
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
  stats (loop [] segments)

(* --------------------------------------------------------------------------- *)

(* [message_table] converts a list of targeted runs to a table (a mapping) of
   states to located sentences and messages. Optionally, it can detect that
   two sentences lead to the same state, and report an error. *)

let message_table (detect_redundancy : bool) (runs : filtered_targeted_run list)
  : (located_sentence * message) Lr1.NodeMap.t =

  let c = Error.new_category() in
  let table =
    List.fold_left (fun table (sentences_and_states, message) ->
      List.fold_left (fun table (sentence2, target) ->
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
      ) table sentences_and_states
    ) Lr1.NodeMap.empty runs
  in
  Error.exit_if c;
  table

(* --------------------------------------------------------------------------- *)

(* [compile_runs] converts a list of targeted runs to OCaml code that encodes
   a mapping of state numbers to error messages. The code is sent to the
   standard output channel. *)

let compile_runs filename (runs : filtered_targeted_run list) : unit =

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
    List.fold_left (fun branches (sentences_and_states, message) ->
      (* Create an or-pattern for these states. *)
      let states = List.map (fun (_, target) ->
        let s = target2state target in
        pint (Lr1.number s)
      ) sentences_and_states in
      (* Map all these states to this message. *)
      { branchpat = POr states;
        branchbody = EStringConst message } :: branches
    ) [ default ] runs
  in
  let messagedef = {
    valpublic = true;
    valpat = PVar name;
    valval = EFun ([ PVar "s" ], EMatch (EVar "s", branches))
  } in
  let program = [
    SIComment (Printf.sprintf
      "This file was auto-generated based on \"%s\"." filename);
    SIComment (Printf.sprintf
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
    Printf.printf "Ready!\n%!";
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

    (* Read the file. *)
    let runs : run or_comment list = read_messages filename in
    (* Drop the comments in between two runs. *)
    let runs : run list = filter_things runs in

    (* Convert every sentence to a state number. We signal an error if a
       sentence does not end in an error, as expected. *)
    let runs : targeted_run list = target_runs runs in

    (* Remove comments within the runs. *)
    let runs : filtered_targeted_run list = List.map filter_run runs in

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

let () =
  Settings.compare_errors |> Option.iter (fun (filename1, filename2) ->

    (* Read and convert both files, as above. *)
    let runs1 = read_messages filename1
    and runs2 = read_messages filename2 in
    let runs1 = filter_things runs1
    and runs2 = filter_things runs2 in
    let runs1 = target_runs runs1
    and runs2 = target_runs runs2 in (* here, it would be OK to ignore errors *)
    let runs1 = List.map filter_run runs1
    and runs2 = List.map filter_run runs2 in
    let table1 = message_table false runs1
    and table2 = message_table false runs2 in

    (* Check that the domain of [table1] is a subset of the domain of [table2]. *)
    let c = Error.new_category() in
    table1 |> Lr1.NodeMap.iter (fun s ((poss1, _), _) ->
      if not (Lr1.NodeMap.mem s table2) then
        Error.signal c poss1
          "this sentence leads to an error in state %d.\n\
           No sentence that leads to this state exists in \"%s\"."
          (Lr1.number s) filename2
    );

    (* Check that [table1] is a subset of [table2], that is, for every state
       [s] in the domain of [table1], [s] is mapped by [table1] and [table2]
       to the same error message. As an exception, if the message found in
       [table1] is the default message, then no comparison takes place. This
       allows using [--list-errors] and [--compare-errors] in conjunction to
       ensure that a [.messages] file is complete, without seeing warnings
       about different messages. *)
    table1 |> Lr1.NodeMap.iter (fun s ((poss1, _), message1) ->
      if message1 <> default_message then
        try
          let (poss2, _), message2 = Lr1.NodeMap.find s table2 in
          if message1 <> message2 then
            Error.warning (poss1 @ poss2)
              "these sentences lead to an error in state %d.\n\
               The corresponding messages in \"%s\" and \"%s\" differ."
              (Lr1.number s) filename1 filename2
        with Not_found ->
          ()
    );

    Error.exit_if c;
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
    let runs : run or_comment list = read_messages filename in

    (* Convert every sentence to a state number. Warn, but do not
       fail, if a sentence does not end in an error, as it should. *)
    let runs : maybe_targeted_run or_comment list =
      List.map (or_comment_map (target_run_1 Error.warning)) runs
    in

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
    let runs : run or_comment list = read_messages filename in

    (* Echo. *)
    List.iter (or_comment_iter (fun run ->
      let (sentences : located_sentence or_comment list), _, _ = run in
      List.iter (or_comment_iter (fun (_, sentence) ->
        print_string (print_sentence sentence)
      )) sentences
    )) runs;

    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* End of the functor [Run]. *)

end

let run () =
  let module R = Run(struct end) in
  ()
