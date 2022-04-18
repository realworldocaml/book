open Printf

(* -------------------------------------------------------------------------- *)

(* Parse the command line. *)

(* [--runs] allows the desired number of runs to be set via the command line. *)

let runs =
  ref 10

(* Our input source. The default is [stdin]. A different file name can
   be supplied on the command line. *)

let input =
  ref stdin

let options =
  Arg.align [
    "--runs",
    Arg.Set_int runs,
    sprintf "<runs> Set the desired number of runs (default %d)" !runs;
  ]

let usage =
  sprintf "Usage: %s <options>" Sys.argv.(0)

let () =
  Arg.parse options (fun filename -> input := open_in filename) usage

let input =
  !input

(* ---------------------------------------------------------------------------- *)

(* Read the tokens from the input file. This phase is not timed. *)

let tokens =
  let lexbuf = Lexing.from_channel input in
  let stack = ref [] in
  try
    while true do
      stack := Lexer.token lexbuf :: !stack
    done;
    assert false
  with Lexer.ExnEOF ->
    List.rev !stack
    |> Array.of_list

let () =
  Gc.full_major()

(* ---------------------------------------------------------------------------- *)

(* Define a home-made lexer that reads from the token array in memory. *)

open Lexing

let lexer lexbuf =
  let pos = lexbuf.lex_curr_pos in
  (* As long as we parse well-formed sequences of tokens, we cannot hit the
     end of the array. *)
  assert (pos < Array.length tokens);
  let token = Array.unsafe_get tokens pos in
  lexbuf.lex_curr_pos <- pos + 1;
  token

let new_lexbuf () =
  let lexbuf = from_string "" in
  lexbuf.lex_start_p <- dummy_pos;
  lexbuf.lex_curr_p <- dummy_pos;
  lexbuf.lex_curr_pos <- 0;
  lexbuf

(* ---------------------------------------------------------------------------- *)

(* Run. *)

(* Running the parser several times in succession (without explicitly invoking
   the GC between runs) should allow us to obtain somewhat stable timings. *)

let () =
  printf "About to perform %d runs...\n" !runs;
  let gc1 = Gc.quick_stat () in
  let times1 = Unix.times () in

  for _i = 1 to !runs - 1 do
    ignore (Parser.main lexer (new_lexbuf ()))
  done;
  (* In the last run, we keep the semantic value, because we want to
      measure its size. *)
  let v = Parser.main lexer (new_lexbuf ()) in

  let times2 = Unix.times () in
  (* Gather statistics. *)
  let elapsed = times2.tms_utime -. times1.tms_utime in
  let gc2 = Gc.quick_stat () in
  let size = Obj.reachable_words (Obj.repr v) in
  let minor = gc2.minor_words -. gc1.minor_words
  and major = gc2.major_words -. gc1.major_words
  and promoted = gc2.promoted_words -. gc1.promoted_words in
  let runs = float_of_int !runs in
  eprintf "tokens: %d\n" (Array.length tokens);
  eprintf "time: %f\n" (elapsed /. runs);
  eprintf "minor: %f\n" (minor /. runs);
  eprintf "major: %f\n" (major /. runs);
  eprintf "promoted: %f\n" (promoted /. runs);
  eprintf "size: %d\n" size
