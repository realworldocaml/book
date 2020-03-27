open Printf
open Parser
open Stream
open Generator

(* ---------------------------------------------------------------------------- *)

(* A token printer, for debugging. *)

let print_token = function
  | INT i ->
      printf "%d" i
  | PLUS ->
      printf " + "
  | MINUS ->
      printf " - "
  | TIMES ->
      printf " * "
  | DIV ->
      printf " / "
  | LPAREN ->
      printf "("
  | RPAREN ->
      printf ")"
  | EOL ->
      printf "\n"

let print_token_stream =
  iter print_token

(* ---------------------------------------------------------------------------- *)

(* Parse the command line. *)

(* [--dry-run] offers a choice between running just the generator, or both the
   generator and the parser. *)

let dry_run =
  ref false

(* [--seed] allows the random seed to be set via the command line. *)

let seed =
  ref 61112962

(* [--runs] allows the desired number of runs to be set via the command line. *)

let runs =
  ref 10

(* [--size] allows the size of the randomly-generated expression to be
   set via the command line. *)

let size =
  ref 10000000

let options = Arg.align [
  "--dry-run", Arg.Set dry_run, "Run only the generator, not the parser";
  "--seed", Arg.Set_int seed, sprintf "<seed> Set the random seed (%d)" !seed;
  "--runs", Arg.Set_int runs, sprintf "<runs> Set the number of runs (%d)" !runs;
  "--size", Arg.Set_int size, sprintf "<size> Set the size of the test (%d)" !size;
]

let usage =
  sprintf "Usage: %s <options>" Sys.argv.(0)

let () =
  Arg.parse options (fun _ -> ()) usage

(* ---------------------------------------------------------------------------- *)

(* Run. *)

open Lexing

let tokens : token array =
  Random.init !seed;
  produce !size
  |> to_array

let () =
  Gc.major()

let () =
  if !dry_run then
    exit 0

let lexer lexbuf =
  let pos = lexbuf.lex_curr_pos in
  if pos < Array.length tokens then begin
    let token = tokens.(pos) in
    lexbuf.lex_curr_pos <- pos + 1;
    token
  end
  else
    raise End_of_file

let new_lexbuf () =
  let lexbuf = from_string "" in
  lexbuf.lex_start_p <- dummy_pos;
  lexbuf.lex_curr_p <- dummy_pos;
  lexbuf.lex_curr_pos <- 0;
  lexbuf

let () =
  (* Running the parser several times in succession (without re-generating the
     random data, and without explicitly invoking the GC between runs) should
     allow us to obtain slightly more stable timings. *)
  let chrono = Time.fresh() in
  Time.chrono chrono (fun () ->
    for _run = 0 to !runs do
      printf "%d\n%!"
        (Parser.main lexer (new_lexbuf()))
    done
  );
  Time.display stderr chrono
