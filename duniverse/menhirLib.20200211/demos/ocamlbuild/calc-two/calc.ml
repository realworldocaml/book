let algebraic =
  ref true

let () =
  Arg.parse [
    "--algebraic", Arg.Set algebraic, " Use algebraic (that is, infix) notation";
    "--reverse", Arg.Clear algebraic, " Use reverse Polish (that is, postfix) notation";
  ] (fun _ -> ()) (Printf.sprintf "Usage: %s <options>" Sys.argv.(0))

let main =
  if !algebraic then
    Algebraic.main
  else
    Reverse.main

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    Printf.printf "%d\n%!" (main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Algebraic.Error
  | Reverse.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (Lexing.from_channel stdin)

