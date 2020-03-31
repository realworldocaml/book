(* Let's do floating-point evaluation, for a change. *)

module FloatSemantics = struct

  type number =
      float

  let inject =
    float_of_int

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let (~- ) = (~-. )

end

(* Let us now specialize our parameterized parser. *)

module FloatParser =
  Parser.Make(FloatSemantics)

(* The rest is as usual. *)

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    Printf.printf "%f\n%!" (FloatParser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | FloatParser.Error ->
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

