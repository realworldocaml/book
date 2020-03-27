open Printf
open MenhirSdk

(* ------------------------------------------------------------------------ *)

(* This compatibility layer compensates for the absence of some [String] in
   OCaml 4.02.3. *)

module Compatibility = struct

  module Char = struct

    let lowercase_ascii c =
      if (c >= 'A' && c <= 'Z')
      then Char.chr (Char.code c + 32)
      else c

    let uppercase_ascii c =
      if (c >= 'a' && c <= 'z')
      then Char.chr (Char.code c - 32)
      else c

  end

  module Bytes = struct

    include Bytes

    let apply1 f s =
      if Bytes.length s = 0 then s else begin
        let r = Bytes.copy s in
        Bytes.unsafe_set r 0 (f (Bytes.unsafe_get s 0));
        r
      end

    let capitalize_ascii s =
      apply1 Char.uppercase_ascii s

    let uncapitalize_ascii s =
      apply1 Char.lowercase_ascii s

  end

  module String = struct

    let capitalize_ascii s =
      Bytes.unsafe_to_string (Bytes.capitalize_ascii (Bytes.unsafe_of_string s))

    let uncapitalize_ascii s =
      Bytes.unsafe_to_string (Bytes.uncapitalize_ascii (Bytes.unsafe_of_string s))

  end

end

(* ------------------------------------------------------------------------ *)

(* We expect one command line argument: the name of a .cmly file. *)

let filename =
  if Array.length Sys.argv = 2
  && Filename.check_suffix Sys.argv.(1) ".cmly" then
    Sys.argv.(1)
  else begin
    eprintf "Usage: %s <parser.cmly>\n" Sys.argv.(0);
    exit 1
  end

(* ------------------------------------------------------------------------ *)

(* Read this file. This gives rise to a module whose signature is
   [Cmly_api.GRAMMAR]. We include it, so we can use it without even
   naming it. *)

include Cmly_read.Read (struct
  let filename = filename
end)

(* ------------------------------------------------------------------------ *)

(* All names which refer to Menhir's inspection API are qualified with this
   module name. We do not [open] this module because that might hide some
    names exploited by the user within attributes. *)

let menhir =
  "MenhirInterpreter"

(* ------------------------------------------------------------------------ *)

(* The header consists of an [open] directive, followed with content taken
   from [@header] attributes. *)

let module_name =
  filename
    |> Filename.basename
    |> Filename.chop_extension
    |> Compatibility.String.capitalize_ascii

let header () =
  printf "open %s\n\n" module_name;
  List.iter (fun attr ->
    if Attribute.has_label "header" attr then
      printf "%s\n" (Attribute.payload attr)
  ) Grammar.attributes

(* ------------------------------------------------------------------------ *)

(* [name default attrs] returns the payload of an [@name] attribute found in
   [attrs], if there is one, and the literal string [default] otherwise. *)

let name default attrs =
  try
    let attr = List.find (Attribute.has_label "name") attrs in
    Attribute.payload attr
  with Not_found ->
    sprintf "%S" default

(* [print_symbol()] generates code for a [print_symbol] function, which
   converts a symbol to a string. The type of a symbol is [xsymbol];
   see the documentation of Menhir's inspection API. *)

let print_symbol () =
  printf "let print_symbol = function\n";
  Terminal.iter (fun t ->
    match Terminal.kind t with
    | `REGULAR | `ERROR ->
        printf "  | %s.X (%s.T %s.T_%s) -> %s\n"
          menhir menhir menhir
          (Terminal.name t)
          (name (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO | `EOF ->
         ()
  );
  Nonterminal.iter (fun n ->
    match Nonterminal.kind n with
    | `REGULAR ->
        printf "  | %s.X (%s.N %s.N_%s) -> %s\n"
          menhir menhir menhir
          (Nonterminal.name n)
          (name (Nonterminal.name n) (Nonterminal.attributes n))
    | `START ->
        ()
  );
  printf "\n"

(* ------------------------------------------------------------------------ *)

(* [printer default attrs] returns the payload of a [@printer] attribute
   found in [attrs], within parentheses, if there is one. Otherwise, it
   returns a function that ignores its argument and always returns the
   literal string [name default attrs]. *)

let printer default attrs =
  try
    let attr = List.find (Attribute.has_label "printer") attrs in
    sprintf "(%s)" (Attribute.payload attr)
  with Not_found ->
    sprintf "(fun _ -> %s)" (name default attrs)

(* [print_value()] generates code for a [print_value] function, which
   converts a pair of a symbol and its semantic value to a string. The
   type of the symbol is ['a symbol], and the type of the value is ['a].
   See the documentation of Menhir's inspection API. *)

let print_value () =
  printf "let print_value (type a) : a %s.symbol -> a -> string = function\n"
    menhir;
  Terminal.iter (fun t ->
    match Terminal.kind t with
    | `REGULAR | `ERROR ->
        printf "  | %s.T %s.T_%s -> %s\n"
          menhir menhir
          (Terminal.name t)
          (printer (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO | `EOF ->
        ()
  );
  Nonterminal.iter (fun n ->
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %s.N %s.N_%s -> %s\n"
        menhir menhir
        (Nonterminal.name n)
        (printer (Nonterminal.name n) (Nonterminal.attributes n))
    | `START ->
        ()
  );
  printf "\n"

(* [print_token()] generates code for a [print_token] function, which
   converts a token to a string. The type of the token is [token].
   This is done by converting the token to a pair of a symbol and a value
   and invoking [print_value]. *)

let print_token () =
  printf "let print_token = function\n";
  Terminal.iter (fun t ->
    match Terminal.kind t with
    | `REGULAR ->
        (* Deal with the case where the token carries no semantic value. *)
        let pattern, value =
          match Terminal.typ t with
          | None -> "", "()"
          | Some _typ -> " v", "v"
        in
        printf "  | %s%s -> print_value (%s.T %s.T_%s) %s\n"
          (Terminal.name t)
          pattern
          menhir menhir
          (Terminal.name t)
          value
    | `ERROR | `PSEUDO | `EOF ->
        ()
  );
  printf "\n"

(* ------------------------------------------------------------------------ *)

(* The main program. *)

let () =
  header();
  print_symbol();
  print_value();
  print_token()
