%{
  open Ast
  open Parsing
  open Lexing
  open Keywords
%}

%token END IMPORT FOR EQ LINK SEP EOF
%token <string> HEADER
%token <string> ID
%token <string>DEF
%token COQ CAML CAMLI

%start main
%type <unit> main

%%

main:
| EOF {()}
| header_def main { $1 }
| coll_def main { $1 }
  ;

header_def:
  COQ HEADER { Hashtbl.add pure_def (toplevel,coq_h) ("",Some $2) }
| CAML HEADER {Hashtbl.add pure_def (toplevel,caml_h) ($2,None)}
| CAMLI HEADER {Hashtbl.add pure_def (toplevel,caml_hi) ($2,None)}
;
coll_def:
| IMPORT FOR ID limport IMPORT
      { List.iter (fun x -> let (a,b,c,d) = x in
		     (if c <> "" then Hashtbl.add pure_def ($3,c) ("",d));
		     Hashtbl.add pure_def ($3 , a) (b,d)) $4 }
;
limport:
| END { [] }
| ID idlist def SEP limport { let (a,b) = $3 in
			      let name = if a = "" then $1 else a in
				(name,b,"",None) :: $5 }
| ID idlist def LINK def SEP limport { let (a,b) = $3 in
				       let name = if a = "" then $1 else a in
				       let (c,d) = $5 in
					 (name,b,c,Some d) :: $7 }
;

def:
  DEF { ("" , $1) }
| ID DEF { ($1,$2) }
;

idlist:
| EQ { [] }
| ID idlist { $1 :: $2 }
;

%%

let parse_it rgl s =
  try main rgl s with
    | Parse_error ->
       prerr_string "error occured while parsing .fml file : ";
        prerr_string ((lexeme s) ^ " at position ");
        prerr_int (lexeme_end s );
        prerr_newline();

    | Exit -> raise Exit
    | s -> prerr_endline "Unknown error\n"; raise s
;;
(* $Id: pure_def.mly,v 1.2 2003/09/19 12:25:21 prevosto Exp $ *)
