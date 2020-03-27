/* $Header: /home/pauillac/formel1/fpottier/cvs/photos/queryp.mly,v 1.7 2004/06/12 15:07:03 fpottier Exp $ */

%{

open Search

(* Error handling. *)

let failure index message =
  raise (SyntaxError (Parsing.rhs_start index, message))

let today () =
  let date = fst (Unix.mktime {
    (Unix.localtime (Unix.time ())) with
    Unix.tm_hour = 0;
    Unix.tm_min = 0;
    Unix.tm_sec = 0
  }) in
  date, date +. 86400.0

%}

%token <int> NUMBER
%token LPAREN
%token RPAREN
%token SLASH
%token <string> LITERAL
%token NOT
%token AND
%token OR
%token AFTER
%token BEFORE
%token ON
%token NAME
%token MATCHES
%token HAS
%token EOF
%token TRUE
%token TODAY
%token YESTERDAY

%start query
%type <Search.query> query

%%

/* TEMPORARY on peut ajouter plus de clauses de gestion d'erreur */

query0:
| NAME LITERAL
    { Name (fun s -> Filename.chop_extension s = $2) }
| BEFORE date
    { let _, date = $2 in DateLessThan date }
| ON date
    { let date1, date2 = $2 in And (DateGreaterThan date1, DateLessThan date2) }
| TODAY
    { let date1, date2 = today () in And (DateGreaterThan date1, DateLessThan date2) }
| YESTERDAY
    { let date1, date2 = today () in let date1, date2 = date1 -. 86400.0, date2 -. 86400.0 in
      And (DateGreaterThan date1, DateLessThan date2) }
| AFTER date
    { let date, _ = $2 in DateGreaterThan date }
| MATCHES LITERAL
    { let regexp = Str.regexp $2 in
      let predicate s = try let _ = Str.search_forward regexp s 0 in true with Not_found -> false in
      Or (Key ("caption", predicate), Key ("description", predicate)) }
| HAS LITERAL
    { Property $2 }
| NOT query0
    { Not $2 }
| LPAREN query RPAREN
    { $2 }
| TRUE
    { True }
;

query1:
| query1 AND query0
    { And ($1, $3) }
| query0
    { $1 }
;

query2:
| query2 OR query1
    { Or ($1, $3) }
| query1
    { $1 }
;

query:
| query2
    { $1 }
;

date:
| NUMBER SLASH NUMBER SLASH NUMBER
    { try
        let date = fst (Unix.mktime {
			Unix.tm_year = $1 - 1900;
			Unix.tm_mon = $3 - 1;
			Unix.tm_mday = $5;
			Unix.tm_hour = 0;
			Unix.tm_min = 0;
			Unix.tm_sec = 0;
			Unix.tm_wday = 0;
			Unix.tm_yday = 0;
			Unix.tm_isdst = false
		      }) in
	date, date +. 86400.0
      with Unix.Unix_error _ ->
	failure 1 "date invalide"
    }
| NUMBER SLASH NUMBER SLASH error
    { failure 5 "jour du mois (deux chiffres) attendu" }
| NUMBER SLASH NUMBER error
    { failure 4 "'/' attendu" }
| NUMBER SLASH error
    { failure 3 "mois de l'année (deux chiffres) attendu" }
| NUMBER error
    { failure 2 "'/' attendu" }
;

