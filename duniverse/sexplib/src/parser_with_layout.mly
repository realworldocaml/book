%{
  (* Parser: Grammar Specification for Parsing S-expressions *)
  (* compare to parser.mly *)

  open Lexing

  let parse_failure what =
    let pos = Parsing.symbol_start_pos () in
    let msg =
      Printf.sprintf "Sexplib.Parser: failed to parse line %d char %d: %s"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) what in
    failwith msg

  module With_pos = struct

    open Type_with_layout.Parsed

    let coerce = Src_pos.Absolute.of_lexing

    let start_pos () = coerce (Parsing.symbol_start_pos ())

    let end_pos () =
      let p = Parsing.symbol_end_pos () in
      coerce { p with Lexing.pos_cnum = p.Lexing.pos_cnum - 1 }

    let atom (x, y) =
      let (pos, y) =
        match y with
        | None -> (start_pos (), None)
        | Some (pos, x) -> (coerce pos, Some x)
      in
      Atom (pos, x, y)

    let list ts = List (start_pos (), ts, end_pos ())

    let sexp    x = Sexp    x
    let comment x = Comment x

    let sexp_comment cs t = Sexp_comment (start_pos (), cs, t)

    let plain_comment (x, pos_opt) =
      let pos =
        match pos_opt with
        | None -> start_pos ()
        | Some pos -> coerce pos
      in
      Plain_comment (pos, x)

  end

%}

%token <string * (Lexing.position * string) option> STRING
%token <string * Lexing.position option> COMMENT
%token LPAREN RPAREN EOF HASH_SEMI

%start sexp
%type <Type_with_layout.t_or_comment> sexp

%start sexp_opt
%type <Type_with_layout.t_or_comment option> sexp_opt

%start sexps
%type <Type_with_layout.t_or_comment list> sexps

%start sexps_abs
%type <Type_with_layout.Parsed.t_or_comment list> sexps_abs

%start rev_sexps
%type <Type_with_layout.t_or_comment list> rev_sexps

%%

sexp_but_no_comment_abs
  : STRING { With_pos.atom $1 }
  | LPAREN rev_sexps_abs RPAREN { With_pos.list (List.rev $2) }
  | error { parse_failure "sexp" }

comment_abs
  : COMMENT { With_pos.plain_comment $1 }
  | HASH_SEMI rev_comments_abs sexp_but_no_comment_abs { With_pos.sexp_comment (List.rev $2) $3 }

rev_comments_abs
  : /* nothing */ { [] }
  | rev_comments_abs comment_abs { $2 :: $1 }

sexp_abs
  : sexp_but_no_comment_abs { With_pos.sexp $1 }
  | comment_abs { With_pos.comment $1 }

rev_sexps_abs
  : /* empty */ { [] }
  | rev_sexps_abs sexp_abs { $2 :: $1 }

sexp
  : sexp_abs { Type_with_layout.relativize $1 }

sexp_opt
  : sexp { Some $1 }
  | EOF { None }

rev_sexps_aux
  : sexp { [$1] }
  | rev_sexps_aux sexp { $2 :: $1 }

rev_sexps
  : rev_sexps_aux EOF { $1 }
  | EOF { [] }

sexps
  : rev_sexps_aux EOF { List.rev $1 }
  | EOF { [] }

/* for debugging positions */
sexps_abs
  : rev_sexps_abs EOF { List.rev $1 }

