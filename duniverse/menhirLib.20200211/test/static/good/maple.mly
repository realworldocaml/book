%{
(* François Thomasset -- INRIA Rocquencourt -- Octobre 2001 *)

(* Translation from Maple to MuPad : syntaxic specification of maple *)

(*
Copyright © 2001-2002 François Thomasset, all rights reserved.
Copying is covered by the GNU General Public License (GPL).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

$Revision: 1.1.1.1 $
$Date: 2002/03/08 08:31:57 $

*)

open Maple;;
let is_empty_comments = function
  | [] -> true
  | _ -> false;;
let is_empty_stat = function
  | EmptyStat -> true
  | _ -> false ;;
let convert_exprs_to_names exprs =
  let cve = function
    | NameExpr (n) -> n
    | e ->
	begin
	  Printf.printf "ERROR: a parameter of an arrow expression is a %s\n" (string_of_expr e);
	  Printf.printf "NameExpr was expected\n";
	  Printf.printf "line number : %d\n" !Maple_lexer.linecount;
	  flush stdout;
	  failwith "convert_exprs_to_names"
	end
  in
  let rec cv = function
    | [] -> []
    | n :: reste -> (cve n) :: cv reste
  in cv exprs ;;
%}
%token <string> ID
%token <int> INT
%token <string> STRING
%token <string> QUOTED_STRING
%token SEMICOLON COLON ASSIGN DOUBLE_COLON
%token PLUS SUBTRACT MULT
%token AND NOT OR
%token SLASH EXP CARET EXCLAM
%token EQUAL LT GT LE GE NE
%token AT ARROW REPEAT_COMPOSE SEQ ELLIPSE
%token DOT COMMA LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token QUOTE BQUOTE DQUOTE
%token BAR AMPMUL UNDERSCORE PERCENT BACKSLASH QUESTIONMARK
%token BY DO DONE ELIF ELSE END FI FOR FROM IF IN LOCAL GLOBAL OD OPTION
%token PROC QUIT READ SAVE STOP THEN TO WHILE UNION INTERSECT MINUS MOD
%token <string> AMPOP
%token <string> COMMENT
%token EOF

%nonassoc ASSIGN
%left COMMA
%right ARROW
%left OR
%left AND
%right NOT
%nonassoc SEQ
%nonassoc LT LE GT GE EQUAL NE
%nonassoc ELLIPSE
%nonassoc MOD
%nonassoc UMINUS UPLUS
%left PLUS SUBTRACT MINUS UNION
%left MULT SLASH AT INTERSECT AMPMUL
%nonassoc CARET EXP REPEAT_COMPOSE
%left EXCLAM
%left AMPOP
%nonassoc PERCENT
%left DOT

%start program
%type <Maple.prog> program

%%
program : statseq EOF { List.rev $1 } ;
comments : comments COMMENT { $2 :: $1 }
        | { [] } ;
statseq : statseq SEMICOLON comments stat { { comments = List.rev $3;
					      sep=SemiColon;
					      stat = $4
					    }
					    :: $1
					  }
        | statseq COLON comments stat { { comments = List.rev $3;
					  sep=Colon;
					  stat = $4
					}
					:: $1
				      }
	| comments stat
	    {
	     if (is_empty_stat $2) & (is_empty_comments $1) then []
	     else
	       [ { comments = List.rev $1;
		   sep=NoSep;
		   stat = $2
		 }
	       ]
	    } ;
stat    : nameseq ASSIGN exprseq { AssignStat ( { lhs_of_assign=$1; rhs=$3 } ) }
	| expr { ExprStat ($1) }
	| READ expr { match $2 with
                      | ParenExpr ([e]) -> ReadStat (e)
		      |	_ -> ReadStat ($2) ;
		    }
	| SAVE name_string { SaveStat ( [] , $2 ) }
	| SAVE nameseq COMMA name_string { SaveStat ( $2 , $4 ) }
	| ifpart FI { $1 }
	| ifpart ELSE statseq FI
	      { match $1 with
	        | IfThenElseStat current_if_stat
		  ->
	            let condition = current_if_stat.cond and
			th = current_if_stat.then_part and
			alts = current_if_stat.alternatives
		    in IfThenElseStat ( { cond = condition;
					  then_part = th;
					  alternatives = alts;
					  else_part = List.rev $3
					} )
		| _ ->
		    begin
		      Printf.printf "ERROR: ifpart is a %s\n" (string_of_stat $1);
		      Printf.printf "IfThenElseStat was expected\n";
		      Printf.printf "current rule: stat: ifpart ELSE statseq FI\n";
		      Printf.printf "line number : %d\n" !Maple_lexer.linecount;
		      flush stdout;
		      failwith "ifpart"
		    end
	      }
	| for_stmt { $1 }
	| for_in_stmt { $1 }
	| QUIT { StopStat }
	| STOP { StopStat }
	| { EmptyStat } ;
for_stmt :
	| FOR name for_without_name
	    { let fwn = $3 in
              ForStat ( { loop_index = Some $2;
			  from_expr = fwn.from_expr;
			  by_expr = fwn.by_expr;
			  to_expr = fwn.to_expr;
			  while_expr = fwn.while_expr;
			  body = fwn.body
			} )
	    }
	| for_without_name { ForStat ($1) }
for_without_name :
	| FROM expr for_without_from
	    { let fwf = $3 in
	      { loop_index = None;
		from_expr = Some $2;
		by_expr = fwf.by_expr;
		to_expr = fwf.to_expr;
		while_expr = fwf.while_expr;
		body = fwf.body
	      }
	    }
	| for_without_from { $1 }
for_without_from :
	| BY expr TO expr for_body
	    { let fwb = $5 in
	      { loop_index = None;
		from_expr = None;
		by_expr = Some $2 ;
		to_expr = Some $4 ;
		while_expr = fwb.while_expr;
		body = fwb.body
	      }
	    }
	| TO expr BY expr for_body
	    { let fwb = $5 in
	      { loop_index = None;
		from_expr = None;
		by_expr = Some $4 ;
		to_expr = Some $2 ;
		while_expr = fwb.while_expr;
		body = fwb.body
	      }
	    }
	| TO expr for_body
	    { let fwb = $3 in
	      { loop_index = None;
		from_expr = None;
		by_expr = None;
		to_expr = Some $2 ;
		while_expr = fwb.while_expr;
		body = fwb.body
	      }
	    }
	| BY expr for_body
	    { let fwb = $3 in
	      { loop_index = None;
		from_expr = None;
		by_expr = Some $2 ;
		to_expr = None ;
		while_expr = fwb.while_expr;
		body = fwb.body
	      }
	    }
	| for_body { $1 }
for_body :
	| WHILE expr DO statseq OD
	    { { loop_index = None;
		from_expr = None;
		by_expr = None;
		to_expr = None;
		while_expr = Some $2;
		body = List.rev $4
	      }
	    }
	| DO statseq OD { { loop_index = None;
			    from_expr = None;
			    by_expr = None;
			    to_expr = None;
			    while_expr = None;
			    body = List.rev $2
			  }
			}
for_in_stmt :
          FOR name IN expr DO statseq OD
	    {
	      ForInStat ( { loop_var = $2;
			    in_expr = $4;
			    in_while = None;
			    in_body = List.rev $6
			  }
			 )
	    }
	| FOR name IN expr WHILE expr DO statseq OD
	    {
	      ForInStat ( { loop_var = $2;
			    in_expr = $4;
			    in_while = Some $6;
			    in_body = List.rev $8
			  }
			 )
	    }
ifpart  : IF expr THEN statseq { IfThenElseStat ( { cond=$2;
						    then_part = List.rev $4;
						    alternatives = [];
						    else_part = []
						  } )
			       }
	| ifpart ELIF expr THEN statseq
	      { match $1 with
	        | IfThenElseStat current_if_stat
		  ->
	            let condition = current_if_stat.cond and
			th = current_if_stat.then_part and
			alts = current_if_stat.alternatives
		    in IfThenElseStat ( { cond = condition;
					  then_part = th;
					  alternatives = alts @ [ { cond_of_alt = $3;
								    stats_of_alt = List.rev $5 } ];
					  else_part = []
					} )
		| _ ->
		    begin
		      Printf.printf "ERROR: ifpart is a %s\n" (string_of_stat $1);
		      Printf.printf "IfThenElseStat was expected\n";
		      Printf.printf "current rule: ifpart: IF expr THEN statseq\n";
		      Printf.printf "line number : %d\n" !Maple_lexer.linecount;
		      flush stdout;
		      failwith "ifpart"
		    end
	      } ;

expr    : expr ARROW expr
            { match $1 with
	    | NameExpr (n) -> ArrowExpr ( [n] , $3 )
	    | ParenExpr (exps) -> ArrowExpr ( (convert_exprs_to_names exps) , $3 )
	    | _ ->
		begin
		  Printf.printf "ERROR: expected NameExpr or ParenExpr\n";
		  Printf.printf "current rule: expr: expr ARROW expr\n";
		  Printf.printf "first expr is a %s\n" (string_of_expr $1);
		  Printf.printf "line number : %d\n" !Maple_lexer.linecount;
		  flush stdout;
		  failwith "arrow expr"
		end
	    }
	| expr OR expr { BinExpr ( { left = $1; bin_op = OrOp; right = $3 } ) }
	| expr AND expr { BinExpr ( { left = $1; bin_op = AndOp; right = $3 } ) }
	| NOT expr { UnaryExpr ( NotOp, $2) }
	| expr SUBTRACT expr { BinExpr ( { left = $1; bin_op = MinusOp; right = $3 } ) }
	| expr PLUS expr { BinExpr ( { left = $1; bin_op = PlusOp; right = $3 } ) }
	| PLUS expr %prec UPLUS { UnaryExpr ( UnaryPlus , $2 ) }
	| SUBTRACT expr %prec UMINUS { UnaryExpr ( UnaryMinus , $2 ) }
	| expr MULT expr { BinExpr ( { left = $1; bin_op = MultOp; right = $3 } ) }
	| expr AMPOP expr { BinExpr ( { left = $1; bin_op = NeutralOp($2); right = $3 } ) }
	| expr AMPMUL expr { BinExpr ( { left = $1; bin_op = NeutralMulOp; right = $3 } ) }
	| expr SLASH expr { BinExpr ( { left = $1; bin_op = DivOp; right = $3 } ) }
	| expr CARET expr { BinExpr ( { left = $1; bin_op = ExpoOp; right = $3 } ) }
	| expr EXP expr { BinExpr ( { left = $1; bin_op = ExpoOp; right = $3 } ) }
	| expr SEQ expr { SeqExpr ( Some $1 , $3 ) }
	| SEQ expr { SeqExpr ( None , $2 ) }
	| expr LT expr { BinExpr ( { left = $1; bin_op = LTOp; right = $3 } ) }
	| expr GT expr { BinExpr ( { left = $1; bin_op = GTOp; right = $3 } ) }
	| expr LE expr { BinExpr ( { left = $1; bin_op = LEOp; right = $3 } ) }
	| expr GE expr { BinExpr ( { left = $1; bin_op = GEOp; right = $3 } ) }
	| expr NE expr { BinExpr ( { left = $1; bin_op = NEOp; right = $3 } ) }
	| expr EQUAL expr { BinExpr ( { left = $1; bin_op = EQOp; right = $3 } ) }
	| expr MOD expr { BinExpr ( { left = $1; bin_op = ModOp; right = $3 } ) }
	| expr ELLIPSE expr { IntervalExpr ( $1 , $3 ) }
	| expr UNION expr { BinExpr ( { left = $1; bin_op = Union; right = $3 } ) }
	| expr MINUS expr { BinExpr ( { left = $1; bin_op = Minus; right = $3 } ) }
	| expr INTERSECT expr { BinExpr ( { left = $1; bin_op = Intersect; right = $3 } ) }
	| expr EXCLAM { Factorial ( $1 ) }
	| QUOTE expr QUOTE { UnevaluatedExpr ( $2 ) }
	| LBRACK exprseq RBRACK { ListExpr ( $2 ) }
	| LBRACE exprseq RBRACE { SetExpr ( $2 ) }
	| name { NameExpr ( $1 ) }
	| STRING { StringExpr ($1) }
	| name functional_operator
	    {
	     match $2 with
	     | [a] -> CallExpr ( { callee = $1; args = a } )
	     | _ -> FunctionalCallExpr ( { callee_f = $1; fargs = $2 } )
	   }
	| INT { IntExpr ($1) }
	| INT DOT INT { FloatExpr ( $1, $3 ) }
	| INT DOT { FloatExpr ( $1 , 0 ) }
	| DOT INT { FloatExpr ( 0 , $2 ) }
	| PROC LPAREN parmseq RPAREN result_type decls_proc options_of_proc statseq END
	    {
	     ProcDef ( { params = $3;
			 result_type = $5;
			 locals = $6;
			 options = $7;
			 body_of_proc = List.rev $8;
		       }
		     )
	    }
	| LPAREN exprseq RPAREN { ParenExpr ($2) }
	| expr AT expr { ComposeExpr ($1,$3) }
	| expr REPEAT_COMPOSE expr { RepeatComposeExpr ($1,$3) };
exprseq : { [] }
        | exprseq COMMA expr { $1 @ [$3] }
	| expr { [ $1 ] } ;
name    : name_string { NameString ($1) }
	| name DOT INT { NameConcat ( $1 , N_int ($3) ) }
	| name DOT STRING { NameConcat ( $1 , N_str ($3) ) }
	| name DOT LPAREN expr RPAREN { NameConcat ( $1 , N_expr ($4) ) }
	| name LBRACK exprseq RBRACK { NameIndexed ( $1 , $3 ) } ;
functional_operator :
	| LPAREN exprseq RPAREN { [ $2 ] }
	| functional_operator LPAREN exprseq RPAREN { $1 @ [$3] };
parmseq : { [] }
	| oneparm { [$1] }
	| parmseq COMMA oneparm { $1 @ [$3] };
result_type :
	| { None }
	| DOUBLE_COLON name_string SEMICOLON { Some $2 };
oneparm : name { { param_name =$1 ; param_type = None } }
	| name DOUBLE_COLON name_string { { param_name =$1 ; param_type = Some $3 } }
nameseq : { [] }
	| name { [$1] }
	| nameseq COMMA name { $1 @ [$3] }
decls_proc :
	| { [] }
	| globals_of_proc { [] }
	| locals_of_proc { $1 }
	| locals_of_proc globals_of_proc { $1 }
	| globals_of_proc locals_of_proc { $2 }
locals_of_proc :
	| LOCAL nameseq SEMICOLON { $2 }
globals_of_proc :
	| GLOBAL nameseq SEMICOLON { () }
options_of_proc :
	| { [] }
	| OPTION nameseq SEMICOLON { $2 }
name_string :
          ID { Ident ( $1  ) }
	| QUOTED_STRING { QuotedString ( $1 ) } ;
%%
