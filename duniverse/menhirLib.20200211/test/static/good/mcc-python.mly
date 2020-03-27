/* Parser for Python
 * David Bustos
 */
%{
open Python_ast
open Python_ast_util

let no_pos = ("", 0, 0, 0, 0)
let sys_maxint = Symbol.add "sys.maxint"
let ellipsis = Symbol.add "Ellipsis"
let zero = LiteralExpr ((IntLiteral (0, no_pos)), no_pos)
let none = Symbol.add "None"
let none_expr = IdExpr (none, no_pos)

exception Impossible
exception InconsistentDedent

(********** Position stuff **********)
(*
 * Combine two positions.
 *)
let union_pos
    (file1, sline1, schar1, eline1, echar1)
    (file2, sline2, schar2, eline2, echar2) =
   if file1 <> file2 then
      raise (Invalid_argument (**)
                (Printf.sprintf "union_pos: file mistmatch: \"%s\":\"%s\"" (**)
                    (String.escaped file1) (String.escaped file2)));
   let sline, schar =
      if sline1 < sline2 then
         sline1, schar1
      else if sline1 > sline2 then
         sline2, schar2
      else
         sline1, min schar1 schar2
   in
   let eline, echar =
      if eline1 > eline2 then
         eline1, echar1
      else if eline1 < eline2 then
         eline2, echar2
      else
         eline1, max echar1 echar2
   in
      file1, sline, schar, eline, echar

let fatal_error pos str =
   Python_position.print_pos pos;
   print_endline str;
   exit 1

let parse_error str =
   fatal_error
       !Python_position.position
       (Printf.sprintf "ParseError: `%s'" str)

%}

/* Line structure */
%token <Python_ast.pos> Newline
%token <Python_ast.pos> Indent
%token <Python_ast.pos> Dedent
/* Identifiers and keywords */
%token <Symbol.symbol * Python_ast.pos> Identifier
%token <Python_ast.pos> And
%token <Python_ast.pos> Assert
%token <Python_ast.pos> Break
%token <Python_ast.pos> Class
%token <Python_ast.pos> Continue
%token <Python_ast.pos> Def
%token <Python_ast.pos> Del
%token <Python_ast.pos> Elif
%token <Python_ast.pos> Else
%token <Python_ast.pos> Except
%token <Python_ast.pos> Exec
%token <Python_ast.pos> Finally
%token <Python_ast.pos> For
%token <Python_ast.pos> From
%token <Python_ast.pos> Global
%token <Python_ast.pos> If
%token <Python_ast.pos> Import
%token <Python_ast.pos> In
%token <Python_ast.pos> Is
%token <Python_ast.pos> Lambda
%token <Python_ast.pos> Not
%token <Python_ast.pos> Or
%token <Python_ast.pos> Pass
%token <Python_ast.pos> Print
%token <Python_ast.pos> Raise
%token <Python_ast.pos> Return
%token <Python_ast.pos> Try
%token <Python_ast.pos> While
/* Literals */
%token <string * Python_ast.pos>  String
%token <int    * Python_ast.pos>  Integer
%token <string * Python_ast.pos>  Longinteger
%token <float  * Python_ast.pos>  Float
%token <float  * Python_ast.pos>  Imaginary
/* Operators */
%token <Python_ast.pos> Plus
%token <Python_ast.pos> Dash
%token <Python_ast.pos> Star
%token <Python_ast.pos> StarStar
%token <Python_ast.pos> Slash
%token <Python_ast.pos> Percent
%token <Python_ast.pos> Leftshift
%token <Python_ast.pos> Rightshift
%token <Python_ast.pos> Ampersand
%token <Python_ast.pos> Bar
%token <Python_ast.pos> Caret
%token <Python_ast.pos> Tilde
%token <Python_ast.pos> Lessthan
%token <Python_ast.pos> Greaterthan
%token <Python_ast.pos> LessthanEq
%token <Python_ast.pos> GreaterthanEq
%token <Python_ast.pos> EqualEqual
%token <Python_ast.pos> NotEqual
/* Delimiters */
%token <Python_ast.pos> Leftparen
%token <Python_ast.pos> Leftbracket
%token <Python_ast.pos> Leftbrace
%token <Python_ast.pos> Rightparen
%token <Python_ast.pos> Rightbracket
%token <Python_ast.pos> Rightbrace
%token <Python_ast.pos> Comma
%token <Python_ast.pos> Colon
%token <Python_ast.pos> Period
%token <Python_ast.pos> Backquote
%token <Python_ast.pos> Equal
%token <Python_ast.pos> Semicolon
%token <Python_ast.pos> PlusEqual
%token <Python_ast.pos> DashEqual
%token <Python_ast.pos> StarEqual
%token <Python_ast.pos> SlashEqual
%token <Python_ast.pos> PercentEqual
%token <Python_ast.pos> StarStarEqual
%token <Python_ast.pos> AmpersandEqual
%token <Python_ast.pos> BarEqual
%token <Python_ast.pos> CaretEqual
%token <Python_ast.pos> LeftshiftEqual
%token <Python_ast.pos> RightshiftEqual
/* Other */
%token <Python_ast.pos> Ellipsis
%token <Python_ast.pos> EOF


/* Precedences */
%nonassoc Backquote				/* XXX Where should this be? */
%nonassoc Not Plus Dash Leftparen Leftbracket


%start file_input
%type <Python_ast.stmt list> file_input

/*
%start interactive_input
%type <Python_ast.stmt> interactive_input

%start eval_input input_input
%type <Python_ast.expr list> eval_input input_input
*/

%%


/********** 5.2 Atoms **********/
atom:
    Identifier	{ IdExpr (fst $1, snd $1)      }
  | literal	{ LiteralExpr (fst $1, snd $1) }
  | enclosure	{ $1			       }
;

enclosure:
   parenth_form		{ $1 }
 | list_display		{ $1 }
 | dict_display		{ $1 }
 | string_conversion	{ $1 }
;


/********** 5.2.1 Identifiers **********/


/********** 5.2.2 Literals **********/
literal:
    String	{ StrLiteral     (fst $1, snd $1), snd $1 }
  | Integer	{ IntLiteral     (fst $1, snd $1), snd $1 }
  | Longinteger	{ LongIntLiteral (fst $1, snd $1), snd $1 }
  | Float	{ FloatLiteral   (fst $1, snd $1), snd $1 }
  | Imaginary	{ ImagLiteral    (fst $1, snd $1), snd $1 }
;


/********** 5.2.3 Parenthesized forms **********/
parenth_form:
   Leftparen Rightparen		{ TupleExpr ([], union_pos $1 $2) }

 | Leftparen expr_list_without_comma Rightparen
      {  let (el,_) = $2 in
	 if List.length el = 1		(* If there was only one expression,  *)
	    then List.hd el		(* then this is just that expression. *)
				        (* Otherwise, it's a tuple.           *)
	    else TupleExpr (el, union_pos $1 $3)
      }

 | Leftparen expr_list_without_comma Comma Rightparen
      { TupleExpr (fst $2, union_pos $1 $4) }
;


/********** 5.2.4 List displays **********/
list_display:
   Leftbracket Rightbracket		{ ListExpr (List [], union_pos $1 $2) }
 | Leftbracket listmaker Rightbracket	{ ListExpr ($2, union_pos $1 $3)      }
;

listmaker:
/*   expression list_for	{ ListComp ($1, $2) } */
 | expr_list		{ List (fst $1) }
;

/* Using target_list instead of expr_list, since that's how for_stmt is. */
/*
list_for:
   For target_list In expr_list
      {  let (el,elp) = $4 in
	 ForStmt ($2, el, [], [], union_pos $1 elp)
      }

 | For target_list In expr_list list_iter
      {  let (el,elp) = $4 in
	 ForStmt ($2, el, [$5], [], union_pos $1 (pos_of_stmt $5))
      }
;

list_iter:
   list_for	{ $1 }
 | list_if	{ $1 }
;

list_if:
   If expression	{ IfStmt($2, [], [], union_pos $1 (pos_of_expr $2)) }

 | If expression list_iter
      { IfStmt ($2, [$3], [], union_pos $1 (pos_of_stmt $3)) }
;
*/


/********** 5.2.5 Dictionary displays **********/
dict_display:
   Leftbrace Rightbrace			{ DictExpr ([], union_pos $1 $2)     }
 | Leftbrace key_datum_list Rightbrace	{ DictExpr (fst $2, union_pos $1 $3) }
;

key_datum_list:
   key_datum_list_without_comma	       { $1				 }
 | key_datum_list_without_comma Comma  { (fst $1, union_pos (snd $1) $2) }
;

key_datum_list_without_comma:
   key_datum			{ let (e1,e2,p) = $1 in ([(e1, e2)], p) }

 | key_datum_list_without_comma Comma key_datum
      {  let (kdl,p1) = $1 in
	 let (kd1,kd2,p2) = $3 in
	 (kdl @ [(kd1, kd2)], union_pos p1 p2)
      }
;

key_datum:
   expression Colon expression
      { ($1, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;


/********** 5.2.6 String conversions **********/
string_conversion:
   Backquote expr_list_without_comma Backquote
      { StrConvExpr (fst $2, union_pos $1 $3) }

/*   Backquote expr_list Backquote	{ "`" ^ $2 ^ "`" }
 * According to the grammar, this should work.  But it doesn't work in
 * cPython, so I'm not going to worry about it here.
 */
;


/********** 5.3 Primaries **********/
primary:  /* expr */
    atom		{ $1 }
  | attributeref	{ $1 }
/*| subscription	{ $1 } */	/* Omitted: see below */
  | slicing		{ $1 }
  | call		{ $1 }
;


/********** 5.3.1 Attribute references **********/
attributeref:
    primary Period Identifier
      { AttrRefExpr ($1, fst $3, union_pos (pos_of_expr $1) (snd $3)) }
;


/********** 5.3.2 Subscriptions **********/
/* The language produced by this rule is a proper subset of the language
 * produced by the slicing rule.  Since this rule yields two reduce/reduce
 * conflicts when included, I'll just leave it out.
 *
subscription:
    primary Leftbracket expr_list Rightbracket	{ $1 ^ "[" ^ $3 ^ "]" }
;
*/


/********** 5.3.3 Slicings **********/
slicing:
   primary Leftbracket slice_list Rightbracket
      { SliceExpr ($1, fst $3, union_pos (pos_of_expr $1) $4) }

/* | primary Leftbracket slice_item Rightbracket	{ "" }
 * Excluded because a slice_item constitutes a slice_list and including it
 * yields a shift/reduce conflict. */
;

slice_list:
   slice_list_without_comma	   { $1                              }
 | slice_list_without_comma Comma  { (fst $1, union_pos (snd $1) $2) }
;

slice_list_without_comma:
   slice_item					 { ([fst $1], snd $1) }
 | slice_list_without_comma Comma slice_item
      { ((fst $1) @ [fst $3], union_pos (snd $1) (snd $3)) }
;

slice_item:
   expression			{ ExprSlice ($1), pos_of_expr $1 }
 | short_slice			{ $1 }
 | short_slice Colon
      {  let ss, pos = $1 in
	 match ss with
	    SimpleSlice(lower, upper) ->
	       ExtSlice([lower; upper; IdExpr (none, no_pos)]), pos
	  | _ ->
	    fatal_error pos "InternalParserError: unexpected short_slice"
      }

 | short_slice Colon expression
      {  let ss, pos = $1 in
	 match ss with
	    SimpleSlice(lower, upper) -> ExtSlice([lower; upper; $3]), pos
	  | _ ->
	    fatal_error pos "InternalParserError: unexpected short_slice"
      }

 | Ellipsis			{ ExtSlice ([IdExpr (ellipsis, no_pos)]), $1 }
;

short_slice:
   Colon		{ SimpleSlice(zero, IdExpr(sys_maxint, no_pos)), $1 }
 | expression Colon	{ SimpleSlice($1, IdExpr(sys_maxint, no_pos)), union_pos (pos_of_expr $1) $2 }
 | Colon expression	{ SimpleSlice(zero, $2), union_pos $1 (pos_of_expr $2) }

 | expression Colon expression
      { SimpleSlice($1, $3), union_pos (pos_of_expr $1) (pos_of_expr $3) }
;


/********** 5.3.4 Calls **********/
call:
   primary Leftparen Rightparen
      { CallExpr($1, [], union_pos (pos_of_expr $1) $3) }

 | primary Leftparen arg_list Rightparen
      { CallExpr ($1, List.rev $3, union_pos (pos_of_expr $1) $4) }
;

arg_list:		/* Backwards! */
   arg_comma_star argument optional_comma	{ $2 :: $1 }
 | arg_comma_star Star atom
      { StarArg ($3, pos_of_expr $3) :: $1 }

 | arg_comma_star Star atom Comma StarStar atom
      { StarStarArg ($6, pos_of_expr $6) :: StarArg ($3, pos_of_expr $3) :: $1 }

 | arg_comma_star StarStar atom
      { StarStarArg ($3, pos_of_expr $3) :: $1 }
;

arg_comma_star:		/* Backwards! */
					{ [] }
 | arg_comma_star argument Comma	{ $2 :: $1 }
;

argument:
   expression			{ let p = pos_of_expr $1 in PosArg ($1, p) }
 | Identifier Equal expression
      {  let pos = union_pos (snd $1) (pos_of_expr $3) in
	 KeywordArg (fst $1, $3, pos)
      }
;

/*
argument_list:
   positional_arguments optional_comma				{ $1 }
 | positional_arguments Comma keyword_arguments optional_comma
      { ((fst $1) @ (fst $3), union_pos (snd $1) (snd $3)) }

 | keyword_arguments optional_comma				{ $1 }
 | Star atom	{ [$2] }
 | Star atom Comma StarStar atom	{ [$2] }
 | positional_arguments Comma Star atom	{ $1 }
 | positional_arguments Comma Star atom Comma StarStar atom	{ $1 }
;

positional_arguments:
   expression		{ let p = pos_of_expr $1 in [PosArg ($1, p)], p }

 | positional_arguments Comma expression
      {  let ep = pos_of_expr $3 in
	 ((fst $1) @ [PosArg ($3, ep)], union_pos (snd $1) ep)
      }
;

keyword_arguments:
   Identifier Equal expression
      {  let pos = union_pos (snd $1) (pos_of_expr $3) in
	 ([KeywordArg(fst $1, $3, pos)], pos)
      }

 | keyword_arguments Comma Identifier Equal expression
      {  let (kwl,kwlp) = $1 in
	 let (s,p) = $3 in
	 (kwl @ [KeywordArg (s, $5, union_pos p (pos_of_expr $5))],
	    union_pos kwlp (pos_of_expr $5))
      }
;
*/

optional_comma:
	        { () }
 | Comma	{ () }
;


/********** 5.4 The power operator **********/
power:
    primary			{ $1 }
  | primary StarStar u_expr
      { OpExpr ($1, StarStar, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;


/********** 5.5 Unary arithetic operations **********/
u_expr:
    power	  { $1                                                      }
  | Dash u_expr	  { OpExpr (zero, Dash, $2, union_pos $1 (pos_of_expr $2))  }
  | Plus u_expr	  { OpExpr (zero, Plus, $2, union_pos $1 (pos_of_expr $2))  }
  | Tilde u_expr  { OpExpr (zero, Tilde, $2, union_pos $1 (pos_of_expr $2)) }
;


/********** 5.6 Binary arithmetic operations **********/
m_expr:
   u_expr		{ $1 }

 | m_expr Star u_expr
      { OpExpr ($1, Star, $3, union_pos (pos_of_expr $1) (pos_of_expr $3))    }

 | m_expr Slash u_expr
      { OpExpr ($1, Slash, $3, union_pos (pos_of_expr $1) (pos_of_expr $3))   }

 | m_expr Percent u_expr
      { OpExpr ($1, Percent, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

a_expr:
   m_expr		{ $1 }

 | a_expr Plus m_expr
      { OpExpr ($1, Plus, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }

 | a_expr Dash m_expr
      { OpExpr ($1, Dash, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;


/********** 5.7 Shifting operations **********/
shift_expr:
   a_expr			{ $1 }
 | shift_expr Leftshift a_expr
      { OpExpr ($1, Leftshift, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
 | shift_expr Rightshift a_expr
      { OpExpr ($1, Rightshift, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;


/********** 5.8 Binary bit-wise operations **********/
and_expr:
   shift_expr				{ $1 }

 | and_expr Ampersand shift_expr
      { OpExpr ($1, Ampersand, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

xor_expr:
   and_expr			{ $1 }

 | xor_expr Caret and_expr
      { OpExpr ($1, Caret, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

or_expr:
   xor_expr		{ $1 }

 | or_expr Bar xor_expr
      { OpExpr ($1, Bar, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;


/********** 5.9 Comparisons **********/
comparison:
    or_expr				{ $1 }

  | comparison comp_operator or_expr
      { OpExpr($1, $2, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

comp_operator:
    Lessthan		{ Lessthan	}
  | Greaterthan		{ Greaterthan	}
  | EqualEqual		{ EqualEqual	}
  | GreaterthanEq	{ GreaterthanEq	}
  | LessthanEq		{ LessthanEq	}
  | NotEqual		{ NotEqual	}
  | Is			{ Is		}
  | Is Not		{ IsNot		}
  | In			{ In		}
  | Not In		{ NotIn		}
;


/********** 5.10 Boolean operations **********/
expression:
   or_test	{ $1 }
 | lambda_form	{ $1 }
;

or_test:
    and_test		{ $1 }

  | or_test Or and_test
      { OpExpr($1, Or, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

and_test:
    not_test			{ $1 }

  | and_test And not_test
      { OpExpr($1, And, $3, union_pos (pos_of_expr $1) (pos_of_expr $3)) }
;

not_test:
    comparison		{ $1 }
  | Not not_test	{ OpExpr(zero, Not, $2, union_pos $1 (pos_of_expr $2)) }
;

lambda_form:
   Lambda Colon expression
      { FuncExpr([], [ReturnStmt($3, no_pos)], union_pos $1 (pos_of_expr $3)) }

 | Lambda parameter_list Colon expression
      { FuncExpr(fst $2, [ReturnStmt($4, no_pos)], union_pos $1 (pos_of_expr $4)) }
;


/********** 5.11 Expression lists *********/
expr_list_without_comma:
   expression					{ [$1], pos_of_expr $1 }
 | expr_list_without_comma Comma expression
      {  let (el,elp) = $1 in
	 el @ [$3], union_pos elp (pos_of_expr $3)
      }
;

expr_list:  /* expr list * pos */
   expr_list_without_comma	  { $1				    }
 | expr_list_without_comma Comma  { (fst $1), union_pos (snd $1) $2 }
;


/********** 6 Simple statements **********/
simple_stmt:
    expression_stmt		{ $1 }
  | assert_stmt			{ $1 }
  | assignment_stmt		{ $1 }
  | augmented_assignment_stmt	{ $1 }		/* Added by bustos */
  | pass_stmt			{ $1 }
  | del_stmt			{ $1 }
  | print_stmt			{ $1 }
  | return_stmt			{ $1 }
  | raise_stmt			{ $1 }
  | break_stmt			{ $1 }
  | continue_stmt		{ $1 }
  | import_stmt			{ $1 }
  | global_stmt			{ $1 }
/*  | exec_stmt			{ $1 } */	/* See exec_stmt below. */
;


/********** 6.1 Expression statements **********/
expression_stmt:
   expr_list	{ let (el,elp) = $1 in ExprList (el, elp) }
;


/********** 6.2 Assert statements **********/
assert_stmt:
    Assert expression
      { AssertStmt($2, None, union_pos $1 (pos_of_expr $2)) }

  | Assert expression Comma expression
      { AssertStmt($2, Some $4, union_pos $1 (pos_of_expr $4)) }
;


/********** 6.3 Assignment statements **********/
/* Using expr_list instead of target_list because the former produces a
 * superset of the latter and using target_list yields a reduce/reduce
 * conflict.
 */
assignment_stmt:
   expr_list Equal expr_list
      {  let (el1,p1) = $1 in
	 let (el2,p2) = $3 in
	 AssignStmt ([el1], el2, union_pos p1 p2)
      }

 | expr_list Equal assignment_stmt
      {  let (el,elp) = $1 in
         match $3 with
	    AssignStmt (dst,src,pos) ->
	       AssignStmt(dst @ [el], src, union_pos elp pos)
	  | _ ->
	    fatal_error $2 "InternalParserError: unexpected assignment_stmt"
      }
;

/* target_list is still used in for statements and del statements. */
target_list:  /* expr list * pos */
   target_list_without_comma	    { $1 }
 | target_list_without_comma Comma  { let (tl,tlp) = $1 in (tl, union_pos tlp $2) }
;

/* Using primary instead of target because the former produces a superset of
 * the latter and using target yields 12 reduce/reduce conflicts.
 */
target_list_without_comma:
   primary				{ [$1], pos_of_expr $1 }

 | target_list_without_comma Comma primary
      {  let (tl,tlp) = $1 in
	 (tl @ [$3], union_pos tlp (pos_of_expr $3))
      }
;

/*
target:
    Identifier					{ $1 }
  | Leftparen target_list Rightparen		{ "(" ^ $2 ^ ")" }
  | Leftbracket target_list Rightbracket	{ "[" ^ $2 ^ "]" }
  | attributeref				{ $1 }
  | subscription				{ $1 }
  | slicing					{ $1 }
;
*/


/********** 6.3.1 Augmented assignment statements **********/
augmented_assignment_stmt:
   primary augop expr_list
      {  let (el,elp) = $3 in
	 AugAssignStmt ($1, $2, el, union_pos (pos_of_expr $1) elp)
      }
;

augop:
   PlusEqual		{ PlusEqual       }
 | DashEqual		{ DashEqual       }
 | StarEqual		{ StarEqual       }
 | SlashEqual		{ SlashEqual      }
 | PercentEqual		{ PercentEqual    }
 | StarStarEqual	{ StarStarEqual   }
 | RightshiftEqual	{ RightshiftEqual }
 | LeftshiftEqual	{ LeftshiftEqual  }
 | AmpersandEqual	{ AmpersandEqual  }
 | CaretEqual		{ CaretEqual      }
 | BarEqual		{ BarEqual        }
;


/********** 6.4 The pass statement **********/
pass_stmt:
   Pass		{ PassStmt $1 }
;


/********** 6.5 The del statement **********/
del_stmt:
   Del target_list	{ let (tl,tlp) = $2 in DelStmt (tl, union_pos $1 tlp) }
;


/********** 6.6 The print statement **********/
print_stmt:
    Print		{ PrintStmt (none_expr, [], $1) }
  | Print expr_list
      { let (el,elp) = $2 in PrintStmt (none_expr, el, union_pos $1 elp) }

  | Print Rightshift expression
      { PrintStmt ($3, [], union_pos $1 (pos_of_expr $3)) }

  | Print Rightshift expression Comma expr_list
      { let (el,elp) = $5 in PrintStmt ($3, el, union_pos $1 elp) }
;


/********** 6.7 The return statement **********/
return_stmt:
   Return		{ ReturnStmt (none_expr, $1) }
 | Return expr_list
      {  let (el,elp) = $2 in
	 let pos = union_pos $1 elp in
	 if List.length el = 1
	    then ReturnStmt (List.hd el, pos)
	    else ReturnStmt (TupleExpr (el, elp), pos)
      }
;


/********** 6.8 The raise statement **********/
raise_stmt:
    Raise		{ RaiseStmt (none_expr, none_expr, none_expr, $1) }
  | Raise expression
      { RaiseStmt ($2, none_expr, none_expr, union_pos $1 (pos_of_expr $2)) }

  | Raise expression Comma expression
      { RaiseStmt ($2, $4, none_expr, union_pos $1 (pos_of_expr $4)) }

  | Raise expression Comma expression Comma expression
      { RaiseStmt ($2, $4, $6, union_pos $1 (pos_of_expr $6)) }
;


/********** 6.9 The break statement **********/
break_stmt:
   Break	{ BreakStmt $1 }
;


/********** 6.10 The continue statement **********/
continue_stmt:
   Continue	{ ContStmt $1 }
;


/********** 6.11 The import statement **********/
import_stmt:
    Import module_list
      {  let (ml,mlp) = $2 in
	 ImportStmt ([], ml, union_pos $1 mlp)
      }

  | From module_name Import Star
      {  let (mn,mnp) = $2 in
	 ImportStmt (mn, [], union_pos $1 $4)
      }

  | From module_name Import import_ident_list
      {  let (mn,mnp) = $2 in
	 let (il,ilp) = $4 in
	 ImportStmt (mn, il, union_pos $1 ilp)
      }
;

module_list:
    module_name		{ let (mn,mnp) = $1 in [mn, none], mnp }

  | module_name Identifier Identifier
      /* Outside of this context, "as" is an identifier.  So we take it as one
       * here, and verify that is in fact "as". */
      {  let (s,_) = $2 in
	 if Symbol.to_string s = "as"
	    then
	       let (mn,mnp) = $1 in
	       let (id,idp) = $3 in
	       [mn, id], union_pos mnp idp
	    else raise Parsing.Parse_error
      }

  | module_list Comma module_name
      {  let (ml,mlp) = $1 in
	 let (mn,mnp) = $3 in
	 ml @ [mn, none], union_pos mlp mnp
      }

  | module_list Comma module_name Identifier Identifier
      /* As above. */
      {  let (s,_) = $4 in
	 if Symbol.to_string s = "as"
	    then
	       let (ml,mlp) = $1 in
	       let (mn,mnp) = $3 in
	       let (id,idp) = $5 in
	       ml @ [mn, id], union_pos mlp idp

	    else raise Parsing.Parse_error
      }
;

/* Have to use "module_name" instead of "module" because the latter is an
 * OCaml keyword and ocamlc chokes on it. */
module_name:
    Identifier	{ let (s,p) = $1 in [s], p }
  | module_name Period Identifier
      {  let (mn,mnp) = $1 in
	 let (s,p) = $3 in
	 mn @ [s], union_pos mnp p
      }
;

import_ident_list:
   Identifier	{ let (id,idp) = $1 in [[id], none], idp }

 | Identifier Identifier Identifier
      {  let (s,_) = $2 in
	 if Symbol.to_string s = "as"
	    then
	       let (id1,idp1) = $1 in
	       let (id2,idp2) = $3 in
	       [[id1], id2], union_pos idp1 idp2
	    else raise Parsing.Parse_error
      }

 | import_ident_list Comma Identifier
      {  let (il,ilp) = $1 in
	 let (id,idp) = $3 in
	 il @ [[id], none], union_pos ilp idp
      }

 | import_ident_list Comma Identifier Identifier Identifier
      {  let (s,_) = $4 in
	 if Symbol.to_string s = "as"
	    then
	       let (il,ilp) = $1 in
	       let (id1,idp1) = $3 in
	       let (id2,idp2) = $5 in
	       il @ [[id1], id2], union_pos ilp idp2
	    else raise Parsing.Parse_error
      }
;


/********** 6.12 The global statement **********/
global_stmt:
   Global identifier_list
      { let (il,ilp) = $2 in GlobalStmt (il, union_pos $1 ilp) }
;

identifier_list:
   Identifier				{ let (id,idp) = $1 in [id], idp }
 | identifier_list Comma Identifier
      {  let (il,ilp) = $1 in
	 let (id,idp) = $3 in
	 il @ [id], union_pos ilp idp
      }
;


/********** 6.13 The exec statement: not implemented **********/
/* Including this rule yields a shift/reduce conflict.  Rather than try to fix
 * it, I'll leave it out for now, since exec's will be difficult to support in
 * a compiler anyway.  */
/*
exec_stmt:
   Exec expression				{ PassStmt }
 | Exec expression In expression		{ PassStmt }
 | Exec expression In expression Comma expression
      { "exec " ^ $2 ^ " in " ^ $4 ^ ", " ^ $6 }
;
*/


/********** 7 Compound statements **********/
compound_stmt:
   if_stmt	{ $1 }
 | while_stmt	{ $1 }
 | for_stmt	{ $1 }
 | try_stmt	{ $1 }
 | funcdef	{ $1 }
 | classdef	{ $1 }
;

suite:
    stmt_list Newline				{ $1 }
  | Newline Indent statement_plus Dedent	{ $3 }
;

statement_plus:
   statement			{ [$1], pos_of_stmt $1 }
 | statement_plus statement
      {  let (stp,stpp) = $1 in
	 stp @ [$2], union_pos stpp (pos_of_stmt $2)
      }
;

statement:
    stmt_list Newline	{ let (sl,slp) = $1 in StmtList (sl, slp) }
  | compound_stmt	{ $1                                      }
;

/* stmt list * pos */
stmt_list:
   stmt_list_without_semicolon			{ $1 }
 | stmt_list_without_semicolon Semicolon
      { let (sl,slp) = $1 in sl, union_pos slp $2 }
;

stmt_list_without_semicolon:
   simple_stmt		{ [$1], pos_of_stmt $1 }

 | stmt_list_without_semicolon Semicolon simple_stmt
      { let (sl,slp) = $1 in $3 :: sl, union_pos slp (pos_of_stmt $3) }
;


/********** 7.1 The if statement **********/
if_stmt:
   If expression Colon suite
      { let (s,sp) = $4 in IfStmt ($2, s, [], union_pos $1 sp) }

 | If expression Colon suite elif_list
      {  let (s,sp) = $4 in
	 let (el,elp) = $5 in
	 IfStmt ($2, s, el, union_pos $1 elp)
      }
;

elif_list:
   Else Colon suite		{ let (s,sp) = $3 in s, union_pos $1 sp }

 | Elif expression Colon suite
      {  let (s,sp) = $4 in
	 let pos = union_pos $1 sp in
	 [IfStmt ($2, s, [], pos)], pos
      }

 | Elif expression Colon suite elif_list
      {  let (s,sp) = $4 in
	 let (el,elp) = $5 in
	 let pos = union_pos $1 elp in
	 [IfStmt ($2, s, el, pos)], pos
      }
;


/********** 7.2 The while statement **********/
while_stmt:
   While expression Colon suite
      { let (s,sp) = $4 in WhileStmt ($2, s, [], sp) }

 | While expression Colon suite Else Colon suite
      {  let (s1,sp1) = $4 in
	 let (s2,sp2) = $7 in
	 WhileStmt ($2, s1, s2, union_pos $1 sp2)
      }
;


/********** 7.3 The for statement **********/
for_stmt:
   For target_list In expr_list Colon suite
      {  let (tl,tlp) = $2 in
	 let (el,elp) = $4 in
	 let (s,sp) = $6 in
	 ForStmt (tl, el, s, [], union_pos $1 sp)
      }

 | For target_list In expr_list Colon suite Else Colon suite
      {  let (tl,tlp) = $2 in
	 let (el,elp) = $4 in
	 let (s1,sp1) = $6 in
	 let (s2,sp2) = $9 in
	 ForStmt (tl, el, s1, s2, union_pos $1 sp2)
      }
;


/********** 7.4 The try statement **********/
try_stmt:
   try_exc_stmt		{ $1 }
 | try_fin_stmt		{ $1 }
;

try_exc_stmt:
   Try Colon suite exc_list
      {  let (s,sp) = $3 in
	 let (el,elp) = $4 in
	 TryStmt (s, el, [], union_pos $1 elp)
      }

 | Try Colon suite exc_list Else Colon suite
      {  let (s1,sp1) = $3 in
	 let (el,elp) = $4 in
	 let (s2,sp2) = $7 in
	 TryStmt (s1, el, s2, union_pos $1 sp2)
      }
;

exc_list:
   exc_item		{ let a, b, c, d = $1 in [a, b, c], d }

 | exc_list exc_item
      {  let (el,elp) = $1 in
	 let (a,b,c,d) = $2 in
	 el @ [a, b, c], union_pos elp d
      }
;

exc_item:
   Except Colon suite
      { let (s,sp) = $3 in none_expr, none_expr, s, union_pos $1 sp }

 | Except expression Colon suite
      { let (s,sp) = $4 in $2, none_expr, s, union_pos $1 sp }

   /* Using primary instead of target.  See above. */
 | Except expression Comma primary Colon suite
      { let (s,sp) = $6 in $2, $4, s, union_pos $1 sp }
;

/* XXX Should this be its own statement type? */
try_fin_stmt:
   Try Colon suite Finally Colon suite
      {  let (s1,sp1) = $3 in
	 let (s2,sp2) = $6 in
	 TryStmt (s1, [], s2, union_pos $1 sp2)
      }
;


/********** 7.5 Function definitions **********/
funcdef:
   Def Identifier Leftparen Rightparen Colon suite
      {  let (id,idp) = $2 in
	 let (s,sp) = $6 in
	 FuncDef (id, [], s, union_pos $1 sp)
      }

 | Def Identifier Leftparen parameter_list Rightparen Colon suite
      {  let (id,idp) = $2 in
	 let (pl,plp) = $4 in
	 let (s,sp) = $7 in
	 FuncDef (id, pl, s, union_pos $1 sp)
      }
;

parameter_list:
   defparams optional_comma		{ $1 }

 | defparams Comma Star Identifier
      {  let (dp,dpp) = $1 in
	 let (id,idp) = $4 in
	 dp @ [ExtraPosParam (id, idp)], union_pos dpp idp
      }

 | defparams Comma StarStar Identifier
      {  let (dp,dpp) = $1 in
	 let (id,idp) = $4 in
	 dp @ [ExtraKwParam (id, idp)], union_pos dpp idp
      }

 | Star Identifier
      { let (id,idp) = $2 in [ExtraPosParam (id, idp)], union_pos $1 idp }

 | StarStar Identifier
      { let (id,idp) = $2 in [ExtraKwParam (id, idp)], union_pos $1 idp }

 | Star Identifier Comma StarStar Identifier
      {  let (id1,idp1) = $2 in
	 let (id2,idp2) = $5 in
	 [ExtraPosParam (id1, idp1); ExtraPosParam (id2, idp2)],
	    union_pos $1 idp2
      }

 | defparams Comma Star Identifier Comma StarStar Identifier
      {  let (dp,dpp) = $1 in
	 let (id1,idp1) = $4 in
	 let (id2,idp2) = $7 in
	 dp @ [ExtraPosParam (id1, idp1); ExtraKwParam (id2, idp2)],
	    union_pos dpp idp2
      }
;

defparams:
   defparam			{ [$1], pos_of_param $1 }

 | defparams Comma defparam
      { let (pl,plp) = $1 in pl @ [$3], union_pos plp (pos_of_param $3) }
;

defparam:
   parameter			{ $1 }

 | parameter Equal expression
      {  DefParam ($1, $3, union_pos (pos_of_param $1) (pos_of_expr $3)) }
;

parameter:
   Identifier			{ let (id,idp) = $1 in IdParam (id, idp) }

 | Leftparen sublist Rightparen
      { let (sl,slp) = $2 in SublistParam (sl, union_pos $1 $3) }
;

sublist:
   sublist_without_comma	{ $1 }
 | sublist_without_comma Comma	{ $1 }
;

sublist_without_comma:
   parameter					{ [$1], pos_of_param $1 }

 | sublist_without_comma Comma parameter
      { let (sl,slp) = $1 in sl @ [$3], union_pos slp (pos_of_param $3) }
;


/********** 7.6 Class definitions **********/
classdef:
   Class Identifier Colon suite
      {  let (id,idp) = $2 in
	 let (s,sp) = $4 in
	 ClassDef (id, [], s, union_pos $1 sp)
      }

 | Class Identifier inheritance Colon suite
      {  let (id,idp) = $2 in
	 let (s,sp) = $5 in
	 ClassDef (id, $3, s, union_pos $1 sp)
      }
;

inheritance:
   Leftparen Rightparen			{ []     }
 | Leftparen expr_list Rightparen	{ fst $2 }
;


/********** 8.2 File input **********/
file_input:
   file_input_list   { List.rev $1 }
;

file_input_list:
 | Newline			{ []        }
 | statement			{ [$1]      }
 | file_input_list Newline	{ $1        }
 | file_input_list statement	{ $2 :: $1  }
;


/********** 8.3 Interactive input **********/
/* Not relevent here.
interactive_input:
   Newline			{ StmtList ([], $1)	    }
 | stmt_list Newline		{ StmtList (fst $1, snd $1) }
 | compound_stmt Newline	{ $1			    }
;
*/


/********** 8.4 Expression input **********/
/* Maybe later.
eval_input:
   expr_list newline_star	{ fst $1 }
;

newline_star:
			        { () }
 | newline_star Newline		{ () }
;

input_input:
   expr_list Newline		{ fst $1 }
;
*/
