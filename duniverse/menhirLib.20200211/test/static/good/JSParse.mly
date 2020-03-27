%{
  module Parsing = ParsingOwn.Make(LexingOwn)
  open Parsing
  module Lexing = Parsing.Lexing
 open Annotation
 open AST

let raise_parse_error e =
	print_endline e;
	raise Parse_error e

let fsto3 (a, _, _) = a
let sndo3 (_, a, _) = a
let trdo3 (_, _, a) = a

let string_of_constant c =
  match c with
    Number (_,f)  -> string_of_float f
  | String (_,s)  -> s
  | True  _    -> "true"
  | False _    -> "false"
  | Undefined _-> ""
  | Null _     -> "null"

%}

/* Tokens revisited */

/* Values */

%token <Annotation.t> Ltrue
%token <Annotation.t> Lfalse
%token <Annotation.t * int> Lint
%token <Annotation.t * float> Lfloat
%token <Annotation.t * char> Lchar
%token <Annotation.t * string> Lstring
%token <Annotation.t * string> Lident
%token <Annotation.t * string * string> Lregexp
%token <Annotation.t> Lnull
%token <Annotation.t> Leof

/* to be ignored */

%token <Annotation.t> Lwhitespace
%token <Annotation.t> Lline_terminator
%token <Annotation.t * string> Lcomment
%token <Annotation.t * string> LDcomment
%token <Annotation.t * string> LCcomment
%token <Annotation.t> LInitBegin
%token <Annotation.t> LInitEnd


/* Keywords */

%token <Annotation.t> KWbreak
%token <Annotation.t> KWcase
%token <Annotation.t> KWcatch
%token <Annotation.t> KWcontinue
%token <Annotation.t> KWdefault
%token <Annotation.t> KWdelete
%token <Annotation.t> KWdo
%token <Annotation.t> KWelse
%token <Annotation.t> KWfinally
%token <Annotation.t> KWfor
%token <Annotation.t> KWfunction
%token <Annotation.t> KWif
%token <Annotation.t> KWin
%token <Annotation.t> KWinstanceof
%token <Annotation.t> KWnew
%token <Annotation.t> KWreturn
%token <Annotation.t> KWswitch
%token <Annotation.t> KWthis
%token <Annotation.t> KWthrow
%token <Annotation.t> KWtry
%token <Annotation.t> KWtypeof
%token <Annotation.t> KWvar
%token <Annotation.t> KWvoid
%token <Annotation.t> KWwhile
%token <Annotation.t> KWwith

/* Future reserved words */

%token <Annotation.t> FRWabstract
%token <Annotation.t> FRWboolean
%token <Annotation.t> FRWbyte
%token <Annotation.t> FRWchar
%token <Annotation.t> FRWclass
%token <Annotation.t> FRWconst
%token <Annotation.t> FRWdebugger
%token <Annotation.t> FRWenum
%token <Annotation.t> FRWexport
%token <Annotation.t> FRWextends
%token <Annotation.t> FRWfinal
%token <Annotation.t> FRWfloat
%token <Annotation.t> FRWgoto
%token <Annotation.t> FRWimplements
%token <Annotation.t> FRWint
%token <Annotation.t> FRWinterface
%token <Annotation.t> FRWlong
%token <Annotation.t> FRWnative
%token <Annotation.t> FRWpackage
%token <Annotation.t> FRWprivate
%token <Annotation.t> FRWprotected
%token <Annotation.t> FRWshort
%token <Annotation.t> FRWstatic
%token <Annotation.t> FRWsuper
%token <Annotation.t> FRWsynchronized
%token <Annotation.t> FRWthrows
%token <Annotation.t> FRWtransient
%token <Annotation.t> FRWvolatile

/* E4X context keywords */

%token <Annotation.t> CKWeach
%token <Annotation.t> CKWxml
%token <Annotation.t> CKWnamespace
%token <Annotation.t> CKWdefaultxmlnamespace

/* strucutral information (parenthesises, comma, dot, semicolon) */

%token <Annotation.t> Llparen
%token <Annotation.t> Lrparen
%token <Annotation.t> Llbrace
%token <Annotation.t> Lrbrace
%token <Annotation.t> Llbracket
%token <Annotation.t> Lrbracket
%token <Annotation.t> Lsemicolon
%token <Annotation.t> Lcomma
%token <Annotation.t> Ldot
%token <Annotation.t> Lhook
%token <Annotation.t> Lcolon

/* Assignments and operators */

%token <Annotation.t> Lassign
%token <Annotation.t> Lgreater
%token <Annotation.t> Lless
%token <Annotation.t> Lbang
%token <Annotation.t> Ltilde
%token <Annotation.t> Leq
%token <Annotation.t> Lle
%token <Annotation.t> Lge
%token <Annotation.t> Lne
%token <Annotation.t> Leqq
%token <Annotation.t> Lneq
%token <Annotation.t> Lsc_or
%token <Annotation.t> Lsc_and
%token <Annotation.t> Lincr
%token <Annotation.t> Ldecr
%token <Annotation.t> Lplus
%token <Annotation.t> Lminus
%token <Annotation.t> Lstar
%token <Annotation.t> Lslash
%token <Annotation.t> Lbit_and
%token <Annotation.t> Lbit_or
%token <Annotation.t> Lxor
%token <Annotation.t> Lrem
%token <Annotation.t> Llshift
%token <Annotation.t> Lrsignedshift
%token <Annotation.t> Lrunsignedshift
%token <Annotation.t> Lplusassign
%token <Annotation.t> Lminusassign
%token <Annotation.t> Lstarassign
%token <Annotation.t> Lslashassign
%token <Annotation.t> Landassign
%token <Annotation.t> Lorassign
%token <Annotation.t> Lxorassign
%token <Annotation.t> Lremassign
%token <Annotation.t> Llshiftassign
%token <Annotation.t> Lrsignedshiftassign
%token <Annotation.t> Lrunsignedshiftassign

/* E4X Punctuators */

%token <Annotation.t> Lddot
%token <Annotation.t> Lat
%token <Annotation.t> Ldcolon

/* E4X stuff */

%token <Annotation.t * string> XMLcomment
%token <Annotation.t * string> XMLcdata
%token <Annotation.t * string> XMLpi
%token <Annotation.t * string> XMLtag_chars
%token <Annotation.t> XMLassign
%token <Annotation.t> XMLtag_close /* > */
%token <Annotation.t> XMLempty_tag_close /* /> */
%token <Annotation.t * string> XMLattr_val
%token <Annotation.t> XMLwhitespace
%token <Annotation.t * string> XMLtext
%token <Annotation.t> XMLotag_open /* < */
%token <Annotation.t> XMLctag_open /* </ */

/* Startsymbol */

%start program
%type <string AST.program> program

/* ******************** */
/*    Toplevel-Rules    */
/* ******************** */

/* Productions */

/* Every Production despite program returns a triplet: */
/* (value, from_linenumber, to_linenumber)             */

/* better: (value,from_position,to_position)    */

%%

program :
   source_elements Leof
     {Program ((default_annotation (sndo3 $1) $2), (fsto3 $1))}
;

/* Source Elements */

source_elements :
  Leof                           { ([], $1, $1)}
| source_element                 { ([fsto3 $1], sndo3 $1, trdo3 $1) }
| source_elements source_element { (((fsto3 $1) @ [(fsto3 $2)]),
				                    sndo3 $1, trdo3 $2) }
;

source_element :
  statement
    {(Statement ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| function_declaration {$1}
;

/* Function declarations */

function_declaration :
  KWfunction identifier Llparen formal_parameter_list Lrparen
  Llbrace function_body Lrbrace
  {(Function_declaration ((default_annotation $1 $8),"",fsto3 $2, fsto3 $4,None, fsto3 $7),
    $1, $8)}
|
  LCcomment KWfunction identifier Llparen formal_parameter_list Lrparen
  Llbrace function_body Lrbrace
  {(Function_declaration ((default_annotation (fst $1) $9),snd $1,fsto3 $3, fsto3 $5, None, fsto3 $8),
    fst $1, $9) }
;

function_expression :
  KWfunction Llparen formal_parameter_list Lrparen
    Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation $1 $7), None, None, fsto3 $3, None, fsto3 $6), $1, $7)}
| KWfunction identifier Llparen formal_parameter_list Lrparen
    Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation $1 $8), None, (Some (fsto3 $2)), fsto3 $4, None, fsto3 $7),
      $1, $8)}
| LCcomment KWfunction Llparen formal_parameter_list Lrparen
    Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation (fst $1) $8), Some (snd $1), None, fsto3 $4, None, fsto3 $7), (fst $1), $8)}
| LCcomment KWfunction identifier Llparen formal_parameter_list Lrparen
    Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation (fst $1) $9),Some (snd $1), (Some (fsto3 $3)), fsto3 $5, None, fsto3 $8),
      (fst $1), $9)}
| KWfunction Llparen formal_parameter_list Lrparen
    LCcomment Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation $1 $8), Some (snd $5) ,None, fsto3 $3, None, fsto3 $7), $1, $8)}
| KWfunction identifier Llparen formal_parameter_list Lrparen
    LCcomment Llbrace function_body Lrbrace
    {(Function_expression ((default_annotation $1 $9),Some (snd $6) ,(Some (fsto3 $2)), fsto3 $4, None, fsto3 $8),
      $1, $9)}
    ;

formal_parameter_list :
                                          {([], null_annotation, null_annotation)}
| identifier                              {([fsto3 $1], sndo3 $1, trdo3 $1)}
| formal_parameter_list Lcomma identifier {((fsto3 $1) @ [fsto3 $3], sndo3 $1, trdo3 $3)}
;

function_body :
  source_elements {$1}
;

/* ************** */
/*   Statements   */
/* ************** */

/* Top-Level for statements */

statement :
  block                {$1}
| variable_statement   {$1}
| empty_statement      {$1}
| expression_statement {$1}
| if_statement         {$1}
| iteration_statement  {$1}
| continue_statement   {$1}
| break_statement      {$1}
| return_statement     {$1}
| with_statement       {$1}
| labelled_statement   {$1}
| switch_statement     {$1}
| throw_statement      {$1}
| try_statement        {$1}
/* E4X Extends */
| default_xml_namespace_statement {$1};

/* Block */

block :
  Llbrace optional_statement_list Lrbrace
  {(Block ((default_annotation $1 $3), fsto3 $2), $1, $3)}
;

optional_statement_list :
                                     {([], null_annotation,null_annotation)}
| non_empty_statement_list           {$1}
;

non_empty_statement_list :
  statement
  {([fsto3 $1], sndo3 $1, trdo3 $1)}
| non_empty_statement_list statement
  {(((fsto3 $1) @ [fsto3 $2]), sndo3 $1, trdo3 $2)}
;

/* Variable Statement */

variable_statement :
  KWvar variable_declaration_list Lsemicolon
  {(Variable_declaration
      ((default_annotation $1 $3), (fsto3 $2)),
      $1, $3)}
;

variable_declaration_list :
  variable_declaration
    {([fsto3 $1], sndo3 $1, trdo3 $1)}
| variable_declaration_list Lcomma variable_declaration
    {(((fsto3 $1) @ [fsto3 $3]), sndo3 $1, trdo3 $3)}
;

variable_declaration_list_no_in :
  variable_declaration_no_in
    {([fsto3 $1], sndo3 $1, trdo3 $1)}
| variable_declaration_list_no_in Lcomma variable_declaration_no_in
    {(((fsto3 $1) @ [fsto3 $3]), sndo3 $1, trdo3 $3)}
;

variable_declaration :
  identifier             {(((fsto3 $1), None), sndo3 $1, trdo3 $1)}
| identifier initialiser {(((fsto3 $1), Some (fsto3 $2)), sndo3 $1, trdo3 $2)}
;

variable_declaration_no_in :
  identifier                   {(((fsto3 $1), None), sndo3 $1, trdo3 $1)}
| identifier initialiser_no_in {(((fsto3 $1), Some (fsto3 $2)), sndo3 $1, trdo3 $2)}
;

initialiser :
  Lassign assignment_expression {((fsto3 $2), $1, trdo3 $2)}
;

initialiser_no_in :
  Lassign assignment_expression_no_in {((fsto3 $2), $1, trdo3 $2)}
;

/* Skip */

empty_statement :
  Lsemicolon {(Skip (default_annotation $1 $1), $1, $1)}
;

/* Expression */

expression_statement :
  init_expression Lsemicolon {(Expression ((default_annotation (sndo3 $1) $2), (fsto3 $1)),
			       sndo3 $1, $2)}
;

/* Conditionals  */

/* This produces a shift/reduce - conflict, since in nested if-constructs
 * it is not clear to which if the else refers. ocamlyacc associates it
 * to the nearest possible if by shifting immediatly. This is the correct
 * behaviour according to the ECMAScript Language Specification 12.5.
 */

if_statement :
  KWif Llparen expression Lrparen statement KWelse statement
    {(If ((default_annotation $1 (trdo3 $7)), fsto3 $3, fsto3 $5, Some (fsto3 $7)),
     $1, trdo3 $7)}
| KWif Llparen expression Lrparen statement
    {(If ((default_annotation $1 (trdo3 $5)), fsto3 $3, fsto3 $5, None), $1, trdo3 $5)}
;

switch_statement :
  KWswitch Llparen expression Lrparen
    case_block
    {(Switch ((default_annotation $1 (trdo3 $5)),
	      fsto3 $3, (fsto3 (fsto3 $5)),
	      (sndo3 (fsto3 $5)), (trdo3 (fsto3 $5))),
      $1, trdo3 $5)}
;

/* The case_clauses after the default_clause are treated similar
   to the ones before. But if there's a satisfied expression in
   the first list and no break in the first list but one in the
   default case, than the second list is not evaluated. One small
   difference, but we have to keep them in separate lists. */
;

case_block :
  Llbrace case_clauses Lrbrace                             {((fsto3 $2, None, []), $1, $3)}
| Llbrace Lrbrace                                          {(([], None, []), $1, $2)}
| Llbrace default_clause Lrbrace                           {(([], Some (fsto3 $2), []), $1, $3)}
| Llbrace case_clauses default_clause Lrbrace
    {((fsto3 $2, Some (fsto3 $3), []), $1, $4)}
| Llbrace default_clause case_clauses Lrbrace
    {(([], Some (fsto3 $2), fsto3 $3), $1, $4)}
| Llbrace case_clauses default_clause case_clauses Lrbrace
    {((fsto3 $2, Some (fsto3 $3), fsto3 $4), $1, $5)}

case_clauses :
  case_clause              {([fsto3 $1], sndo3 $1, trdo3 $1)}
| case_clauses case_clause {((fsto3 $1) @ [fsto3 $2], sndo3 $1, trdo3 $2)}
;

case_clause :
  KWcase expression Lcolon                          {((fsto3 $2, None), $1, $3)}
| KWcase expression Lcolon non_empty_statement_list
    {((fsto3 $2, Some (fsto3 $4)), $1, trdo3 $4)}
;

default_clause :
  KWdefault Lcolon                          {(None, $1, $2)}
| KWdefault Lcolon non_empty_statement_list {(Some (fsto3 $3), $1, trdo3 $3)}
;

/* Iterations */

iteration_statement :
  KWdo statement KWwhile Llparen expression Lrparen Lsemicolon
    {(Do ((default_annotation $1 $7), fsto3 $2, fsto3 $5), $1, $7)}
| KWwhile Llparen expression Lrparen statement
    {(While ((default_annotation $1 (trdo3 $5)), fsto3 $3, fsto3 $5), $1, trdo3 $5)}
| KWfor Llparen for_bracket Lrparen statement
    {(For ((default_annotation $1 (trdo3 $5)), fsto3 $3, fsto3 $5), $1, (trdo3 $5))}

/* Add for EAX  */
| KWfor CKWeach Llparen left_hand_side_expression KWin expression Lrparen statement
    {(For_each ((default_annotation $1 (trdo3 $8)),
		(With_in ((default_annotation $3 $7), fsto3 $4, fsto3 $6)), fsto3 $8),
      $1, trdo3 $8)}
| KWfor CKWeach Llparen KWvar variable_declaration_no_in KWin expression Lrparen statement
    {(For_each ((default_annotation $1 (trdo3 $9)),
	       (With_in_and_var ((default_annotation $3 $8),
				 fsto3 $5, fsto3 $7)),
	       fsto3 $9),
      $1, trdo3 $9)}
;

for_bracket :
  optional_expression_no_in Lsemicolon optional_expression Lsemicolon optional_expression
    {(Regular ((default_annotation (sndo3 $1) (trdo3 $5)), fsto3 $1, fsto3 $3, fsto3 $5),
      sndo3 $1, trdo3 $5)}
| KWvar variable_declaration_list_no_in Lsemicolon optional_expression Lsemicolon optional_expression
    {(Regular_var ((default_annotation $1 (trdo3 $6)), fsto3 $2, fsto3 $4, fsto3 $6),
      $1, (trdo3 $6))}
| left_hand_side_expression KWin expression
    {(With_in ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| KWvar variable_declaration_no_in KWin expression
    {(With_in_and_var ((default_annotation $1 (trdo3 $4)), fsto3 $2, fsto3 $4),
      $1, trdo3 $4)}
;

optional_expression :
             {(None, null_annotation, null_annotation)}
| expression {(Some (fsto3 $1), sndo3 $1, trdo3 $1)}
;

optional_expression_no_in :
                   {(None, null_annotation, null_annotation)}
| expression_no_in {(Some (fsto3 $1), sndo3 $1, trdo3 $1)}
;

/* Keyword-Statements */

continue_statement :
  KWcontinue
  identifier Lsemicolon
  {if (different_line $1 (sndo3 $2))
   then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
   else (Continue ((default_annotation $1 $3),Some (fsto3 $2)), $1, $3)}
| KWcontinue
  Lsemicolon
  {if (different_line $1 $2)
   then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
   else (Continue ((default_annotation $1 $2), None), $1, $2)}
;

break_statement :
  KWbreak
  identifier Lsemicolon
    {if (different_line (sndo3 $2)  $1)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
    else (Break ((default_annotation $1 $3), Some (fsto3 $2)), $1, $3)}
| KWbreak
  Lsemicolon
    {if (different_line $1 $2)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
    else (Break ((default_annotation $1 $2), None), $1, $2)}
;

return_statement :
  KWreturn
  expression Lsemicolon
    {if different_line (sndo3 $2) $1
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
    else (Return ((default_annotation $1 $3), Some (fsto3 $2)), $1, $3)}
| KWreturn
  Lsemicolon
    {if (different_line $1 $2)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
    else (Return ((default_annotation $1 $2), None), $1, $2)}
;

with_statement :
  KWwith Llparen expression Lrparen statement
    {(With ((default_annotation $1 (trdo3 $5)), fsto3 $3, fsto3 $5), $1, trdo3 $5)}
;

/* Other Control Structures */

labelled_statement :
  identifier Lcolon statement
    {(Labelled_statement ((default_annotation
			     (sndo3 $1)
			     (trdo3 $3)),
			  fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
;

throw_statement :
  KWthrow
  expression Lsemicolon
    {if (different_line (sndo3 $2) $1)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line $1))
    else (Throw ((default_annotation $1 $3), fsto3 $2), $1, $3)}
;

try_statement :
  KWtry block catch_prod
    {(Try_catch_finally ((default_annotation $1 (trdo3 $3)),
			 fsto3 $2, (Some (fsto3 $3)), None),
      $1, trdo3 $3)}
| KWtry block finally_prod
    {(Try_catch_finally ((default_annotation $1 (trdo3 $3)),
			 fsto3 $2, None, (Some (fsto3 $3))),
      $1, trdo3 $3)}
| KWtry block catch_prod finally_prod
    {(Try_catch_finally ((default_annotation $1 (trdo3 $4)),
			 fsto3 $2, (Some (fsto3 $3)), (Some (fsto3 $4))),
      $1, trdo3 $4)}
;

catch_prod :
  KWcatch Llparen identifier Lrparen block {((fsto3 $3, fsto3 $5), $1, trdo3 $5)}
;

finally_prod :
  KWfinally block {(fsto3 $2, $1, trdo3 $2)}
;



/* E4X Extends */
/* fuehrt zu 2 neuen Shift/Reduce Konflikten, ich weiss */
/* leider nicht, wie ich das weg kriege. Es wird wohl durch */
/* den default Fall beim case Statement verursacht. */
default_xml_namespace_statement :
    CKWdefaultxmlnamespace Lassign expression Lsemicolon
      {(DefaultXMLNamespace ((default_annotation $1 $4), fsto3 $3), $1, $4)}

   /* KWdefault CKWxml CKWnamespace Lassign expression Lsemicolon  */

/* **************** */
/*   Expressions    */
/* **************** */

/* Top-Level Expression structure */

/* NOTE: There are up to three versions of expression-rules:
 * expr (the regular expression rules)
 * init_expr (may not start with { or function) and
 * expr_no_in (does not contain keyword in).
 * This is necessary to circumvent ambiguities */

expression :
  sequence_of_expression
    { (Sequence
        ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
        sndo3 $1, trdo3 $1)
    };
expression_no_in :
  sequence_of_expression_no_in
    {(Sequence ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)};
init_expression :
  sequence_of_init_expression
    {(Sequence ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)};

sequence_of_expression :
  assignment_expression
    {([fsto3 $1], sndo3 $1, trdo3 $1)}
| sequence_of_expression Lcomma assignment_expression
    {((fsto3 $1) @ [fsto3 $3], sndo3 $1, trdo3 $3)};
sequence_of_expression_no_in :
  assignment_expression_no_in
    {([fsto3 $1], sndo3 $1, trdo3 $1)}
| sequence_of_expression_no_in Lcomma assignment_expression_no_in
    {((fsto3 $1) @ [fsto3 $3], sndo3 $1, trdo3 $3)};
sequence_of_init_expression :
  assignment_init_expression
    {([fsto3 $1], sndo3 $1, trdo3 $1)}
| sequence_of_init_expression Lcomma assignment_expression
    {((fsto3 $1) @ [fsto3 $3], sndo3 $1, trdo3 $3)};

member_expression :
  primary_expression
    {$1}
| function_expression
    {$1}
| member_expression Llbracket expression Lrbracket
    {(Array_access ((default_annotation (sndo3 $1) $4), fsto3 $1, fsto3 $3),
      sndo3 $1, $4)}
| member_expression Ldot identifier
    {(Object_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| KWnew member_expression arguments
    {(New_expression ((default_annotation $1 (trdo3 $2)), fsto3 $2, fsto3 $3), $1, trdo3 $2)}
  /* Regexp-Literals are used as new-constructor of a RegExp-Object */
| Lregexp
    {(RegExp ((default_annotation (fsto3 $1) (fsto3 $1)), (sndo3 $1, trdo3 $1)),
      fsto3 $1, fsto3 $1)}

/* Extends for EAX */
| member_expression Ldot property_identifier
    {(Property_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| member_expression Lddot identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| member_expression Lddot property_identifier
    {(Descendant_access ((default_annotation (sndo3 $1)(trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| member_expression Ldot Llparen expression Lrparen
    {(Filter_access ((default_annotation (sndo3 $1) $5), fsto3 $1, fsto3 $4),
      sndo3 $1, $5)}
;

member_init_expression :
  primary_init_expression
    {$1}
  /* no function_expression ! */
| member_init_expression Llbracket expression Lrbracket
    {(Array_access ((default_annotation (sndo3 $1) $4), fsto3 $1, fsto3 $3),
      sndo3 $1, $4)}
| member_init_expression Ldot identifier
    {(Object_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| KWnew member_expression arguments
    {(New_expression ((default_annotation $1 (trdo3 $3)), fsto3 $2, fsto3 $3),
      $1, trdo3 $3)}
| Lregexp
    {(RegExp ((default_annotation (fsto3 $1) (fsto3 $1)),
	      ((sndo3 $1), (trdo3 $1))), fsto3 $1, fsto3 $1)}

/* Extends for EAX */
| member_init_expression Ldot property_identifier
    {(Property_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| member_init_expression Lddot identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)), fsto3 $1, fsto3 $3),
      sndo3 $1, trdo3 $3)}
| member_init_expression Lddot property_identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)), (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| member_init_expression Ldot Llparen expression Lrparen
    {(Filter_access ((default_annotation (sndo3 $1) $5), fsto3 $1, fsto3 $4),
      sndo3 $1, $5)}
;


/* Basic Expressions */

primary_expression :
  KWthis
  { (This $1,$1,$1)}
| identifier
    {(Variable ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1), sndo3 $1, trdo3 $1)}
| literal
    {(Constant ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1), sndo3 $1, trdo3 $1)}
| array_literal
    {(Array_construction ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| object_literal
    {(Object_construction ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| Llparen expression Lrparen {(fsto3 $2, $1, $3)}
/* EAX Extends */
| property_identifier
    {(Property_construction ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| xml_initialiser            {$1}
| xml_list_initialiser       {$1}

primary_init_expression :
  KWthis
  { (This $1,$1,$1)}
| identifier
    {(Variable ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| literal
    {(Constant ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
| array_literal
    {(Array_construction ((default_annotation (sndo3 $1) (trdo3 $1)), fsto3 $1),
      sndo3 $1, trdo3 $1)}
  /* no object_literal! */
| Llparen expression Lrparen      {(fsto3 $2, $1, $3)}
;

literal :
  numeric_literal {$1}
| Lnull           {(Null (default_annotation $1 $1), $1, $1)}
| Ltrue           {(True (default_annotation $1 $1), $1, $1)}
| Lfalse          {(False (default_annotation $1 $1), $1, $1)}
| Lstring         {(String ((default_annotation (fst $1) (fst $1)), (snd $1)),
		    (fst $1), (fst $1))}
;

numeric_literal :
  Lfloat   {(Number ((default_annotation (fst $1) (fst $1)), (snd $1)), fst $1, fst $1)}
| Lint     {(Number ((default_annotation (fst $1) (fst $1)), (float_of_int (snd $1))),
	     fst $1, fst $1)}
;

/* quite different from the definition, but should work as well */

array_literal :
  Llbracket Lrbracket                        {([], $1, $2)}
| Llbracket non_empty_element_list Lrbracket {(fsto3 $2, $1, $3)}
;

non_empty_element_list :
  assignment_expression                               {([Some (fsto3 $1)], sndo3 $1, trdo3 $1)}
| non_empty_element_list Lcomma assignment_expression
    {((fsto3 $1) @ [Some (fsto3 $3)], sndo3 $1, trdo3 $3)}
| elision                                             {([fsto3 $1], sndo3 $1, trdo3 $1)}
| non_empty_element_list Lcomma elision
    {((fsto3 $1) @ [(fsto3 $3)], sndo3 $1, trdo3 $3)}
;

elision :
  Lcomma         {(None, $1, $1)}
;

object_literal :
  Llbrace Lrbrace                              {([], $1, $2)}
| Llbrace property_name_and_value_list Lrbrace {(fsto3 $2, $1, $3)}
;

/* Identifier for E4X Context Keywords */
identifier:
  Lident
    {(Identifier ((default_annotation (fst $1) (fst $1)), (snd $1)), fst $1, fst $1)}
| CKWeach
    {(Identifier ((default_annotation $1 $1),"each"), $1, $1)}
| CKWnamespace
    {(Identifier ((default_annotation $1 $1),"namespace"), $1, $1)}
| CKWxml
    {(Identifier ((default_annotation $1 $1),"xml"), $1, $1)}
;

property_identifier :
  attribute_identifier                          {$1}
| qualified_identifier                          {$1}
| wildcard_identifier                           {$1}
;

attribute_identifier :
  Lat property_selector
    {(AttributeIdentifier ((default_annotation $1 (trdo3 $2)), (fsto3 $2)), $1, trdo3 $2)}
| Lat qualified_identifier
    {(AttributeIdentifier ((default_annotation $1 (trdo3 $2)), (fsto3 $2)), $1, trdo3 $2)}
| Lat Llbracket expression Lrbracket
    {(AttributeIdentifierExp ((default_annotation $1 $4), (fsto3 $3)), $1, $4)}
;

property_selector :
  identifier          {$1}
| wildcard_identifier {$1}
;

qualified_identifier:
  property_selector Ldcolon property_selector
    {(QualifiedIdentifier ((default_annotation (sndo3 $1) (trdo3 $3)),
			   (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| property_selector Ldcolon Llbracket expression Lrbracket
    {(QualifiedIdentifierExp ((default_annotation (sndo3 $1) $5), (fsto3 $1), (fsto3 $4)),
      sndo3 $1, $5)}
;

wildcard_identifier:
  Lstar {(Wildcard (default_annotation $1 $1), $1, $1)}
;

xml_initialiser:
  xml_markup  {((XMLInitialiser ((default_annotation (sndo3 $1) (trdo3 $1)),(fsto3 $1))),
		sndo3 $1, trdo3 $1)}
| xml_element {((XMLInitialiser ((default_annotation (sndo3 $1) (trdo3 $1)),(fsto3 $1))),
		sndo3 $1, trdo3 $1)}
;

xml_element:
  XMLotag_open xml_tag_content_option XMLempty_tag_close
           {(XMLElementEmpty ((default_annotation $1 $3), (fsto3 $2)), $1, $3)}
| XMLotag_open xml_tag_content_option XMLtag_close xml_element_content /* opt */
    XMLctag_open xml_tag_content_option XMLtag_close
    {(XMLElement ((default_annotation $1 $7),(fsto3 $2), Some (fsto3 $4), (fsto3 $6)),
      $1, $7)}
| XMLotag_open xml_tag_content_option XMLtag_close
    XMLctag_open xml_tag_content_option XMLtag_close
    {(XMLElement ((default_annotation $1 $6),(fsto3 $2), None, (fsto3 $5)), $1, $6)}
;

xml_element_content:
    xml_markup
    {(XMLElementContent ((default_annotation (sndo3 $1) (trdo3 $1)), (fsto3 $1), None),
      sndo3 $1, trdo3 $1)}
| xml_markup xml_element_content
    {(XMLElementContent ((default_annotation (sndo3 $1) (trdo3 $2)),
			 fsto3 $1, Some (fsto3 $2)),
      sndo3 $1, trdo3 $2)}
| XMLtext
    {(XMLElementContent ((default_annotation (fst $1) (fst $1)),
			 XMLText ((default_annotation (fst $1) (fst $1)), (snd $1)), None),
     fst $1, fst $1)}
| XMLtext xml_element_content
    {(XMLElementContent ((default_annotation (fst $1) (trdo3 $2)),
			 XMLText ((default_annotation (fst $1) (trdo3 $2)),
				  (snd $1)), Some (fsto3 $2)),
      fst $1, trdo3 $2)}
| xml_element
    {(XMLElementContent ((default_annotation (sndo3 $1) (trdo3 $1)),
			 (fsto3 $1), None), (sndo3 $1), (trdo3 $1))}
| xml_element xml_element_content
    {(XMLElementContent ((default_annotation (sndo3 $1) (trdo3 $2)),
			 (fsto3 $1), Some (fsto3 $2)),
      sndo3 $1, trdo3 $2)}
| Llbrace expression Lrbrace
    {(XMLElementContent ((default_annotation $1 $3),
			 XMLExpression ((default_annotation $1 $3),(fsto3 $2)), None),
      $1, $3)}
| Llbrace expression Lrbrace xml_element_content
    {(XMLElementContent ((default_annotation $1 $3),
			 XMLExpression ((default_annotation $1 $3),
					(fsto3 $2)), Some (fsto3 $4)),
      $1, $3)}
;


xml_list_initialiser :
  XMLotag_open XMLtag_close xml_element_content XMLctag_open XMLtag_close
    {(XMLListInitialiser ((default_annotation $1 $5),(fsto3 $3)), $1, $5)}
;


xml_tag_content_option :
  xml_tag_content                            {([(fsto3 $1)], sndo3 $1, trdo3 $1)}
| xml_tag_content  xml_tag_content_option    {([(fsto3 $1)] @ (fsto3 $2), sndo3 $1, trdo3 $2)}
;

xml_tag_content :
  XMLtag_chars
    {(XMLTagChars ((default_annotation (fst $1) (fst $1)),(snd $1)), fst $1, fst $1)}
| XMLwhitespace
    {(XMLWhitespace (default_annotation $1 $1), $1, $1)}
| Llbrace expression Lrbrace
    {(XMLExpression ((default_annotation $1 $3),(fsto3 $2)), $1, $3)}
| XMLassign Llbrace expression Lrbrace
    {(XMLAssignExpression ((default_annotation $1 $4),(fsto3 $3)), $1, $4)}
| XMLassign XMLattr_val
    {(XMLAssignAttr ((default_annotation $1 (fst $2)), (snd $2)), $1, fst $2)}
;


xml_markup :
  XMLcomment {(XMLComment ((default_annotation (fst $1) (fst $1)),(snd $1)), fst $1, fst $1)}
| XMLcdata   {(XMLCDATA ((default_annotation (fst $1) (fst $1)),(snd $1)), fst $1, fst $1)}
| XMLpi      {(XMLPI ((default_annotation (fst $1) (fst $1)),(snd $1)), fst $1, fst $1)}
;

property_name_and_value_list :
    property_name Lcolon assignment_expression
    {([((fsto3 $1), (fsto3 $3))], sndo3 $1, trdo3 $3)}
| property_name_and_value_list Lcomma
    property_name Lcolon assignment_expression
    {((fsto3 $1) @ [((fsto3 $3), (fsto3 $5))], sndo3 $1, trdo3 $5)}
;

property_name :
  identifier
    {((DynamicName ((default_annotation (sndo3 $1) (trdo3 $1)),(fsto3 $1))),
      sndo3 $1, trdo3 $1)}
| Lstring
    {((StaticName ((default_annotation (fst $1) (fst $1)),
		   String ((default_annotation (fst $1) (fst $1)),(snd $1)))),
      fst $1, fst $1)}
| numeric_literal
    {((StaticName ((default_annotation (sndo3 $1) (trdo3 $1)),(fsto3 $1))),
      sndo3 $1, trdo3 $1)}
;

new_expression :
  member_expression         {$1}
| KWnew new_expression      {(New_expression ((default_annotation $1 (trdo3 $2)),
					      (fsto3 $2),[]),
			      $1, trdo3 $2)};

new_init_expression :
  member_init_expression    {$1}
| KWnew new_expression      {(New_expression ((default_annotation $1 (trdo3 $2)),
					      (fsto3 $2),[]),
			      $1, trdo3 $2)};

call_expression :
    member_expression arguments
    {(Function_call ((default_annotation (sndo3 $1) (trdo3 $2)),
		     (fsto3 $1), (fsto3 $2)),
      sndo3 $1, trdo3 $2)}
| call_expression arguments
    {(Function_call (default_annotation (sndo3 $1) (trdo3 $2),
		      (fsto3 $1), (fsto3 $2)),
		     sndo3 $1, trdo3 $2)}
| call_expression Llbracket expression Lrbracket
    {(Array_access ((default_annotation (sndo3 $1) $4),
		    (fsto3 $1), (fsto3 $3)),
      sndo3 $1, $4)}
| call_expression Ldot identifier
    {(Object_access ((default_annotation (sndo3 $1) (trdo3 $3)),
		     (fsto3 $1), ((fsto3 $3))),
      sndo3 $1, trdo3 $3)}

/* E4X Extends */
| call_expression Ldot property_identifier
    {(Property_access ((default_annotation (sndo3 $1) (trdo3 $3)),
		       (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| call_expression Lddot identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)),
			 (fsto3 $1), ((fsto3 $3))),
      sndo3 $1, trdo3 $3)}
| call_expression Lddot property_identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)),
			 (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| call_expression Ldot Llparen expression Lrparen
    {(Filter_access ((default_annotation (sndo3 $1) $5),
		     (fsto3 $1), fsto3 $4),
      sndo3 $1, $5)}
;

call_init_expression :
  member_init_expression arguments
    {(Function_call ((default_annotation (sndo3 $1) (trdo3 $2)),
		     (fsto3 $1), (fsto3 $2)),
      sndo3 $1, trdo3 $2)}
| call_init_expression arguments
    {(Function_call ((default_annotation (sndo3 $1) (trdo3 $2)),
		     (fsto3 $1), (fsto3 $2)),
      sndo3 $1, trdo3 $2)}
| call_init_expression Llbracket expression Lrbracket
    {(Array_access ((default_annotation (sndo3 $1) $4),
		    (fsto3 $1), (fsto3 $3)),
      sndo3 $1, $4)}
| call_init_expression Ldot identifier
    {(Object_access ((default_annotation (sndo3 $1) (trdo3 $3)),
		     (fsto3 $1), ((fsto3 $3))),
      sndo3 $1, trdo3 $3)}

/* E4X Extends */
| call_init_expression Ldot property_identifier
    {(Property_access ((default_annotation (sndo3 $1) (trdo3 $3)),
		       (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| call_init_expression Lddot identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)),
			 (fsto3 $1), ((fsto3 $3))),
      sndo3 $1, trdo3 $3)}
| call_init_expression Lddot property_identifier
    {(Descendant_access ((default_annotation (sndo3 $1) (trdo3 $3)),
			 (fsto3 $1), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
| call_init_expression Ldot Llparen expression Lrparen
    {(Filter_access ((default_annotation (sndo3 $1) $5),
		     (fsto3 $1), (fsto3 $4)),
      sndo3 $1, $5)}
;


arguments :
  Llparen Lrparen               {([], $1, $2)}
| Llparen argument_list Lrparen {((fsto3 $2), $1, $3)}
;

argument_list :
  assignment_expression                      {([(fsto3 $1)], sndo3 $1, trdo3 $1)}
| argument_list Lcomma assignment_expression {((fsto3 $1) @ [(fsto3 $3)], sndo3 $1, trdo3 $3)}
;

left_hand_side_expression :
  new_expression       {$1}
| call_expression      {$1}
;

left_hand_side_init_expression :
  new_init_expression  {$1}
| call_init_expression {$1}
;

/* Operations */

postfix_expression :
  left_hand_side_expression
    {$1}
| left_hand_side_expression
  Lincr
    { if (different_line (trdo3 $1)  $2)
      then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line (trdo3 $1)))
      else (Unop ((default_annotation (sndo3 $1) $2),(fsto3 $1),
	    Incr_postfix (default_annotation $2 $2)),
      sndo3 $1, $2)}
| left_hand_side_expression
  Ldecr
    { if (different_line (trdo3 $1) $2)
      then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line (trdo3 $1)))
      else (Unop ((default_annotation (sndo3 $1) $2),(fsto3 $1),
	    Decr_postfix (default_annotation $2 $2)),
      sndo3 $1, $2)}
;

postfix_init_expression :
  left_hand_side_init_expression
    {$1}
| left_hand_side_init_expression
  Lincr
    {if (different_line (sndo3 $1) $2)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line (sndo3 $1)))
    else (Unop ((default_annotation (sndo3 $1) $2),(fsto3 $1),
	    Incr_postfix (default_annotation $2 $2)),
      sndo3 $1, $2)}
| left_hand_side_init_expression
  Ldecr
    {if (different_line (sndo3 $1) $2)
    then raise_parse_error("Illegal line terminator in line "^(string_of_starting_line (sndo3 $1)))
    else (Unop ((default_annotation (sndo3 $1) $2),(fsto3 $1),
	    Decr_postfix (default_annotation $2 $2)),
      sndo3 $1, $2)}
;

unary_expression :
  postfix_expression             {$1}
| KWdelete unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Delete (default_annotation $1 $1)),
      $1, trdo3 $2)}
| KWvoid unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Void (default_annotation $1 $1)),
      $1, trdo3 $2)}
| KWtypeof unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Typeof (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lincr unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Incr_prefix (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Ldecr unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Decr_prefix (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lplus unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Positive (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lminus unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Negative (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Ltilde unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Tilde (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lbang unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Bang (default_annotation $1 $1)),
      $1, trdo3 $2)}
;

unary_init_expression :
  postfix_init_expression   {$1}
| KWdelete unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Delete (default_annotation $1 $1)),
      $1, trdo3 $2)}
| KWvoid unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Void (default_annotation $1 $1)),
      $1, trdo3 $2)}
| KWtypeof unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Typeof (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lincr unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Incr_prefix (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Ldecr unary_expression
    {(Unop ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
	    Decr_prefix (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lplus unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Positive (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lminus unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Negative (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Ltilde unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Tilde (default_annotation $1 $1)),
      $1, trdo3 $2)}
| Lbang unary_expression
    {(Unop_without_sideeffect ((default_annotation $1 (trdo3 $2)),(fsto3 $2),
			       Bang (default_annotation $1 $1) ),
      $1, trdo3 $2)}
;

multiplicative_expression :
  unary_expression                                       {$1}
| multiplicative_expression Lstar unary_expression       {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Star (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| multiplicative_expression Lslash unary_expression
      {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Slash (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
| multiplicative_expression Lrem unary_expression        {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Rem (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

multiplicative_init_expression :
  unary_init_expression
    {$1}
| multiplicative_init_expression Lstar unary_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Star (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| multiplicative_init_expression Lslash unary_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Slash (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| multiplicative_init_expression Lrem unary_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Rem (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

additive_expression :
  multiplicative_expression                            {$1}
| additive_expression Lplus multiplicative_expression  {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Plus (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| additive_expression Lminus multiplicative_expression {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Minus (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

additive_init_expression :
  multiplicative_init_expression
    {$1}
| additive_init_expression Lplus multiplicative_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Plus (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| additive_init_expression Lminus multiplicative_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Minus (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

shift_expression :
  additive_expression
    {$1}
| shift_expression Llshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Lshift (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| shift_expression Lrsignedshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Rsignedshift (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
| shift_expression Lrunsignedshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Runsignedshift (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

shift_init_expression :
  additive_init_expression
    {$1}
| shift_init_expression Llshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Lshift (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
| shift_init_expression Lrsignedshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Rsignedshift (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| shift_init_expression Lrunsignedshift additive_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Runsignedshift (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

relational_expression :
  shift_expression
    {$1}
| relational_expression Lless shift_expression
    { (Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Less (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression Lgreater shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Greater (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression Lle shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Le (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression Lge shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ge (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression KWinstanceof shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Instanceof (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression KWin shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), In (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

relational_expression_no_in :
  shift_expression
    {$1}
| relational_expression_no_in Lless shift_expression
    { (Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Less (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression_no_in Lgreater shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Greater (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression_no_in Lle shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Le (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression_no_in Lge shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ge (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_expression_no_in KWinstanceof shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Instanceof (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
/* No KWin - rule! */
;

relational_init_expression :
  shift_init_expression
    {$1}
| relational_init_expression Lless shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Less (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_init_expression Lgreater shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Greater (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_init_expression Lle shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Le (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_init_expression Lge shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ge (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_init_expression KWinstanceof shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Instanceof (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| relational_init_expression KWin shift_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), In (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

equality_expression :
  relational_expression                                   {$1}
| equality_expression Leq relational_expression
{(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression Lne relational_expression
{(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ne (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression Leqq relational_expression
{(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eqq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression Lneq relational_expression
{(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Neq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

equality_expression_no_in :
  relational_expression_no_in
    {$1}
| equality_expression_no_in Leq relational_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression_no_in Lne relational_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ne (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression_no_in Leqq relational_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eqq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_expression_no_in Lneq relational_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Neq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

equality_init_expression :
  relational_init_expression                            {$1}
| equality_init_expression Leq relational_expression    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_init_expression Lne relational_expression    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Ne (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_init_expression Leqq relational_expression   {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Eqq (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
| equality_init_expression Lneq relational_expression   {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Neq (default_annotation $2 $2) , (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_and_expression :
  equality_expression                                 {$1}
| bitwise_and_expression Lbit_and equality_expression {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_and_expression_no_in :
  equality_expression_no_in  {$1}
| bitwise_and_expression_no_in Lbit_and equality_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_and_init_expression :
  equality_init_expression  {$1}
| bitwise_and_init_expression Lbit_and equality_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_xor_expression :
  bitwise_and_expression                             {$1}
| bitwise_xor_expression Lxor bitwise_and_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Xor (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_xor_expression_no_in :
  bitwise_and_expression_no_in
    {$1}
| bitwise_xor_expression_no_in Lxor bitwise_and_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Xor (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_xor_init_expression :
  bitwise_and_init_expression
    {$1}
| bitwise_xor_init_expression Lxor bitwise_and_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Xor (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_or_expression :
  bitwise_xor_expression
    {$1}
| bitwise_or_expression Lbit_or bitwise_xor_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_or_expression_no_in :
  bitwise_xor_expression_no_in
    {$1}
| bitwise_or_expression_no_in Lbit_or bitwise_xor_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

bitwise_or_init_expression :
  bitwise_xor_init_expression
    {$1}
| bitwise_or_init_expression Lbit_or bitwise_xor_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Bit_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_and_expression :
  bitwise_or_expression
    {$1}
| logical_and_expression Lsc_and bitwise_or_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_and_expression_no_in :
  bitwise_or_expression_no_in
    {$1}
| logical_and_expression_no_in Lsc_and bitwise_or_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_and_init_expression :
  bitwise_or_init_expression
    {$1}
| logical_and_init_expression Lsc_and bitwise_or_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_and (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_or_expression :
  logical_and_expression
    {$1}
| logical_or_expression Lsc_or logical_and_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_or_expression_no_in :
  logical_and_expression_no_in
    {$1}
| logical_or_expression_no_in Lsc_or logical_and_expression_no_in
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

logical_or_init_expression :
  logical_and_init_expression
    {$1}
| logical_or_init_expression Lsc_or logical_and_expression
    {(Binop ((default_annotation (sndo3 $1) (trdo3 $3)),(fsto3 $1), Sc_or (default_annotation $2 $2), (fsto3 $3)), sndo3 $1, trdo3 $3)}
;

conditional_expression :
  logical_or_expression
    {$1}
| logical_or_expression Lhook assignment_expression Lcolon
    assignment_expression
    {(Conditional ((default_annotation (sndo3 $1) (trdo3 $5)),
		   (fsto3 $1), (fsto3 $3), (fsto3 $5)),
      sndo3 $1, trdo3 $5)}
;

conditional_expression_no_in :
  logical_or_expression_no_in
    {$1}
| logical_or_expression_no_in Lhook assignment_expression_no_in Lcolon
    assignment_expression_no_in
    {(Conditional ((default_annotation (sndo3 $1) (trdo3 $5)),
		   (fsto3 $1), (fsto3 $3), (fsto3 $5)),
      sndo3 $1, trdo3 $5)}
;

conditional_init_expression :
  logical_or_init_expression
    {$1}
| logical_or_init_expression Lhook assignment_expression Lcolon
    assignment_expression
    {(Conditional ((default_annotation (sndo3 $1) (trdo3 $5)),
		   (fsto3 $1), (fsto3 $3), (fsto3 $5)),
      sndo3 $1, trdo3 $5)};

/* Assignment Expressions */

assignment_expression :
  conditional_expression
    {$1}
| left_hand_side_expression assignment_operator assignment_expression
    {(Assign ((default_annotation (sndo3 $1) (trdo3 $3)),
	      (fsto3 $1), (fsto3 $2), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
;

assignment_expression_no_in :
  conditional_expression_no_in
    {$1}
| left_hand_side_expression assignment_operator assignment_expression_no_in
    {(Assign ((default_annotation (sndo3 $1) (trdo3 $3)),
	      (fsto3 $1), (fsto3 $2), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
;

assignment_init_expression :
  conditional_init_expression
    {$1}
| left_hand_side_init_expression assignment_operator assignment_expression
    {(Assign ((default_annotation (sndo3 $1) (trdo3 $3)),
	      (fsto3 $1), (fsto3 $2), (fsto3 $3)),
      sndo3 $1, trdo3 $3)}
;

assignment_operator :
  Lassign               {(Regular_assign (default_annotation $1 $1), $1, $1)}
| Lstarassign           {(Star_assign (default_annotation $1 $1), $1, $1)}
| Lslashassign          {(Slash_assign (default_annotation $1 $1), $1, $1)}
| Lremassign            {(Rem_assign (default_annotation $1 $1), $1, $1)}
| Lplusassign           {(Plus_assign (default_annotation $1 $1), $1, $1)}
| Lminusassign          {(Minus_assign (default_annotation $1 $1), $1, $1)}
| Llshiftassign         {(Lshift_assign (default_annotation $1 $1), $1, $1)}
| Lrsignedshiftassign   {(Rsignedshift_assign (default_annotation $1 $1), $1, $1)}
| Lrunsignedshiftassign {(Runsignedshift_assign (default_annotation $1 $1), $1, $1)}
| Landassign            {(And_assign (default_annotation $1 $1), $1, $1)}
| Lxorassign            {(Xor_assign (default_annotation $1 $1), $1, $1)}
| Lorassign             {(Or_assign (default_annotation $1 $1), $1, $1)}
;
%%
