(* Original file: psmt2-frontend.0.1/psmt2-frontend-0.1/src/smtlib_parser.mly *)
/******************************************************************************/
/*                                                                            */
/* An SMT-LIB 2 for the Alt-Ergo Theorem Prover                               */
/*                                                                            */
/******************************************************************************/
%{
   open Smtlib_syntax

   let mk_data p c =
       {p;c;ty= Smtlib_ty.new_type Smtlib_ty.TDummy;is_quantif=false}
%}

%start commands
%start term
%start term_list

%token EOF AS EXISTS FORALL  LET LP POP PUSH ECHO RP UNDERSCORE PAR  PATTERN
MATCH EXCLIMATIONPT

%token ASSERT CHECKSAT EXIT RESET RESETASSERTIONS CHECKSATASSUMING
 CHECKENTAILMENT
DECLAREFUN DECLARESORT DECLARECONST  DECLAREDATATYPES DECLAREDATATYPE
DEFINEFUN DEFINEFUNREC DEFINEFUNSREC DEFINESORT
GETASSERT GETASSIGN GETINFO GETOPTION GETPROOF GETUNSATCORE
GETVALUE GETMODEL GETUNSATASSUMPTIONS
SETINFO SETLOGIC SETOPTION

%token ALLSTATS AUTHORS AUTHOR AXIOMS CATEGORY DEFINITIO DIFFICULTY INSTANCE
DIAGNOOUTPUTCHAN ERRORBEHAV EXTENSIONS FUNS FUNSDESCRIPT GLOBALDECLARATIONS
INTERACTIVE LANGUAGE LICENSE NAME NAMED NOTES SERIES
PRODUCEASSERTIONS PRINTSUCCESS PRODUCEUNSATASSUMPTIONS PRODUCEASSIGNEMENT
PRODUCEMODELS PRODUCEPROOFS PRODUCEUNSATCORES
RANDOMSEED REASONUNKNOWN REGULAROUTPUTCHAN
SOURCE SMTLIBVERSION SORTS SORTSDESCRIPTION STATUTS
THEORIES VALUES VERBOSITY VERSION RESSOURCELIMIT ASSERTIONSTACKLVL

%token <string> ASCIIWOR BINARY DECIMAL HEXADECIMAL NUMERAL STRINGLIT
SYMBOL

%type <Smtlib_syntax.commands> commands
%type <Smtlib_syntax.command> command
%type <Smtlib_syntax.term> term
%type <Smtlib_syntax.term list * bool> term_list
%type <Smtlib_syntax.varbinding> varbinding
%type <Smtlib_syntax.qualidentifier> qualidentifier
%type <Smtlib_syntax.sorted_var> sorted_var
%type <Smtlib_syntax.sort> sort
%type <Smtlib_syntax.identifier> identifier
%type <Smtlib_syntax.attribute_value> attribute_value
%type <Smtlib_syntax.sexpr> sexpr
%type <Smtlib_syntax.symbol> symbol
%type <Smtlib_syntax.key_option> key_option
%type <Smtlib_syntax.key_info> key_info
%type <Smtlib_syntax.key_term> key_term
%type <Smtlib_syntax.keyword> keyword

%%
/*************************************************************************/
term_list:
 | list(term) { $1,true}

constant:
    | DECIMAL     { Const_Dec $1 }
    | NUMERAL     { Const_Num $1 }
    | STRINGLIT   { Const_Str $1 }
    | HEXADECIMAL { Const_Hex $1 }
    | BINARY      { Const_Bin $1 }

symbol:
    | SYMBOL   { mk_data ($startpos,$endpos) ($1) }
    | ASCIIWOR { mk_data ($startpos,$endpos) ($1) }

index:
    | symbol   { mk_data ($startpos,$endpos) (IndexSymbol $1) }
    | NUMERAL  { mk_data ($startpos,$endpos) (IndexNumeral $1) }

identifier:
    | symbol { mk_data ($startpos,$endpos) (IdSymbol $1) }
    | LP UNDERSCORE symbol nonempty_list(index) RP
      { mk_data ($startpos,$endpos) (IdUnderscoreSymNum($3, $4)) }

prop_literal:
    | symbol
      { mk_data ($startpos,$endpos) (PropLit $1) }
    | LP symbol symbol RP
      { mk_data ($startpos,$endpos)
        (if $2.c <> "not" then raise Error; PropLitNot $3) }

sort:
    | identifier { mk_data ($startpos,$endpos) (SortIdentifier $1) }
    | LP identifier nonempty_list(sort) RP
      { mk_data ($startpos,$endpos) (SortIdMulti($2, $3)) }

/*************************************************************************/
attribute_value:
    | constant { mk_data ($startpos,$endpos) (AttributeValSpecConst $1) }
    | symbol { mk_data ($startpos,$endpos) (AttributeValSymbol $1) }
    | LP list(sexpr) RP { mk_data ($startpos,$endpos) (AttributeValSexpr $2) }

attribute:
    | key_info { mk_data ($startpos,$endpos) (AttributeKey $1) }
    | key_info attribute_value
        { mk_data ($startpos,$endpos) (AttributeKeyValue($1,$2)) }

sexpr:
    | constant { mk_data ($startpos,$endpos) (SexprSpecConst $1) }
    | symbol { mk_data ($startpos,$endpos) (SexprSymbol $1) }
    | keyword { mk_data ($startpos,$endpos) (SexprKeyword $1) }
    | LP list(sexpr) RP { mk_data ($startpos,$endpos) (SexprInParen $2) }

/*************************************************************************/
varbinding:
    | LP symbol term RP { $2,$3 }

sorted_var:
    | LP symbol sort RP { $2,$3 }

qualidentifier:
    | identifier { mk_data ($startpos,$endpos) (QualIdentifierId $1) }
    | LP AS identifier sort RP
      { mk_data ($startpos,$endpos) (QualIdentifierAs($3, $4)) }

pattern:
    | symbol { $1,[] }
    | LP symbol nonempty_list(symbol) RP
       { $2, $3 }

match_case:
    | LP pattern term RP { ($2,$3) }

term:
    | constant { mk_data ($startpos,$endpos) (TermSpecConst $1) }
    | qualidentifier { mk_data ($startpos,$endpos) (TermQualIdentifier $1) }
    | LP qualidentifier nonempty_list(term) RP
       { mk_data ($startpos,$endpos) (TermQualIdTerm ($2, $3)) }
    | LP LET LP nonempty_list(varbinding) RP term RP
       { mk_data ($startpos,$endpos) (TermLetTerm ($4, $6)) }
    | LP LET LP RP term RP
       { $5 }
    | LP FORALL LP nonempty_list(sorted_var) RP term RP
       { mk_data ($startpos,$endpos) (TermForAllTerm ($4, $6)) }
    | LP EXISTS LP nonempty_list(sorted_var) RP term RP
       { mk_data ($startpos,$endpos) (TermExistsTerm ($4, $6)) }
    | LP MATCH term LP nonempty_list(match_case) RP RP
       { mk_data ($startpos,$endpos) (TermMatch ($3, $5)) }
    | LP EXCLIMATIONPT term list(key_term) RP
       { mk_data ($startpos,$endpos) (TermExclimationPt ($3, $4)) }

/** keyword *******************************************/
keyword :
    | CATEGORY {mk_data ($startpos,$endpos) Category }
    | SMTLIBVERSION {mk_data ($startpos,$endpos) Smtlibversion }
    | SOURCE {mk_data ($startpos,$endpos) Source }
    | STATUTS symbol
        {Options.set_status $2.c;mk_data ($startpos,$endpos) (Statuts $2) }
    | LICENSE {mk_data ($startpos,$endpos) License }
    | NOTES {mk_data ($startpos,$endpos) Notes }
    | AXIOMS {mk_data ($startpos,$endpos) Axioms }
    | DEFINITIO {mk_data ($startpos,$endpos) Definitio }
    | EXTENSIONS {mk_data ($startpos,$endpos) Extensions }
    | FUNS {mk_data ($startpos,$endpos) Funs }
    | FUNSDESCRIPT {mk_data ($startpos,$endpos) FunsDescript  }
    | LANGUAGE {mk_data ($startpos,$endpos) Language }
    | SORTS {mk_data ($startpos,$endpos) Sorts }
    | SORTSDESCRIPTION {mk_data ($startpos,$endpos) SortsDescr }
    | THEORIES {mk_data ($startpos,$endpos) Theories }
    | VALUES {mk_data ($startpos,$endpos) Values }

key_option:
    | DIAGNOOUTPUTCHAN {mk_data ($startpos,$endpos) Diagnooutputchan }
    | GLOBALDECLARATIONS {mk_data ($startpos,$endpos) Globaldeclarations }
    | INTERACTIVE {mk_data ($startpos,$endpos) Interactive }
    | PRINTSUCCESS {mk_data ($startpos,$endpos) Printsucces }
    | PRODUCEASSERTIONS {mk_data ($startpos,$endpos) Produceassertions }
    | PRODUCEASSIGNEMENT {mk_data ($startpos,$endpos) Produceassignement }
    | PRODUCEMODELS {mk_data ($startpos,$endpos) Producemodels }
    | PRODUCEPROOFS {mk_data ($startpos,$endpos) Produceproofs }
    | PRODUCEUNSATASSUMPTIONS
      {mk_data ($startpos,$endpos) Produceunsatassumptions }
    | PRODUCEUNSATCORES {mk_data ($startpos,$endpos) Produceunsatcores }
    | RANDOMSEED {mk_data ($startpos,$endpos) Randomseed }
    | REGULAROUTPUTCHAN {mk_data ($startpos,$endpos) Regularoutputchan }
    | VERBOSITY {mk_data ($startpos,$endpos) Verbosity }
    | RESSOURCELIMIT {mk_data ($startpos,$endpos) Ressourcelimit }

option:
    | key_option index {mk_data ($startpos,$endpos) (Option_key ($1,$2)) }
    | attribute {mk_data ($startpos,$endpos) (Option_attribute $1) }

key_info:
    | ALLSTATS {mk_data ($startpos,$endpos) Allstats }
    | ASSERTIONSTACKLVL {mk_data ($startpos,$endpos) Assertionstacklvl }
    | AUTHORS {mk_data ($startpos,$endpos) Authors }
    | AUTHOR {mk_data ($startpos,$endpos) Authors }
    | DIFFICULTY {mk_data ($startpos,$endpos) Difficulty }
    | ERRORBEHAV {mk_data ($startpos,$endpos) Errorbehav }
    | INSTANCE {mk_data ($startpos,$endpos) Instance }
    | NAME {mk_data ($startpos,$endpos) Name }
    | REASONUNKNOWN {mk_data ($startpos,$endpos) Reasonunknown }
    | SERIES {mk_data ($startpos,$endpos) Series }
    | VERSION {mk_data ($startpos,$endpos) Version }
    | keyword {mk_data ($startpos,$endpos) (Key_info $1) }

key_term:
    | PATTERN LP nonempty_list(term) RP
      { mk_data ($startpos,$endpos) (Pattern $3) }
    | NAMED symbol { mk_data ($startpos,$endpos) (Named $2) }

/*** Datatypes ************************************************************/
selector_dec:
    | LP symbol sort RP { ($2,$3) }

constructor_dec:
    | LP symbol list(selector_dec) RP { $2,$3 }

datatype_dec:
    | LP nonempty_list(constructor_dec) RP
        { [],$2 }
    | LP PAR LP nonempty_list(symbol) RP LP nonempty_list(constructor_dec) RP RP
        { $4,$7 }

sort_dec:
    | LP symbol NUMERAL RP { ($2,$3) }

/*** Functions *************************************************************/
const_dec:
    | sort
        { [],$1 }
    | LP PAR LP nonempty_list(symbol) RP sort RP
        { $4,$6 }

fun_dec:
    | LP list(sort) RP sort
        { [],$2,$4 }
    | LP PAR LP nonempty_list(symbol) RP LP list(sort) RP sort RP
        { ($4,$7,$9) }

fun_def:
    | symbol LP list(sorted_var) RP sort
        { $1,[],$3,$5 }
    | symbol LP PAR LP nonempty_list(symbol) RP LP list(sorted_var) RP sort RP
        { $1,$5,$8,$10 }

fun_defs:
    | LP fun_def RP { $2 }

/*** Asserts ***************************************************************/
assert_dec:
    | term
        { [],$1 }
    | LP PAR LP nonempty_list(symbol) RP term RP
        { $4,$6 }

/*** Commands **************************************************************/
command:
    | LP ASSERT assert_dec RP
        {mk_data ($startpos,$endpos) (Cmd_Assert ($3)) }
    | LP CHECKSAT RP
        {mk_data ($startpos,$endpos) (Cmd_CheckSat) }
    | LP CHECKSATASSUMING LP list(prop_literal) RP RP
        {mk_data ($startpos,$endpos) (Cmd_CheckSatAssum $4) }
    | LP CHECKENTAILMENT assert_dec RP
        {mk_data ($startpos,$endpos) (Cmd_CheckEntailment $3) }
    | LP DECLARECONST symbol const_dec RP
        {mk_data ($startpos,$endpos) (Cmd_DeclareConst ($3,$4)) }
    | LP DECLAREDATATYPE symbol datatype_dec RP
        { mk_data ($startpos,$endpos) (Cmd_DeclareDataType ($3,$4)) }
    | LP DECLAREDATATYPES LP nonempty_list(sort_dec) RP LP
       nonempty_list(datatype_dec) RP RP
        { mk_data ($startpos,$endpos) (Cmd_DeclareDataTypes ($4,$7)) }
    | LP DECLAREFUN symbol fun_dec RP
        {mk_data ($startpos,$endpos) (Cmd_DeclareFun($3, $4)) }
    | LP DECLARESORT symbol NUMERAL RP
        {mk_data ($startpos,$endpos) (Cmd_DeclareSort ($3, $4)) }
    | LP DEFINEFUN fun_def term RP
        {mk_data ($startpos,$endpos) (Cmd_DefineFun ($3,$4)) }
    | LP DEFINEFUNREC fun_def term RP
        {mk_data ($startpos,$endpos) (Cmd_DefineFunRec ($3,$4)) }
    | LP DEFINEFUNSREC LP list(fun_defs) RP LP nonempty_list(term) RP RP
        {mk_data ($startpos,$endpos) (Cmd_DefineFunsRec ($4,$7)) }
    | LP DEFINESORT symbol LP list(symbol) RP sort RP
        {mk_data ($startpos,$endpos) (Cmd_DefineSort($3, $5, $7)) }
    | LP ECHO symbol RP
        {mk_data ($startpos,$endpos) (Cmd_Echo $3) }
    | LP EXIT RP
        {mk_data ($startpos,$endpos) (Cmd_Exit) }
    | LP GETASSERT RP
        {mk_data ($startpos,$endpos) (Cmd_GetAssert) }
    | LP GETASSIGN RP
        {mk_data ($startpos,$endpos) (Cmd_GetAssign) }
    | LP GETINFO key_info RP
        {mk_data ($startpos,$endpos) (Cmd_GetInfo $3) }
    | LP GETMODEL RP
        {mk_data ($startpos,$endpos) (Cmd_GetModel) }
    | LP GETOPTION keyword RP
        {mk_data ($startpos,$endpos) (Cmd_GetOption $3) }
    | LP GETPROOF RP
        {mk_data ($startpos,$endpos) (Cmd_GetProof) }
    | LP GETUNSATASSUMPTIONS RP
        {mk_data ($startpos,$endpos) (Cmd_GetUnsatAssumptions) }
    | LP GETUNSATCORE RP
        {mk_data ($startpos,$endpos) (Cmd_GetUnsatCore) }
    | LP GETVALUE LP nonempty_list(term) RP RP
        {mk_data ($startpos,$endpos) (Cmd_GetValue $4) }
    | LP PUSH NUMERAL RP
        {mk_data ($startpos,$endpos) (Cmd_Push $3) }
    | LP POP NUMERAL RP
        {mk_data ($startpos,$endpos) (Cmd_Pop $3) }
    | LP RESET RP
        {mk_data ($startpos,$endpos) (Cmd_Reset) }
    | LP RESETASSERTIONS RP
        {mk_data ($startpos,$endpos) (Cmd_ResetAssert) }
    | LP SETINFO attribute RP
        {mk_data ($startpos,$endpos) (Cmd_SetInfo $3) }
    | LP SETLOGIC symbol RP
        {Smtlib_error.set_logic true;
         mk_data ($startpos,$endpos) (Cmd_SetLogic $3) }
    | LP SETOPTION option RP
        {mk_data ($startpos,$endpos) (Cmd_SetOption $3) }

commands:
    | EOF              { [] }
    | command commands { $1::$2 }

%%
