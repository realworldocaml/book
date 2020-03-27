/*
 * Copyright (C) 2008-2013 Stanislaw Findeisen <stf@eisenbits.com>
 *
 * This file is part of phphard.
 *
 * phphard is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * phphard is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with phphard.  If not, see <http://www.gnu.org/licenses/>.
 */

%{
open PHPSyntaxTree;;

let makeTokenData () =
    let pos = symbol_start_pos ()
    in
        (TokenData(pos.Lexing.pos_fname, pos.Lexing.pos_lnum, (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)))
;;
%}

%token <int>    T_INT_LITERAL
%token <float>  T_FLOAT_LITERAL
%token <string> T_STRING_LITERAL_S
%token <string> T_STRING_LITERAL_D
%token <string> T_IDENTIFIER

%token T_PHPSL T_PHPE
%token T_ARRAY T_BOOL T_FLOAT T_INT T_OBJECT T_STRING
%token T_ABSTRACT T_BREAK T_CASE T_CATCH T_CLASS T_CONST T_CONTINUE T_DEFAULT T_ELSE T_EXTENDS T_FUNCTION T_IF T_NULL T_PARENT T_PRIVATE T_PROTECTED T_PUBLIC T_RETURN T_SELF T_STATIC T_SWITCH T_THIS T_THROW T_TRY
%token T_COLON T_DOLLAR T_QUESTION T_SEMICOLON
%token T_LPAREN T_RPAREN T_LBRACS T_RBRACS T_LBRACC T_RBRACC

/*******************************************
 * Operators must be defined both in operator
 * precedence section and here.
 *******************************************/

%token T_COMMA T_OR T_XOR T_AND

/* = += -= *= /= .= %= &= |= ^= <<= >>= */
%token T_EQ T_PLUS_EQ T_MINUS_EQ T_ASTERISK_EQ T_SLASH_EQ T_DOT_EQ T_PERCENT_EQ T_AMPERSAND_EQ T_BAR_EQ T_CARET_EQ T_LT_LT_EQ T_GT_GT_EQ

%token T_BAR_2 T_AMPERSAND_2 T_BAR T_CARET T_AMPERSAND
%token T_EQ_EQ T_EXCL_EQ T_EQ_EQ_EQ T_EXCL_EQ_EQ         /* == != === !== */
%token T_LT T_LT_EQ T_GT T_GT_EQ T_LT_GT                 /* < <= > >= <>  */
%token T_LT_LT T_GT_GT                                   /* << >>         */
%token T_PLUS T_MINUS T_DOT                              /* + - .         */
%token T_ASTERISK T_SLASH T_PERCENT                      /* * / %         */
%token T_EXCLAMATION
%token T_INSTANCEOF
%token T_TILDE T_UMINUS T_UPLUS                          /* ~  -x  +x     */
%token T_PLUS_2 T_MINUS_2                                /* ++ --         */
%token T_COLON_2 T_RARROW                                /* :: -> (not mentioned on PHP site) */
%token T_NEW                                             /* new           */

/*****************************************************************************************
 * Operator precedence and associativity: the later, the higher precedence.
 * This is based on: http://www.php.net/manual/en/language.operators.precedence.php .
 * [ ] is not considered an operator in this declaration section.
 * ? : is not considered an operator in this declaration section.
 *****************************************************************************************/

%left T_COMMA
%left T_OR
%left T_XOR
%left T_AND

/* = += -= *= /= .= %= &= |= ^= <<= >>= */
%right T_EQ T_PLUS_EQ T_MINUS_EQ T_ASTERISK_EQ T_SLASH_EQ T_DOT_EQ T_PERCENT_EQ T_AMPERSAND_EQ T_BAR_EQ T_CARET_EQ T_LT_LT_EQ T_GT_GT_EQ

%left T_BAR_2
%left T_AMPERSAND_2
%left T_BAR
%left T_CARET
%left T_AMPERSAND
%nonassoc T_EQ_EQ T_EXCL_EQ T_EQ_EQ_EQ T_EXCL_EQ_EQ         /* == != === !== */
%nonassoc T_LT T_LT_EQ T_GT T_GT_EQ T_LT_GT                 /* < <= > >= <>  */
%left T_LT_LT T_GT_GT                                       /* << >>         */
%left T_PLUS T_MINUS T_DOT                                  /* + - .         */
%left T_ASTERISK T_SLASH T_PERCENT                          /* * / %         */
%right T_EXCLAMATION                                        /* !             */
%nonassoc T_INSTANCEOF                                      /* instanceof    */
%nonassoc T_TILDE T_UMINUS T_UPLUS                          /* ~  -x  +x     */
%nonassoc T_PLUS_2 T_MINUS_2                                /* ++ --         */
%left T_COLON_2 T_RARROW                                    /* :: -> (not mentioned on PHP site) */
%left T_NEW                                                 /* new           */

%start single_php_source_file              /* the entry point */
%type <PHPSyntaxTree.sourceFile> single_php_source_file

%%

constant_literal:
    T_NULL              {     Null(makeTokenData()) }
  | T_INT_LITERAL       {      Int(makeTokenData(),$1) }
  | T_FLOAT_LITERAL     {    Float(makeTokenData(),$1) }
  | T_STRING_LITERAL_S  { StringSQ(makeTokenData(),$1) }
;
literal:
    constant_literal    { ConstLiteral($1) }
  | T_STRING_LITERAL_D  { StringDQ(makeTokenData(),$1) }            /* This is NOT a constant expression. */
;

/* TODO array, object */
predefined_type:
    T_BOOL              { TypeBool  (makeTokenData()) }
  | T_FLOAT             { TypeFloat (makeTokenData()) }
  | T_INT               { TypeInt   (makeTokenData()) }
  | T_STRING            { TypeString(makeTokenData()) }
;

identifier:
    T_IDENTIFIER           { Identifier(makeTokenData(),$1) }
;
variable:
    T_DOLLAR T_IDENTIFIER  { Variable(makeTokenData(),$2) }
;
identifier_variable:
    identifier       { IdentVar_ident($1) }
  | variable         { IdentVar_var  ($1) }
;

/*******************************************************************
 * Expressions. Operator precedence is based on this:
 * http://www.php.net/manual/en/language.operators.precedence.php
 *******************************************************************/

atomic_expression_noiv:
    literal                                 { $1 }
  | T_LPAREN expression T_RPAREN            { $2 }
;

new_expression_noiv:
    atomic_expression_noiv                               { $1 }
  | T_NEW identifier T_LPAREN expression_list T_RPAREN   { New($2, $4) }      /* new A; is not supported */
;

rarrow_chain:
                                                 { [] }
  | rarrow_chain T_RARROW identifier_variable    { $3::$1 }         /* TODO: list order */
;
nonempty_rarrow_chain:
    rarrow_chain T_RARROW identifier_variable    { $3::$1 }         /* TODO: list order */
;
object_member_expression_noiv:
    new_expression_noiv rarrow_chain  { Dereference($1,                             $2) }
  | T_DOLLAR T_THIS     rarrow_chain  { Dereference(VarExpr(This(makeTokenData())), $3) }
  | variable   nonempty_rarrow_chain  { Dereference(VarExpr($1),                    $2) }
;

static_reference_chain_noiv:
    object_member_expression_noiv                                 { $1 }
  | T_PARENT     T_COLON_2 identifier_variable rarrow_chain       { Parent($3::$4) }                   /* TODO: list order */
  | T_SELF       T_COLON_2 identifier_variable rarrow_chain       {   Self($3::$4) }                   /* TODO: list order */
  | identifier   T_COLON_2 identifier_variable rarrow_chain       { StaticReferenceChain($1, $3::$4) } /* TODO: list order */
;

array_expression_noiv:
    static_reference_chain_noiv                       { $1     }
  | array_expression T_LBRACS expression T_RBRACS     { ArrayExpr($1,$3) }
;
array_expression:
    array_expression_noiv           { $1 }
  | identifier                      { IdentExpr($1) }
  | variable                        {   VarExpr($1) }
;

/* A value can be assigned to this - used below in assignment statements. */
lvalue_novariable:
    array_expression_noiv    { $1 }
  | identifier               { IdentExpr($1) }
;

function_call_expression_basic:                 /* basic version to be used in function call statement. */
    array_expression T_LPAREN expression_list T_RPAREN   { FunCallExpr($1, $3)  }
;
function_call_expression:                       /* cumulative version to be used below. */
    array_expression                 { $1 }
  | function_call_expression_basic   { $1 }
;

crement_expression:
    function_call_expression                          { $1 }
  | function_call_expression T_MINUS_2                { PostDecrement($1) }
  | function_call_expression T_PLUS_2                 { PostIncrement($1) }
  | T_MINUS_2 function_call_expression                {  PreDecrement($2) }
  | T_PLUS_2  function_call_expression                {  PreIncrement($2) }
;

unary_expression:
            crement_expression                           { $1 }
  | T_PLUS  crement_expression %prec T_UPLUS             { $2 }
  | T_MINUS crement_expression %prec T_UMINUS            { UnaryMinus($2) }
  | T_TILDE crement_expression                           { BitwiseNot($2) }
  | T_LPAREN predefined_type T_RPAREN crement_expression { TypeCast($2,$4) }
;

instanceof_expression:
    unary_expression                            { $1 }
  | unary_expression T_INSTANCEOF identifier    { InstanceOf($1,$3) }
;

logical_not_expression:
                  instanceof_expression               { $1 }
  | T_EXCLAMATION logical_not_expression              { LogicalNot($2) }
;

arithmetic_expression:
    logical_not_expression                                      { $1 }
  | arithmetic_expression T_ASTERISK logical_not_expression     { Multiplication($1,$3) }
  | arithmetic_expression T_SLASH    logical_not_expression     { Division($1,$3) }
  | arithmetic_expression T_PERCENT  logical_not_expression     { Modulo($1,$3) }
;

arithmetic_string_expression:
    arithmetic_expression                                       { $1 }
  | arithmetic_string_expression T_PLUS  arithmetic_expression  {   Plus($1,$3) }
  | arithmetic_string_expression T_MINUS arithmetic_expression  {  Minus($1,$3) }
  | arithmetic_string_expression T_DOT   arithmetic_expression  { Concat($1,$3) }
;

shift_expression:
    arithmetic_string_expression                                { $1 }
  | shift_expression T_LT_LT arithmetic_string_expression       { ShiftLeft($1,$3) }
  | shift_expression T_GT_GT arithmetic_string_expression       { ShiftRight($1,$3) }
;

comparison_expression:
    shift_expression                                            { $1 }
  | shift_expression T_LT    shift_expression                   { IsSmaller($1,$3) }
  | shift_expression T_LT_EQ shift_expression                   { IsSmallerEq($1,$3) }
  | shift_expression T_GT    shift_expression                   { IsSmaller($3,$1) }
  | shift_expression T_GT_EQ shift_expression                   { IsSmallerEq($3,$1) }
  | shift_expression T_LT_GT shift_expression                   { LogicalNot(IsEqual($1,$3)) }
;

comparison_expression_weak:
    comparison_expression                                       { $1 }
  | comparison_expression T_EQ_EQ      comparison_expression    {            IsEqual($1,$3) }
  | comparison_expression T_EXCL_EQ    comparison_expression    { LogicalNot(IsEqual($1,$3)) }
  | comparison_expression T_EQ_EQ_EQ   comparison_expression    {            IsIdentical($1,$3) }
  | comparison_expression T_EXCL_EQ_EQ comparison_expression    { LogicalNot(IsIdentical($1,$3)) }
;

bitwise_and_expression:
    comparison_expression_weak                                      { $1 }
  | bitwise_and_expression T_AMPERSAND comparison_expression_weak   { BitwiseAnd($1,$3) }
;
bitwise_xor_expression:
    bitwise_and_expression                                          { $1 }
  | bitwise_xor_expression T_CARET bitwise_and_expression           { BitwiseXor($1,$3) }
;
bitwise_or_expression:
    bitwise_xor_expression                                          { $1 }
  | bitwise_or_expression T_BAR  bitwise_xor_expression             { BitwiseOr($1,$3) }
;

logical_and_expression_strong:
    bitwise_or_expression                                               { $1 }
  | logical_and_expression_strong T_AMPERSAND_2 bitwise_or_expression   { LogicalAnd($1,$3) }
;
logical_or_expression_strong:
    logical_and_expression_strong                                       { $1 }
  | logical_or_expression_strong T_BAR_2 logical_and_expression_strong  { LogicalOr($1,$3) }
;

ternary_choice_expression:
    logical_or_expression_strong                                        { $1 }
    /* PHP allows multiple ?: without brackets, so this is a standard violation: */
  | logical_or_expression_strong T_QUESTION logical_or_expression_strong T_COLON logical_or_expression_strong   { TernaryChoice($1,$3,$5) }
;

assignment_expression:
    ternary_choice_expression                                        { $1 }
  | ternary_choice_expression T_EQ           assignment_expression   { AssignExpr($1,                   $3) }
  | ternary_choice_expression T_PLUS_EQ      assignment_expression   { AssignExpr($1,           Plus($1,$3)) }
  | ternary_choice_expression T_MINUS_EQ     assignment_expression   { AssignExpr($1,          Minus($1,$3)) }
  | ternary_choice_expression T_ASTERISK_EQ  assignment_expression   { AssignExpr($1, Multiplication($1,$3)) }
  | ternary_choice_expression T_SLASH_EQ     assignment_expression   { AssignExpr($1,       Division($1,$3)) }
  | ternary_choice_expression T_DOT_EQ       assignment_expression   { AssignExpr($1,         Concat($1,$3)) }
  | ternary_choice_expression T_PERCENT_EQ   assignment_expression   { AssignExpr($1,         Modulo($1,$3)) }
  | ternary_choice_expression T_AMPERSAND_EQ assignment_expression   { AssignExpr($1,     BitwiseAnd($1,$3)) }
  | ternary_choice_expression T_BAR_EQ       assignment_expression   { AssignExpr($1,      BitwiseOr($1,$3)) }
  | ternary_choice_expression T_CARET_EQ     assignment_expression   { AssignExpr($1,     BitwiseXor($1,$3)) }
  | ternary_choice_expression T_LT_LT_EQ     assignment_expression   { AssignExpr($1,      ShiftLeft($1,$3)) }
  | ternary_choice_expression T_GT_GT_EQ     assignment_expression   { AssignExpr($1,     ShiftRight($1,$3)) }
;

logical_and_expression_weak:
    assignment_expression                                         { $1 }
  | logical_and_expression_weak T_AND assignment_expression       { LogicalAnd($1, $3) }
;
logical_xor_expression_weak:
    logical_and_expression_weak                                   { $1 }
  | logical_xor_expression_weak T_XOR logical_and_expression_weak { LogicalXor($1, $3) }
;
logical_or_expression_weak:
    logical_xor_expression_weak                                   { $1 }
  | logical_or_expression_weak T_OR logical_xor_expression_weak   { LogicalOr($1, $3) }
;

expression:
    logical_or_expression_weak                                    { $1 }
;

expression_list_suffx:
                                                { [] }
  | T_COMMA expression expression_list_suffx    { $2::$3 }
;
expression_list:
                                        { [] }
  | expression expression_list_suffx    { $1::$2 }
;

/***************
 * Functions
 ***************/

formal_argument:
               variable            {      FormalArg($1) }
  | identifier variable            { TypedFormalArg($1,$2) }
;

formal_argument_with_default:
    /* "The default value must be a constant expression, not (for example) a variable, a class member or a function call." */
    formal_argument T_EQ constant_literal  { FormalArgWDefault($1,$3) }
;
nonempty_formal_argument_list_with_defaults:
    formal_argument_with_default                                                     { [$1] }
  | formal_argument_with_default T_COMMA nonempty_formal_argument_list_with_defaults { $1::$3 }
;
nonempty_formal_argument_list:
    formal_argument                                                             { ([$1],[]) }
  | formal_argument T_COMMA nonempty_formal_argument_list                       { ($1::(fst $3), snd $3) }
  | nonempty_formal_argument_list_with_defaults                                 { ([], $1) }
;
formal_argument_list:
                                   { ([],[]) }
  | nonempty_formal_argument_list  {  $1 }
;

function_definition:
    T_FUNCTION identifier T_LPAREN formal_argument_list T_RPAREN block_statement   { Function($2, $4, $6) }
;

/***********
 * Classes
 ***********/

class_item_visibility:
                                                { Public }
  | T_PUBLIC                                    { Public }
  | T_PROTECTED                                 { Protected }
  | T_PRIVATE                                   { Private }
;

method_definition:
    class_item_visibility T_STATIC function_definition      {   StaticMethod($1,$3) }
  | class_item_visibility          function_definition      { InstanceMethod($1,$2) }
;
field_definition:
    class_item_visibility T_STATIC variable_declaration                   { StaticVar  ($1,$3) }
  | class_item_visibility T_CONST identifier T_EQ expression T_SEMICOLON  { StaticConst($1,$3,$5) }
;

class_item_list:
                                           { [] }
  | method_definition class_item_list      { $1::$2 }
  |  field_definition class_item_list      { $1::$2 }
;

abstract_clause:
                   { Concrete }
  | T_ABSTRACT     { Abstract }
;
extends_clause:
                            { RootClass }
  | T_EXTENDS identifier    { Extends($2) }
;
class_definition:
    abstract_clause T_CLASS identifier extends_clause T_LBRACC class_item_list T_RBRACC { Class($1, $3, $4, $6) }
;

/*********************
 * Simple statements
 *********************/

assignment_statement:
    /* array_expression might be variable, so this might be a variable declaration */
    lvalue_novariable T_EQ expression T_SEMICOLON    { AssignStmt($1, $3) }
;
variable_declaration:
    variable                 T_SEMICOLON   { VarDecl     ($1)    }
  | variable T_EQ expression T_SEMICOLON   { VarDeclAssig($1,$3) }
;
function_call_statement:
    function_call_expression_basic T_SEMICOLON          { FunCallStmt(funCallExprAsPair $1) }
;
break_statement:
    T_BREAK             T_SEMICOLON    { Break(makeTokenData()) }
;
return_statement:
    T_RETURN expression T_SEMICOLON    { Return($2) }
;
throw_statement:
    T_THROW expression  T_SEMICOLON    { Throw($2) }
;

simple_statement:
    variable_declaration          { VarDeclStmt($1) }
  | assignment_statement          { $1 }
  | function_call_statement       { $1 }
  | break_statement               { $1 }
  | return_statement              { $1 }
  | throw_statement               { $1 }
;

/***********************
 * Compound statements
 ***********************/

statement_list:
                               { [] }
  | statement statement_list   { $1::$2 }
;

block_statement:
    T_LBRACC statement_list T_RBRACC    { BlockStmt($2) }
;

switch_item_sepa:
    T_COLON           {}
  | T_SEMICOLON       {}
;
switch_case:
    T_CASE expression switch_item_sepa statement_list { (SwCase($2), BlockStmt($4)) }
;
switch_default:
    T_DEFAULT switch_item_sepa statement_list         { (SwDefault,  BlockStmt($3)) }
;
switch_item_list:
                                                      { [] }            /* "default" clause is optional */
  | switch_default                                    { [$1] }
  | switch_case switch_item_list                      { $1::$2 }
;
switch_statement:
    T_SWITCH T_LPAREN expression T_RPAREN T_LBRACC switch_item_list T_RBRACC    { SwitchStmt($3, $6) }
;

catch:
    T_CATCH T_LPAREN formal_argument T_RPAREN block_statement   { ($3,$5) }
;
nonempty_catch_list:
    catch                           { [$1] }
  | catch nonempty_catch_list       { $1::$2 }
;
try_catch_statement:
    T_TRY block_statement nonempty_catch_list       { TryCatch($2,$3) }
;

if_statement_unmatched:
    T_IF T_LPAREN expression T_RPAREN statement                                                   {     If($3,$5)    }
  | T_IF T_LPAREN expression T_RPAREN statement_no_unmatched_if T_ELSE if_statement_unmatched     { IfElse($3,$5,$7) }
;
if_statement_matched:
    T_IF T_LPAREN expression T_RPAREN statement_no_unmatched_if T_ELSE statement_no_unmatched_if  { IfElse($3,$5,$7) }
;

compound_statement_no_unmatched_if:
        block_statement                 { $1 }
  |    switch_statement                 { $1 }
  |        if_statement_matched         { $1 }
  | try_catch_statement                 { $1 }
;

statement_no_unmatched_if:
      simple_statement                  { $1 }
  | compound_statement_no_unmatched_if  { $1 }
;

statement:
       statement_no_unmatched_if        { $1 }
  | if_statement_unmatched              { $1 }
;

/**************************
 * single PHP source file
 **************************/

single_php_source_file_item_list:
                                                            { [] }
  |   class_definition   single_php_source_file_item_list   { (PHPClass    ($1))::$2  }
  | function_definition  single_php_source_file_item_list   { (PHPFunction ($1))::$2  }
  | statement            single_php_source_file_item_list   { (PHPStatement($1))::$2  }
;

single_php_source_file:
    T_PHPSL single_php_source_file_item_list T_PHPE       { PHPSourceFile($2) }    /* TODO: mixing HTML/PHP */
;
