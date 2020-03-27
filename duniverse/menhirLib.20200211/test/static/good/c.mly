%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

(* fpottier: tokens introduced because menhir does not support the '.' notation *)
%token LPAREN RPAREN LBRACK RBRACK DOT COMMA AMPERSAND STAR PLUS MINUS TILDE BANG
%token SLASH PERCENT LT GT HAT BAR QUESTION COLON LBRACE RBRACE SEMICOLON EQUAL

%start<unit> translation_unit
%%

primary_expression
	: IDENTIFIER
	| CONSTANT
	| STRING_LITERAL
	| LPAREN expression RPAREN
	{} ;

postfix_expression
	: primary_expression
	| postfix_expression LBRACK expression RBRACK
	| postfix_expression LPAREN RPAREN
	| postfix_expression LPAREN argument_expression_list RPAREN
	| postfix_expression DOT IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	{} ;

argument_expression_list
	: assignment_expression
	| argument_expression_list COMMA assignment_expression
	{} ;

unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF LPAREN type_name RPAREN
	{} ;

unary_operator
	: AMPERSAND
	| STAR
	| PLUS
	| MINUS
	| TILDE
	| BANG
	{} ;

cast_expression
	: unary_expression
	| LPAREN type_name RPAREN cast_expression
	{} ;

multiplicative_expression
	: cast_expression
	| multiplicative_expression STAR cast_expression
	| multiplicative_expression SLASH cast_expression
	| multiplicative_expression PERCENT cast_expression
	{} ;

additive_expression
	: multiplicative_expression
	| additive_expression PLUS multiplicative_expression
	| additive_expression MINUS multiplicative_expression
	{} ;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	{} ;

relational_expression
	: shift_expression
	| relational_expression LT shift_expression
	| relational_expression GT shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	{} ;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	{} ;

and_expression
	: equality_expression
	| and_expression AMPERSAND equality_expression
	{} ;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression HAT and_expression
	{} ;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression BAR exclusive_or_expression
	{} ;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	{} ;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	{} ;

conditional_expression
	: logical_or_expression
	| logical_or_expression QUESTION expression COLON conditional_expression
	{} ;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	{} ;

assignment_operator
	: EQUAL
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	{} ;

expression
	: assignment_expression
	| expression COMMA assignment_expression
	{} ;

constant_expression
	: conditional_expression
	{} ;

declaration
	: declaration_specifiers SEMICOLON
	| declaration_specifiers init_declarator_list SEMICOLON
	{} ;

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	| type_qualifier
	| type_qualifier declaration_specifiers
	{} ;

init_declarator_list
	: init_declarator
	| init_declarator_list COMMA init_declarator
	{} ;

init_declarator
	: declarator
	| declarator EQUAL c_initializer
	{} ;

storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	{} ;

type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| struct_or_union_specifier
	| enum_specifier
	| TYPE_NAME
	{} ;

struct_or_union_specifier
	: struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE
	| struct_or_union LBRACE struct_declaration_list RBRACE
	| struct_or_union IDENTIFIER
	{} ;

struct_or_union
	: STRUCT
	| UNION
	{} ;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	{} ;

struct_declaration
	: specifier_qualifier_list struct_declarator_list SEMICOLON
	{} ;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	{} ;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list COMMA struct_declarator
	{} ;

struct_declarator
	: declarator
	| COLON constant_expression
	| declarator COLON constant_expression
	{} ;

enum_specifier
	: ENUM LBRACE enumerator_list RBRACE
	| ENUM IDENTIFIER LBRACE enumerator_list RBRACE
	| ENUM IDENTIFIER
	{} ;

enumerator_list
	: enumerator
	| enumerator_list COMMA enumerator
	{} ;

enumerator
	: IDENTIFIER
	| IDENTIFIER EQUAL constant_expression
	{} ;

type_qualifier
	: CONST
	| VOLATILE
	{} ;

declarator
	: pointer direct_declarator
	| direct_declarator
	{} ;

direct_declarator
	: IDENTIFIER
	| LPAREN declarator RPAREN
	| direct_declarator LBRACK constant_expression RBRACK
	| direct_declarator LBRACK RBRACK
	| direct_declarator LPAREN parameter_type_list RPAREN
	| direct_declarator LPAREN identifier_list RPAREN
	| direct_declarator LPAREN RPAREN
	{} ;

pointer
	: STAR
	| STAR type_qualifier_list
	| STAR pointer
	| STAR type_qualifier_list pointer
	{} ;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	{} ;


parameter_type_list
	: parameter_list
	| parameter_list COMMA ELLIPSIS
	{} ;

parameter_list
	: parameter_declaration
	| parameter_list COMMA parameter_declaration
	{} ;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	{} ;

identifier_list
	: IDENTIFIER
	| identifier_list COMMA IDENTIFIER
	{} ;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	{} ;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	{} ;

direct_abstract_declarator
	: LPAREN abstract_declarator RPAREN
	| LBRACK RBRACK
	| LBRACK constant_expression RBRACK
	| direct_abstract_declarator LBRACK RBRACK
	| direct_abstract_declarator LBRACK constant_expression RBRACK
	| LPAREN RPAREN
	| LPAREN parameter_type_list RPAREN
	| direct_abstract_declarator LPAREN RPAREN
	| direct_abstract_declarator LPAREN parameter_type_list RPAREN
	{} ;

c_initializer
	: assignment_expression
	| LBRACE c_initializer_list RBRACE
	| LBRACE c_initializer_list COMMA RBRACE
	{} ;

c_initializer_list
	: c_initializer
	| c_initializer_list COMMA c_initializer
	{} ;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	{} ;

labeled_statement
	: IDENTIFIER COLON statement
	| CASE constant_expression COLON statement
	| DEFAULT COLON statement
	{} ;

compound_statement
	: LBRACE RBRACE
	| LBRACE statement_list RBRACE
	| LBRACE declaration_list RBRACE
	| LBRACE declaration_list statement_list RBRACE
	{} ;

declaration_list
	: declaration
	| declaration_list declaration
	{} ;

statement_list
	: statement
	| statement_list statement
	{} ;

expression_statement
	: SEMICOLON
	| expression SEMICOLON
	{} ;

selection_statement
	: IF LPAREN expression RPAREN statement
	| IF LPAREN expression RPAREN statement ELSE statement
	| SWITCH LPAREN expression RPAREN statement
	{} ;

iteration_statement
	: WHILE LPAREN expression RPAREN statement
	| DO statement WHILE LPAREN expression RPAREN SEMICOLON
	| FOR LPAREN expression_statement expression_statement RPAREN statement
	| FOR LPAREN expression_statement expression_statement expression RPAREN statement
	{} ;

jump_statement
	: GOTO IDENTIFIER SEMICOLON
	| CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN SEMICOLON
	| RETURN expression SEMICOLON
	{} ;

translation_unit
	: external_declaration
	| translation_unit external_declaration
	{} ;

external_declaration
	: function_definition
	| declaration
	{} ;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	| declarator declaration_list compound_statement
	| declarator compound_statement
	{} ;

