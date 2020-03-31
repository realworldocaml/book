%{
open Ast
open ParseUtil
open Sclass
open Tqual

let desg dg_sloc dg = { dg; dg_sloc; }
let decl d_sloc d = { d; d_sloc; }
let stmt s_sloc s = { s; s_sloc; }
let expr e_sloc e = { e; e_sloc; e_type = { t = EmptyType; t_sloc = e_sloc; }; e_cval = Constant.NonConst; }
let ctyp t_sloc t = { t; t_sloc; }

let (@@) f x = f x






%}

%token EOF

%token KW_BOOL
%token KW_CHAR
%token KW_DOUBLE
%token KW_FLOAT
%token KW_INT
%token KW_LONG
%token KW_SHORT
%token KW_SIGNED
%token KW_UNSIGNED
%token KW_VOID
%token KW_WCHAR_T

%token<int> KW_DECIMALN
%token<int> KW_FLOATN
%token<int> KW_INTN

%token KW_COMPLEX
%token KW_CONST
%token KW_RESTRICT
%token KW_VOLATILE

%token KW_AUTO
%token KW_EXTERN
%token KW_INLINE
%token KW_REGISTER
%token KW_STATIC
%token KW_THREAD
%token KW_TYPEDEF

%token KW_DATATYPE
%token KW_ENUM
%token KW_STRUCT
%token KW_UNION

%token KW_ALIGNOF
%token KW_IMAG
%token KW_OFFSETOF
%token KW_REAL
%token KW_SIZEOF
%token KW_TYPEOF
%token KW_TYPES_COMPATIBLE_P
%token KW_VA_ARG
%token KW_VA_LIST

%token KW_BREAK
%token KW_CASE
%token KW_CONTINUE
%token KW_DEFAULT
%token KW_DO
%token KW_ELSE
%token KW_FOR
%token KW_GOTO
%token KW_IF
%token KW_RETURN
%token KW_SWITCH
%token KW_WHILE

%token KW_ASM
%token KW_LABEL

%token KW_ATTRIBUTE
%token KW_DECLSPEC
%token KW_EXTENSION
%token KW_FASTCALL

%token<string> TK_TYPEDEF_NAME
%token<string> TK_IDENTIFIER
%token<string> TK_STRING_LITERAL
%token<string> TK_WSTRING_LITERAL
%token<string> TK_CHAR_CONSTANT
%token<string> TK_WCHAR_CONSTANT
%token<string * string option> TK_FLOATING_CONSTANT
%token<string * string option> TK_HEX_FLOATING_CONSTANT
%token<string * string option> TK_INTEGER_CONSTANT
%token<string * string option> TK_OCTAL_CONSTANT
%token<string * string option> TK_HEX_CONSTANT
%token<string * string option> TK_BIN_CONSTANT

%token<string> TK_INCLUDE

%token<string> WC_EXPR
%token<string> WC_DECL
%token<string> WC_TYPE

%token TK_ELLIPSIS
%token TK_GTGT
%token TK_GTGT_EQ
%token TK_LTLT
%token TK_LTLT_EQ
%token TK_PLUS
%token TK_PLUS_EQ
%token TK_MINUS
%token TK_MINUS_EQ
%token TK_STAR
%token TK_STAR_EQ
%token TK_SLASH
%token TK_SLASH_EQ
%token TK_PERCENT
%token TK_PERCENT_EQ
%token TK_AND
%token TK_AND_EQ
%token TK_CARET
%token TK_CARET_EQ
%token TK_PIPE
%token TK_PIPE_EQ
%token TK_LESS
%token TK_LESS_EQ
%token TK_GREATER
%token TK_GREATER_EQ
%token TK_EQUALS
%token TK_EQEQ
%token TK_NE
%token TK_ANDAND
%token TK_PIPEPIPE
%token TK_INC
%token TK_DEC
%token TK_ARROW
%token TK_PERIOD
%token TK_EXMARK
%token TK_TILDE
%token TK_SEMICOLON
%token TK_COMMA
%token TK_COLON
%token TK_QMARK
%token TK_LBRACE
%token TK_RBRACE
%token TK_LBRACK
%token TK_RBRACK
%token TK_LSQBRACK
%token TK_RSQBRACK

%token KW_ATTR_ALIAS
%token KW_ATTR_ALIGNED
%token KW_ATTR_ALLOC_SIZE
%token KW_ATTR_ALTIVEC
%token KW_ATTR_ALWAYS_INLINE
%token KW_ATTR_ARTIFICIAL
%token KW_ATTR_BACK_SWITCH
%token KW_ATTR_BASED
%token KW_ATTR_BELOW100
%token KW_ATTR_CALLEE_POP_AGGREGATE_RETURN
%token KW_ATTR_CB
%token KW_ATTR_CDECL
%token KW_ATTR_CLEANUP
%token KW_ATTR_COLD
%token KW_ATTR_COMMON
%token KW_ATTR_CONST
%token KW_ATTR_CONSTRUCTOR
%token KW_ATTR_DEPRECATED
%token KW_ATTR_DESTRUCTOR
%token KW_ATTR_DISINTERRUPT
%token KW_ATTR_DLLEXPORT
%token KW_ATTR_DLLIMPORT
%token KW_ATTR_EIGHTBIT_DATA
%token KW_ATTR_EXCEPTION_HANDLER
%token KW_ATTR_EXTERNALLY_VISIBLE
%token KW_ATTR_FAR
%token KW_ATTR_FASTCALL
%token KW_ATTR_FAST_INTERRUPT
%token KW_ATTR_FLATTEN
%token KW_ATTR_FORCE_ALIGN_ARG_POINTER
%token KW_ATTR_FORMAT_ARG
%token KW_ATTR_FORMAT
%token KW_ATTR_FUNCTION_VECTOR
%token KW_ATTR_GCC_STRUCT
%token KW_ATTR_GNU_INLINE
%token KW_ATTR_HOT
%token KW_ATTR_IFUNC
%token KW_ATTR_INTERRUPT_HANDLER
%token KW_ATTR_INTERRUPT_THREAD
%token KW_ATTR_INTERRUPT
%token KW_ATTR_IO
%token KW_ATTR_ISR
%token KW_ATTR_KSPISUSP
%token KW_ATTR_L1_DATA_A
%token KW_ATTR_L1_DATA_B
%token KW_ATTR_L1_DATA
%token KW_ATTR_L1_TEXT
%token KW_ATTR_L2
%token KW_ATTR_LEAF
%token KW_ATTR_LONG_CALL
%token KW_ATTR_LONGCALL
%token KW_ATTR_MALLOC
%token KW_ATTR_MAY_ALIAS
%token KW_ATTR_MIPS16
%token KW_ATTR_MODEL
%token KW_ATTR_MODE
%token KW_ATTR_MS_ABI
%token KW_ATTR_MS_HOOK_PROLOGUE
%token KW_ATTR_MS_STRUCT
%token KW_ATTR_NAKED
%token KW_ATTR_NEAR
%token KW_ATTR_NESTING
%token KW_ATTR_NMI_HANDLER
%token KW_ATTR_NOCLONE
%token KW_ATTR_NOCOMMON
%token KW_ATTR_NOINLINE
%token KW_ATTR_NO_INSTRUMENT_FUNCTION
%token KW_ATTR_NOMIPS16
%token KW_ATTR_NONNULL
%token KW_ATTR_NORETURN
%token KW_ATTR_NO_SPLIT_STACK
%token KW_ATTR_NOTHROW
%token KW_ATTR_NOTSHARED
%token KW_ATTR_OPTIMIZE
%token KW_ATTR_OS_MAIN
%token KW_ATTR_OS_TASK
%token KW_ATTR_PACKED
%token KW_ATTR_PCS
%token KW_ATTR_PROGMEM
%token KW_ATTR_PURE
%token KW_ATTR_REGPARM
%token KW_ATTR_RESBANK
%token KW_ATTR_RETURNS_TWICE
%token KW_ATTR_SAVEALL
%token KW_ATTR_SAVE_VOLATILES
%token KW_ATTR_SECTION
%token KW_ATTR_SELECTANY
%token KW_ATTR_SENTINEL
%token KW_ATTR_SHARED
%token KW_ATTR_SHORT_CALL
%token KW_ATTR_SHORTCALL
%token KW_ATTR_SIGNAL
%token KW_ATTR_SP_SWITCH
%token KW_ATTR_SPU_VECTOR
%token KW_ATTR_SSEREGPARM
%token KW_ATTR_STDCALL
%token KW_ATTR_SYSCALL_LINKAGE
%token KW_ATTR_SYSV_ABI
%token KW_ATTR_TARGET
%token KW_ATTR_THISCALL
%token KW_ATTR_TINY_DATA
%token KW_ATTR_TINY
%token KW_ATTR_TLS_MODEL
%token KW_ATTR_TRANSPARENT_UNION
%token KW_ATTR_TRAP_EXIT
%token KW_ATTR_UNUSED
%token KW_ATTR_USED
%token KW_ATTR_VECTOR_SIZE
%token KW_ATTR_VERSION_ID
%token KW_ATTR_VISIBILITY
%token KW_ATTR_VLIW
%token KW_ATTR_WARN_UNUSED_RESULT
%token KW_ATTR_WEAKREF
%token KW_ATTR_WEAK

%nonassoc KW_IF
%nonassoc KW_ELSE

%start parse_unit parse_decl parse_expr parse_stmt parse_type test
%type<Ast.decl> parse_unit
%type<Ast.decl> parse_decl
%type<Ast.expr> parse_expr
%type<Ast.stmt> parse_stmt
%type<Ast.ctyp> parse_type
%type<bool> test
%%





parse_unit:
 | push_scope external_definitions pop_scope EOF
  { decl ($startpos, $endpos) @@ TranslationUnit (List.rev $2) }
 | EOF
  { decl ($startpos, $endpos) @@ TranslationUnit ([]) }

parse_decl:
 | push_scope declaration pop_scope EOF
  { $2 }

parse_expr:
 | push_scope expression pop_scope EOF
  { $2 }

parse_stmt:
 | push_scope statement pop_scope EOF
  { $2 }

parse_type:
 | push_scope type_name pop_scope EOF
  { $2 }


test:
 | parse_unit
  { true }






external_definitions:
 | external_definition
  { if $1.d == EmptyDecl then [] else [$1] }
 | external_definitions external_definition
  { if $2.d == EmptyDecl then $1 else $2 :: $1 }


external_definition:
 | attribute* declaration
  { attr $1 $2 }
 | attribute* default_toplevel_declaring_list TK_SEMICOLON
  { attr $1 (decl ($startpos, $endpos) @@ DeclaringList (List.rev $2)) }
 | attribute* function_definition
  { attr $1 $2 }
 | KW_ASM TK_LBRACK TK_STRING_LITERAL+ TK_RBRACK
  { decl ($startpos, $endpos) @@ ToplevelAsm $3 }
 | TK_SEMICOLON
  { decl ($startpos, $endpos) @@ EmptyDecl }
 | TK_INCLUDE
  { decl ($startpos, $endpos) @@ PreprocessorDirective ("#include " ^ $1) }
 | KW_EXTENSION external_definition
  { $2 }


function_definition:
 | function_declaration compound_statement
  { Decls.leave_function ();
    decl ($startpos, $endpos) @@ FunctionDefinition ($1, $2) }


function_declaration:
 | decl = old_function_declaration
 | decl = std_function_declaration
  { Decls.enter_function decl; decl }


std_function_declaration:
 | default_int identifier_declarator
  { Decls.set_base_type $2 $1 }
 | declaration_qualifier_list default_int identifier_declarator
  { Decls.set_base_type (Decls.merge_decls $1 $3) $2 }
 | type_qualifier+ default_int identifier_declarator
  { Decls.set_base_type $3 (Types.add_tqual $2 $1) }


old_function_declaration:
 | default_int old_function_declarator declaration*
  { Decls.add_parameter_types (Decls.set_base_type $2 $1) $3 }
 | declaration_specifier old_function_declarator declaration*
  { Decls.add_parameter_types (Decls.merge_decls $1 $2) $3 }
 | type_specifier old_function_declarator declaration*
  { Decls.add_parameter_types (Decls.set_base_type $2 $1) $3 }
 | declaration_qualifier_list default_int old_function_declarator declaration*
  { Decls.add_parameter_types (Decls.set_base_type (Decls.merge_decls $1 $3) $2) $4 }
 | type_qualifier+ default_int old_function_declarator declaration*
  { Decls.add_parameter_types (Decls.set_base_type $3 (Types.add_tqual $2 $1)) $4 }


old_function_declarator:
 | postfix_old_function_declarator
  { $1 }
 | TK_STAR attribute* old_function_declarator
  { attr $2 (Decls.set_base_type $3 (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType))) }
 | TK_STAR attribute* pointer_type_qualifier+ old_function_declarator
  { attr $2 (Decls.set_base_type $4 (Types.add_tqual (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3)) }


postfix_old_function_declarator:
 | paren_identifier_declarator TK_LBRACK sep(TK_COMMA, simple_identifier_declarator) TK_RBRACK
  { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.empty, ctyp ($startpos, $endpos) @@ FunctionType (ctyp ($startpos, $endpos) @@ EmptyType, List.map (fun id -> Decls.set_base_type id default_int) $3), $1, Decls.empty, None) }
 | TK_LBRACK attribute* old_function_declarator TK_RBRACK
  { attr $2 $3 }
 | TK_LBRACK attribute* old_function_declarator TK_RBRACK postfixing_abstract_declarator
  { attr $2 (Decls.set_base_type $3 $5) }


declaration:
 | declaring_list TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList (List.rev $1) }
 | default_declaring_list TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList (List.rev $1) }
 | sue_declaration_specifier TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList [$1] }
 | sue_type_specifier TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList [abstract_decl $1] }
 | attr_function_declaration
  { $1 }


attr_function_declaration:
 | attr_function_declarator compound_statement
  { Decls.leave_function ();
    attr (fst $1) (decl ($startpos, $endpos) @@ FunctionDefinition (snd $1, $2)) }


attr_function_declarator:
 | attr_function_declarator_
  { Decls.enter_function (snd $1); $1 }


attr_function_declarator_:
 | declaration_specifier identifier_declarator
  { [], Decls.merge_decls $1 $2 }
 | type_specifier identifier_declarator
  { [], Decls.set_base_type $2 $1 }
 | declaration_specifier_nosue attribute+ identifier_declarator
  { $2, Decls.merge_decls $1 $3 }
 | type_specifier_nosue attribute+ identifier_declarator
  { $2, Decls.set_base_type $3 $1 }


declaring_list:
 | declaration_specifier declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr $4 (Decls.finish_decl (Decls.merge_decls $1 $2) (ast_from_opt $3) $5)] }
 | declaration_specifier_nosue attribute+ declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr ($2 @ $5) (Decls.finish_decl (Decls.merge_decls $1 $3) (ast_from_opt $4) $6)] }
 | type_specifier declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr $4 (Decls.finish_decl (Decls.merge_decls (decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.empty, $1, Decls.empty, Decls.empty, None)) $2) (ast_from_opt $3) $5)] }
 | type_specifier_nosue attribute+ declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr ($2 @ $5) (Decls.finish_decl (Decls.merge_decls (decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.empty, $1, Decls.empty, Decls.empty, None)) $3) (ast_from_opt $4) $6)] }
 | declaring_list TK_COMMA attribute* declarator asm_declaration_specifier? attribute* decl_initialiser?
  { attr ($3 @ $6) (Decls.finish_decl (Decls.merge_decls (Decls.decl_base_type (List.hd $1)) $4) (ast_from_opt $5) $7) :: $1 }
default_declaring_list:
 | declaration_qualifier_list default_int identifier_declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr $5 (Decls.finish_decl (Decls.merge_decls (Decls.set_base_type $1 $2) $3) (ast_from_opt $4) $6)] }
 | type_qualifier+ default_int identifier_declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr $5 (Decls.finish_decl (Decls.set_base_type $3 (Types.add_tqual $2 $1)) (ast_from_opt $4) $6)] }
 | default_declaring_list TK_COMMA attribute* identifier_declarator asm_declaration_specifier? attribute* decl_initialiser?
  { attr ($3 @ $6) (Decls.finish_decl (Decls.merge_decls (Decls.decl_base_type (List.hd $1)) $4) (ast_from_opt $5) $7) :: $1 }
default_toplevel_declaring_list:
 | default_int identifier_declarator asm_declaration_specifier? attribute* decl_initialiser?
  { [attr $4 (Decls.finish_decl (Decls.set_base_type $2 $1) (ast_from_opt $3) $5)] }
 | default_toplevel_declaring_list TK_COMMA identifier_declarator asm_declaration_specifier? attribute* decl_initialiser?
  { attr $5 (Decls.finish_decl (Decls.merge_decls (Decls.decl_base_type (List.hd $1)) $3) (ast_from_opt $4) $6) :: $1 }


declaration_specifier:
 | declaration_specifier_nosue { $1 }
 | sue_declaration_specifier { $1 }


declaration_specifier_nosue:
 | basic_declaration_specifier { $1 }
 | typedef_declaration_specifier { $1 }
 | typeof_declaration_specifier { $1 }
 | WC_TYPE { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.empty, ctyp ($startpos, $endpos) @@ WildcardType $1, Decls.empty, Decls.empty, None) }


basic_declaration_specifier:
 | basic_type_specifier storage_class
  { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $2, $1, Decls.empty, Decls.empty, None) }
 | declaration_qualifier_list basic_type_name
  { Decls.add_basic_type $1 $2 }
 | basic_declaration_specifier declaration_qualifier
  { Decls.add_dqual $1 $2 }
 | basic_declaration_specifier basic_type_name
  { Decls.add_basic_type $1 $2 }


sue_declaration_specifier:
 | sue_type_specifier storage_class
  { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $2, $1, Decls.empty, Decls.empty, None) }
 | declaration_qualifier_list elaborated_type_name
  { Decls.set_base_type $1 $2 }
 | sue_declaration_specifier declaration_qualifier
  { Decls.add_dqual $1 $2 }


typeof_declaration_specifier:
 | typeof_type_specifier storage_class
  { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $2, $1, Decls.empty, Decls.empty, None) }
 | declaration_qualifier_list typeof_type_name
  { Decls.set_base_type $1 $2 }
 | typeof_declaration_specifier declaration_qualifier
  { Decls.add_dqual $1 $2 }


typedef_declaration_specifier:
 | typedef_type_specifier storage_class
  { decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $2, $1, Decls.empty, Decls.empty, None) }
 | declaration_qualifier_list TK_TYPEDEF_NAME
  { Decls.set_base_type $1 (ctyp ($startpos, $endpos) @@ TypedefType $2) }
 | typedef_declaration_specifier declaration_qualifier
  { Decls.add_dqual $1 $2 }


declaration_qualifier_list:
 | storage_class attribute*
  { attr $2 (decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $1, ctyp ($startpos, $endpos) @@ EmptyType, Decls.empty, Decls.empty, None)) }
 | type_qualifier+ attribute* storage_class attribute*
  { attr ($2 @ $4) (decl ($startpos, $endpos) @@ TypedDecl ("", Sclass.add Sclass.empty $3, Types.add_tqual (ctyp ($startpos, $endpos) @@ EmptyType) $1, Decls.empty, Decls.empty, None)) }
 | declaration_qualifier_list declaration_qualifier attribute*
  { attr $3 (Decls.add_dqual $1 $2) }


declaration_qualifier:
 | type_qualifier { (None, Some $1) }
 | storage_class { (Some $1, None) }


type_specifier:
 | type_specifier_nosue { $1 }
 | sue_type_specifier { $1 }


type_specifier_nosue:
 | basic_type_specifier { $1 }
 | typedef_type_specifier { $1 }
 | typeof_type_specifier { $1 }


basic_type_specifier:
 | basic_type_name
  { ctyp ($startpos, $endpos) @@ PartialBasicType [$1] }
 | type_qualifier+ basic_type_name
  { Types.add_tqual (ctyp ($startpos, $endpos) @@ PartialBasicType [$2]) $1 }
 | basic_type_specifier type_qualifier
  { Types.add_tqual $1 [$2] }
 | basic_type_specifier basic_type_name
  { Types.add_basic_type $1 $2 }


sue_type_specifier:
 | elaborated_type_name
  { $1 }
 | type_qualifier+ elaborated_type_name
  { Types.add_tqual $2 $1 }
 | sue_type_specifier type_qualifier
  { Types.add_tqual $1 [$2] }


typedef_type_specifier:
 | TK_TYPEDEF_NAME
  { ctyp ($startpos, $endpos) @@ TypedefType $1 }
 | type_qualifier+ TK_TYPEDEF_NAME
  { Types.add_tqual (ctyp ($startpos, $endpos) @@ TypedefType $2) $1 }
 | typedef_type_specifier type_qualifier
  { Types.add_tqual $1 [$2] }


typeof_type_specifier:
 | typeof_type_name
  { $1 }
 | type_qualifier+ typeof_type_name
  { Types.add_tqual $2 $1 }
 | typeof_type_specifier type_qualifier
  { Types.add_tqual $1 [$2] }


typeof_type_name:
 | KW_TYPEOF TK_LBRACK type_specifier TK_RBRACK
  { ctyp ($startpos, $endpos) @@ TypeofType $3 }
 | KW_TYPEOF TK_LBRACK expression TK_RBRACK
  { ctyp ($startpos, $endpos) @@ TypeofExpr $3 }


elaborated_type_name:
 | struct_or_union_specifier { $1 }
 | enum_specifier attribute* { $1 }
 | datatype_specifier { $1 }


declarator:
 | paren_typedef_declarator { $1 }
 | parameter_typedef_declarator { $1 }
 | identifier_declarator { $1 }
 | old_function_declarator { $1 }
 | KW_EXTENSION declarator { $2 }


paren_typedef_declarator:
 | paren_postfix_typedef_declarator
  { $1 }
 | TK_STAR attribute* paren_typedef_declarator
  { attr $2 (Decls.add_pointer_type $3) }
 | TK_STAR attribute* TK_LBRACK attribute* simple_paren_typedef_declarator TK_RBRACK
  { attr ($2 @ $4) (Decls.add_pointer_type $5) }
 | TK_STAR attribute* pointer_type_qualifier+ TK_LBRACK attribute* simple_paren_typedef_declarator TK_RBRACK
  { attr ($2 @ $5) (Decls.add_tqual (Decls.add_pointer_type $6) $3) }
 | TK_STAR attribute* pointer_type_qualifier+ paren_typedef_declarator
  { attr $2 (Decls.add_tqual (Decls.add_pointer_type $4) $3) }


paren_postfix_typedef_declarator:
 | TK_LBRACK attribute* paren_typedef_declarator TK_RBRACK
  { attr $2 $3 }
 | TK_LBRACK attribute* simple_paren_typedef_declarator postfixing_abstract_declarator TK_RBRACK
  { attr $2 (Decls.set_base_type $3 $4) }
 | TK_LBRACK attribute* paren_typedef_declarator TK_RBRACK postfixing_abstract_declarator
  { attr $2 (Decls.set_base_type $3 $5) }


simple_paren_typedef_declarator:
 | simple_typedef_declarator
  { $1 }
 | TK_LBRACK attribute* simple_paren_typedef_declarator TK_RBRACK
  { attr $2 $3 }


parameter_typedef_declarator:
 | simple_typedef_declarator
  { $1 }
 | simple_typedef_declarator postfixing_abstract_declarator
  { Decls.set_base_type $1 $2 }
 | clean_typedef_declarator
  { $1 }


simple_typedef_declarator:
 | TK_TYPEDEF_NAME
  { decl ($startpos, $endpos) @@ IdentifierDeclarator ([], $1) }


clean_typedef_declarator:
 | clean_postfix_typedef_declarator
  { $1 }
 | TK_STAR attribute* parameter_typedef_declarator
  { attr $2 (Decls.set_base_type $3 (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType))) }
 | TK_STAR attribute* pointer_type_qualifier+ parameter_typedef_declarator
  { attr $2 (Decls.set_base_type $4 (Types.add_tqual (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3)) }


clean_postfix_typedef_declarator:
 | TK_LBRACK attribute* clean_typedef_declarator TK_RBRACK
  { attr $2 $3 }
 | TK_LBRACK attribute* clean_typedef_declarator TK_RBRACK postfixing_abstract_declarator
  { attr $2 (Decls.set_base_type $3 $5) }


abstract_declarator:
 | unary_abstract_declarator { $1 }
 | postfix_abstract_declarator { $1 }
 | postfixing_abstract_declarator { $1 }


unary_abstract_declarator:
 | TK_STAR attribute*
  { ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType) }
 | TK_STAR attribute* pointer_type_qualifier+
  { Types.add_tqual (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3 }
 | TK_STAR attribute* abstract_declarator
  { Types.set_base_type (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3 }
 | TK_STAR attribute* pointer_type_qualifier+ abstract_declarator
  { Types.set_base_type (Types.add_tqual (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3) $4 }


postfix_abstract_declarator:
 | TK_LBRACK attribute* unary_abstract_declarator TK_RBRACK
  { $3 }
 | TK_LBRACK attribute* postfix_abstract_declarator TK_RBRACK
  { $3 }
 | TK_LBRACK attribute* postfixing_abstract_declarator TK_RBRACK
  { $3 }
 | TK_LBRACK attribute* unary_abstract_declarator TK_RBRACK postfixing_abstract_declarator
  { Types.set_base_type $5 $3 }


postfixing_abstract_declarator:
 | array_abstract_declarator+
  { Types.make_array_type $1 }
 | TK_LBRACK forward_parameter_type_list? TK_RBRACK
  { let params, forwards = list_pair_from_opt $2 in ctyp ($startpos, $endpos) @@ FunctionType (ctyp ($startpos, $endpos) @@ EmptyType, params) }


identifier_declarator:
 | unary_identifier_declarator { $1 }
 | paren_identifier_declarator { $1 }


unary_identifier_declarator:
 | postfix_identifier_declarator
  { $1 }
 | TK_STAR attribute* identifier_declarator
  { attr $2 (Decls.set_base_type $3 (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType))) }
 | TK_STAR attribute* pointer_type_qualifier+ identifier_declarator
  { attr $2 (Decls.set_base_type $4 (Types.add_tqual (ctyp ($startpos, $endpos) @@ PointerType (ctyp ($startpos, $endpos) @@ EmptyType)) $3)) }


postfix_identifier_declarator:
 | paren_identifier_declarator postfixing_abstract_declarator
  { Decls.set_base_type $1 $2 }
 | TK_LBRACK attribute* unary_identifier_declarator TK_RBRACK
  { attr $2 $3 }
 | TK_LBRACK attribute* unary_identifier_declarator TK_RBRACK postfixing_abstract_declarator
  { attr $2 (Decls.set_base_type $3 $5) }


paren_identifier_declarator:
 | simple_identifier_declarator
  { $1 }
 | TK_LBRACK attribute* paren_identifier_declarator TK_RBRACK
  { attr $2 $3 }
 | WC_DECL
  { decl ($startpos, $endpos) @@ WildcardDecl $1 }


simple_identifier_declarator:
 | TK_IDENTIFIER
  { decl ($startpos, $endpos) @@ IdentifierDeclarator ([], $1) }


type_name:
 | attribute* type_name_noattr
  { $2 }


type_name_noattr:
 | type_specifier
  { $1 }
 | type_specifier abstract_declarator
  { Types.set_base_type $1 $2 }
 | type_qualifier+ default_int
  { Types.add_tqual $2 $1 }
 | type_qualifier+ default_int abstract_declarator
  { Types.set_base_type (Types.add_tqual $2 $1) $3 }
 | WC_TYPE
  { ctyp ($startpos, $endpos) @@ WildcardType $1 }






decl_initialiser:
 | TK_EQUALS basic_initialiser
  { $2 }


initialiser:
 | basic_initialiser
  { $1 }
 | member_designators basic_initialiser
  { expr ($startpos, $endpos) @@ DesignatedInitialiser ($1, $2) }
 | TK_LSQBRACK range_expression TK_RSQBRACK TK_EQUALS? basic_initialiser
  { expr ($startpos, $endpos) @@ ArrayLabelledInitialiser ($2, $5) }


member_designators:
 | member_designator+ TK_EQUALS
  { desg ($startpos, $endpos) @@ $1 }
 | TK_IDENTIFIER TK_COLON
  { desg ($startpos, $endpos) @@ [$1] }


member_designator:
 | TK_PERIOD TK_IDENTIFIER
  { $2 }


range_expression:
 | assignment_expression
  { $1 }
 | assignment_expression TK_ELLIPSIS assignment_expression
  { expr ($startpos, $endpos) @@ BinaryExpression (OP_Ellipsis, $1, $3) }


basic_initialiser:
 | bracketed_initialiser_list { $1 }
 | assignment_expression { $1 }


bracketed_initialiser_list:
 | TK_LBRACE TK_RBRACE
  { expr ($startpos, $endpos) @@ InitialiserList [] }
 | TK_LBRACE sep_rev(TK_COMMA, initialiser) TK_COMMA? TK_RBRACE
  { expr ($startpos, $endpos) @@ InitialiserList (List.rev $2) }






forward_parameter_type_list:
 | sep_rev(TK_SEMICOLON, parameter_type_list)
  { List.hd $1, List.rev (List.tl $1) }


parameter_type_list:
 | sep_rev(TK_COMMA, parameter_declaration)
  { List.rev $1 }
 | sep_rev(TK_COMMA, parameter_declaration) TK_COMMA TK_ELLIPSIS
  { List.rev (abstract_decl (ctyp ($startpos, $endpos) @@ PartialBasicType [BT_Ellipsis]) :: $1) }
parameter_declaration:
 | attribute* parameter_declaration_noattr
  { assert (Predicates.is_decl $2); { $2 with d_sloc = ($startpos, $endpos) } }






parameter_declaration_noattr:
 | declaration_specifier
  { $1 }
 | declaration_specifier abstract_declarator
  { Decls.merge_decls $1 (abstract_decl $2) }
 | declaration_specifier identifier_declarator attribute*
  { attr $3 (Decls.merge_decls $1 $2) }
 | declaration_specifier parameter_typedef_declarator attribute*
  { attr $3 (Decls.merge_decls $1 $2) }
 | declaration_qualifier_list default_int
  { Decls.set_base_type $1 $2 }
 | declaration_qualifier_list default_int abstract_declarator
  { Decls.set_base_type (Decls.merge_decls $1 (abstract_decl $3)) $2 }
 | declaration_qualifier_list default_int identifier_declarator attribute*
  { attr $4 (Decls.set_base_type (Decls.merge_decls $1 $3) $2) }
 | type_specifier
  { abstract_decl $1 }
 | type_specifier abstract_declarator
  { abstract_decl (Types.set_base_type $1 $2) }
 | type_specifier identifier_declarator attribute*
  { attr $3 (Decls.set_tspec $2 $1) }
 | type_specifier parameter_typedef_declarator attribute*
  { attr $3 (Decls.set_tspec $2 $1) }
 | type_qualifier+ default_int
  { abstract_decl (Types.add_tqual $2 $1) }
 | type_qualifier+ default_int abstract_declarator
  { abstract_decl (Types.set_base_type (Types.add_tqual $2 $1) $3) }
 | type_qualifier+ default_int identifier_declarator attribute*
  { attr $4 (Decls.set_tspec $3 (Types.add_tqual $2 $1)) }


array_abstract_declarator:
 | TK_LSQBRACK constant_expression? TK_RSQBRACK
  { ctyp ($startpos, $endpos) @@ ArrayType ($2, ctyp ($startpos, $endpos) @@ EmptyType) }


primary_expression:
 | identifier
  { $1 }
 | numeric_literal
  { $1 }
 | string_literal
  { $1 }
 | TK_LBRACK expression TK_RBRACK
  { $2 }
 | statement_expression
  { $1 }
 | WC_EXPR
  { expr ($startpos, $endpos) @@ WildcardExpr $1 }





statement_expression:
 | TK_LBRACK compound_statement TK_RBRACK
  { expr ($startpos, $endpos) @@ BraceExpression $2 }


postfix_expression:
 | primary_expression
  { $1 }
 | postfix_expression TK_LSQBRACK expression TK_RSQBRACK
  { expr ($startpos, $endpos) @@ ArrayAccess ($1, $3) }
 | postfix_expression TK_LBRACK sep(TK_COMMA, assignment_expression)? TK_RBRACK
  { expr ($startpos, $endpos) @@ FunctionCall ($1, list_from_opt $3) }
 | postfix_expression TK_PERIOD identifier_or_typedef_name
  { expr ($startpos, $endpos) @@ MemberAccess ($1, $3) }
 | postfix_expression TK_ARROW identifier_or_typedef_name
  { expr ($startpos, $endpos) @@ PointerAccess ($1, $3) }
 | postfix_expression TK_PERIOD WC_DECL
  { expr ($startpos, $endpos) @@ MemberAccess ($1, $3) }
 | postfix_expression TK_ARROW WC_DECL
  { expr ($startpos, $endpos) @@ PointerAccess ($1, $3) }
 | postfix_expression TK_INC
  { expr ($startpos, $endpos) @@ UnaryExpression (OP_PostIncrement, $1) }
 | postfix_expression TK_DEC
  { expr ($startpos, $endpos) @@ UnaryExpression (OP_PostDecrement, $1) }
 | KW_OFFSETOF TK_LBRACK type_name TK_COMMA postfix_expression TK_RBRACK
  { expr ($startpos, $endpos) @@ Offsetof ($3, $5) }
 | KW_TYPES_COMPATIBLE_P TK_LBRACK type_name TK_COMMA type_name TK_RBRACK
  { expr ($startpos, $endpos) @@ TypesCompatibleP ($3, $5) }
 | KW_VA_ARG TK_LBRACK assignment_expression TK_COMMA type_name TK_RBRACK
  { expr ($startpos, $endpos) @@ VaArg ($3, $5) }
 | TK_LBRACK type_name TK_RBRACK bracketed_initialiser_list
  { expr ($startpos, $endpos) @@ CompoundLiteral ($2, $4) }


unary_prefix_operator:
 | TK_INC { OP_PreIncrement }
 | TK_DEC { OP_PreDecrement }
unary_operator:
 | TK_AND { OP_AddressOf }
 | TK_ANDAND { OP_AddressOfLabel }
 | TK_STAR { OP_Dereference }
 | TK_PLUS { OP_Identity }
 | TK_MINUS { OP_Negate }
 | TK_TILDE { OP_BitwiseNot }
 | TK_EXMARK { OP_LogicalNot }
 | KW_IMAG { OP_Imag }
 | KW_REAL { OP_Real }
unary_expression:
 | postfix_expression
  { $1 }
 | unary_prefix_operator unary_expression
  { expr ($startpos, $endpos) @@ UnaryExpression ($1, $2) }
 | unary_operator cast_expression
  { expr ($startpos, $endpos) @@ UnaryExpression ($1, $2) }
 | KW_SIZEOF unary_expression
  { expr ($startpos, $endpos) @@ SizeofExpr ($2) }
 | KW_SIZEOF TK_LBRACK type_name TK_RBRACK
  { expr ($startpos, $endpos) @@ SizeofType ($3) }
 | KW_ALIGNOF TK_LBRACK unary_expression TK_RBRACK
  { expr ($startpos, $endpos) @@ AlignofExpr ($3) }
 | KW_ALIGNOF TK_LBRACK type_name TK_RBRACK
  { expr ($startpos, $endpos) @@ AlignofType ($3) }
 | KW_EXTENSION cast_expression
  { $2 }


cast_expression:
 | unary_expression
  { $1 }
 | TK_LBRACK type_name TK_RBRACK cast_expression
  { expr ($startpos, $endpos) @@ Cast ($2, $4) }


multiplicative_operator:
 | TK_STAR { OP_Multiply }
 | TK_SLASH { OP_Divide }
 | TK_PERCENT { OP_Modulo }
multiplicative_expression:
 | cast_expression
  { $1 }
 | multiplicative_expression multiplicative_operator cast_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


additive_operator:
 | TK_PLUS { OP_Add }
 | TK_MINUS { OP_Subtract }
additive_expression:
 | multiplicative_expression
  { $1 }
 | additive_expression additive_operator multiplicative_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


shift_operator:
 | TK_LTLT { OP_ShiftLeft }
 | TK_GTGT { OP_ShiftRight }
shift_expression:
 | additive_expression
  { $1 }
 | shift_expression shift_operator additive_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


relational_operator:
 | TK_LESS { OP_Less }
 | TK_GREATER { OP_Greater }
 | TK_LESS_EQ { OP_LessEqual }
 | TK_GREATER_EQ { OP_GreaterEqual }
relational_expression:
 | shift_expression
  { $1 }
 | relational_expression relational_operator shift_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


equality_operator:
 | TK_EQEQ { OP_Equal }
 | TK_NE { OP_NotEqual }
equality_expression:
 | relational_expression
  { $1 }
 | equality_expression equality_operator relational_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


and_operator:
 | TK_AND { OP_BitwiseAnd }
and_expression:
 | equality_expression
  { $1 }
 | and_expression and_operator equality_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


exclusive_or_operator:
 | TK_CARET { OP_BitwiseXor }
exclusive_or_expression:
 | and_expression
  { $1 }
 | exclusive_or_expression exclusive_or_operator and_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


inclusive_or_operator:
 | TK_PIPE { OP_BitwiseOr }
inclusive_or_expression:
 | exclusive_or_expression
  { $1 }
 | inclusive_or_expression inclusive_or_operator exclusive_or_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


logical_and_operator:
 | TK_ANDAND { OP_LogicalAnd }
logical_and_expression:
 | inclusive_or_expression
  { $1 }
 | logical_and_expression logical_and_operator inclusive_or_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


logical_or_operator:
 | TK_PIPEPIPE { OP_LogicalOr }
logical_or_expression:
 | logical_and_expression
  { $1 }
 | logical_or_expression logical_or_operator logical_and_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


conditional_expression:
 | logical_or_expression
  { $1 }
 | logical_or_expression TK_QMARK expression? TK_COLON conditional_expression
  { expr ($startpos, $endpos) @@ TernaryExpression (OP_Conditional, $1, $3, $5) }


assignment_operator:
 | TK_EQUALS { OP_Assign }
 | TK_STAR_EQ { OP_MultiplyAssign }
 | TK_SLASH_EQ { OP_DivideAssign }
 | TK_PERCENT_EQ { OP_ModuloAssign }
 | TK_PLUS_EQ { OP_AddAssign }
 | TK_MINUS_EQ { OP_SubtractAssign }
 | TK_LTLT_EQ { OP_ShiftLeftAssign }
 | TK_GTGT_EQ { OP_ShiftRightAssign }
 | TK_AND_EQ { OP_BitwiseAndAssign }
 | TK_CARET_EQ { OP_BitwiseXorAssign }
 | TK_PIPE_EQ { OP_BitwiseOrAssign }
assignment_expression:
 | conditional_expression
  { $1 }
 | cast_expression assignment_operator assignment_expression
  { expr ($startpos, $endpos) @@ BinaryExpression ($2, $1, $3) }


expression:
 | assignment_expression
  { $1 }
 | expression TK_COMMA assignment_expression
  { expr ($startpos, $endpos) @@ BinaryExpression (OP_Comma, $1, $3) }


constant_expression:
 | conditional_expression
  { $1 }


numeric_literal:
 | TK_INTEGER_CONSTANT
  { expr ($startpos, $endpos) @@ IntegerLiteral (LIT_Dec, fst $1, snd $1) }
 | TK_OCTAL_CONSTANT
  { expr ($startpos, $endpos) @@ IntegerLiteral (LIT_Oct, fst $1, snd $1) }
 | TK_HEX_CONSTANT
  { expr ($startpos, $endpos) @@ IntegerLiteral (LIT_Hex, fst $1, snd $1) }
 | TK_BIN_CONSTANT
  { expr ($startpos, $endpos) @@ IntegerLiteral (LIT_Bin, fst $1, snd $1) }
 | TK_FLOATING_CONSTANT
  { expr ($startpos, $endpos) @@ FloatingLiteral (LIT_Float, fst $1, snd $1) }
 | TK_HEX_FLOATING_CONSTANT
  { expr ($startpos, $endpos) @@ FloatingLiteral (LIT_HexFloat, fst $1, snd $1) }
 | TK_CHAR_CONSTANT
  { expr ($startpos, $endpos) @@ CharLiteral (LIT_Char, $1) }
 | TK_WCHAR_CONSTANT
  { expr ($startpos, $endpos) @@ CharLiteral (LIT_WChar, $1) }


string_literal:
 | simple_string_literal+
  { expr ($startpos, $endpos) @@ merge_string_literals [] $1 }

simple_string_literal:
 | TK_STRING_LITERAL
  { LIT_String, $1 }
 | TK_WSTRING_LITERAL
  { LIT_WString, $1 }


type_qualifier:
 | KW_CONST
  { TQ_Const }
 | KW_VOLATILE
  { TQ_Volatile }
 | KW_RESTRICT
  { TQ_Restrict }
 | KW_COMPLEX
  { TQ_Complex }


pointer_type_qualifier:
 | KW_CONST
  { TQ_Const }
 | KW_VOLATILE
  { TQ_Volatile }
 | KW_RESTRICT
  { TQ_Restrict }


storage_class:
 | KW_TYPEDEF
  { SC_Typedef }
 | KW_EXTERN
  { SC_Extern }
 | KW_STATIC
  { SC_Static }
 | KW_AUTO
  { SC_Auto }
 | KW_REGISTER
  { SC_Register }
 | KW_INLINE
  { SC_Inline }
 | KW_THREAD
  { SC_Thread }


basic_type_name:
 | KW_SIGNED
  { BT_Signed }
 | KW_UNSIGNED
  { BT_Unsigned }
 | KW_BOOL
  { BT_Bool }
 | KW_CHAR
  { BT_Char }
 | KW_INT
  { BT_Int }
 | KW_SHORT
  { BT_Short }
 | KW_LONG
  { BT_Long }

 | KW_FLOAT
  { BT_Float }
 | KW_DOUBLE
  { BT_Double }

 | KW_INTN
  { BT_IntN $1 }
 | KW_FLOATN
  { BT_FloatN $1 }
 | KW_DECIMALN
  { BT_DecimalN $1 }

 | KW_VA_LIST
  { BT_VaList }
 | KW_WCHAR_T
  { BT_WCharT }

 | KW_VOID
  { BT_Void }


default_int:
 |
  { default_int }


statement:
 | statement_noexpr { $1 }
 | expression_statement { $1 }





statement_noexpr:
 | labelled_statement { $1 }
 | compound_statement { $1 }
 | selection_statement { $1 }
 | iteration_statement { $1 }
 | jump_statement { $1 }
 | asm_statement { $1 }
 | KW_EXTENSION statement_noexpr { $2 }


statement_or_declaration:
 | statement
  { $1 }
 | KW_EXTENSION? attribute* declaration
  { stmt ($startpos, $endpos) @@ DeclarationStatement (attr $2 $3) }


labelled_statement:
 | TK_IDENTIFIER TK_COLON statement
  { stmt ($startpos, $endpos) @@ LabelledStatement ($1, $3) }
 | KW_LABEL sep(TK_COMMA, TK_IDENTIFIER) TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ LocalLabel $2 }
 | KW_CASE range_expression TK_COLON
  { stmt ($startpos, $endpos) @@ CaseStatement $2 }
 | KW_DEFAULT TK_COLON
  { stmt ($startpos, $endpos) @@ DefaultStatement }


compound_statement:
 | TK_LBRACE push_scope statement_or_declaration* pop_scope TK_RBRACE
  { stmt ($startpos, $endpos) @@ CompoundStatement ("", $3) }

push_scope:
 | { Lexer_hack.push_scope () }

pop_scope:
 | { Lexer_hack.pop_scope () }


expression_statement:
 | expression? TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ ExpressionStatement $1 }


selection_statement:
 | KW_IF TK_LBRACK expression TK_RBRACK statement %prec KW_IF
  { stmt ($startpos, $endpos) @@ IfStatement ($3, $5, stmt ($startpos, $endpos) @@ EmptyStmt) }
 | KW_IF TK_LBRACK expression TK_RBRACK statement KW_ELSE statement
  { stmt ($startpos, $endpos) @@ IfStatement ($3, $5, $7) }
 | KW_SWITCH TK_LBRACK expression TK_RBRACK statement
  { stmt ($startpos, $endpos) @@ SwitchStatement ($3, $5) }


iteration_statement:
 | KW_WHILE TK_LBRACK expression TK_RBRACK statement
  { stmt ($startpos, $endpos) @@ WhileStatement ($3, $5) }
 | KW_DO statement KW_WHILE TK_LBRACK expression TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ DoWhileStatement ($2, $5) }
 | KW_FOR TK_LBRACK expression? TK_SEMICOLON expression? TK_SEMICOLON expression? TK_RBRACK statement
  { stmt ($startpos, $endpos) @@ ForStatement ($3, $5, $7, $9) }
jump_statement:
 | KW_GOTO expression TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ GotoStatement $2 }
 | KW_CONTINUE TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ ContinueStatement }
 | KW_BREAK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ BreakStatement }
 | KW_RETURN expression? TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ ReturnStatement $2 }


struct_or_union_specifier:
 | struct_or_union attribute* TK_LBRACE struct_declaration_list_opt TK_RBRACE attribute*
  { ctyp ($startpos, $endpos) @@ SUEType (List.flatten $2, $1, "", $4) }
 | struct_or_union attribute* identifier_or_typedef_name attribute*
  { ctyp ($startpos, $endpos) @@ SUEType (List.flatten $2, $1, $3, []) }
 | struct_or_union attribute* identifier_or_typedef_name attribute* TK_LBRACE struct_declaration_list_opt TK_RBRACE attribute*
  { ctyp ($startpos, $endpos) @@ SUEType (List.flatten $2, $1, $3, $6) }


struct_or_union:
 | KW_STRUCT
  { SUE_Struct }
 | KW_UNION
  { SUE_Union }


identifier:
 | TK_IDENTIFIER
  { expr ($startpos, $endpos) @@ Identifier ($1) }


identifier_or_typedef_name:
 | TK_IDENTIFIER
  { $1 }
 | TK_TYPEDEF_NAME
  { $1 }




struct_declaration:
 | struct_declaring_list TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList (List.rev $1) }
 | struct_default_declaring_list TK_SEMICOLON
  { decl ($startpos, $endpos) @@ DeclaringList (List.rev $1) }
 | KW_EXTENSION struct_declaration
  { $2 }


struct_declaration_list:
 | attribute* struct_declaration
  { [$2] }
 | struct_declaration_list attribute* struct_declaration
  { $3 :: $1 }

struct_declaration_list_opt:
 |
  { [Decls.empty] }
 | struct_declaration_list
  { List.rev $1 }


struct_default_declaring_list:
 | type_qualifier+ struct_identifier_declarator
  { [Decls.add_tqual $2 $1] }
 | struct_default_declaring_list TK_COMMA struct_identifier_declarator
  { $3 :: $1 }


struct_declaring_list:
 | type_specifier struct_declarator
  { [Decls.set_tspec $2 $1] }
 | type_specifier_nosue attribute+ struct_declarator
  { [attr $2 (Decls.set_tspec $3 $1)] }
 | struct_declaring_list TK_COMMA struct_declarator
  { (Decls.merge_decls (Decls.decl_base_type (List.hd $1)) $3) :: $1 }
 | struct_or_union attribute* TK_LBRACE struct_declaration_list_opt TK_RBRACE
  { [attr $2 (Decls.set_tspec (decl ($startpos, $endpos) @@ StructDeclarator (Decls.empty, None)) (ctyp ($startpos, $endpos) @@ SUEType (List.flatten $2, $1, "", $4)))] }


struct_declarator:
 | declarator bit_field_size? attribute*
  { decl ($startpos, $endpos) @@ StructDeclarator (attr $3 $1, $2) }
 | bit_field_size attribute*
  { decl ($startpos, $endpos) @@ StructDeclarator (Decls.empty, Some $1) }


struct_identifier_declarator:
 | default_int identifier_declarator bit_field_size? attribute*
  { Decls.set_tspec (decl ($startpos, $endpos) @@ StructDeclarator (attr $4 $2, $3)) $1 }


bit_field_size:
 | TK_COLON constant_expression
  { $2 }




enum_specifier:
 | KW_ENUM TK_LBRACE sep_rev(TK_COMMA, enumerator) TK_COMMA? TK_RBRACE
  { ctyp ($startpos, $endpos) @@ SUEType ([], SUE_Enum, "", List.rev $3) }
 | KW_ENUM TK_IDENTIFIER TK_LBRACE sep_rev(TK_COMMA, enumerator) TK_COMMA? TK_RBRACE
  { ctyp ($startpos, $endpos) @@ SUEType ([], SUE_Enum, $2, List.rev $4) }
 | KW_ENUM TK_IDENTIFIER
  { ctyp ($startpos, $endpos) @@ SUEType ([], SUE_Enum, $2, []) }


enumerator:
 | TK_IDENTIFIER enumerator_value?
  { decl ($startpos, $endpos) @@ Enumerator ($1, $2) }


enumerator_value:
 | TK_EQUALS constant_expression
  { $2 }




datatype_specifier:
 | KW_DATATYPE datatype_name TK_EQUALS datatype_declaring_list
  { ctyp ($startpos, $endpos) @@ SUEType ([], SUE_Enum, $2, []) }


datatype_name:
 | identifier_or_typedef_name
  { Lexer_hack.typedef $1; $1 }


datatype_declaring_list:
 | TK_PIPE sep(TK_PIPE, datatype_declarator)
  { 0 }


datatype_declarator:
 | TK_IDENTIFIER datatype_member_list
  { 0 }


datatype_member_list:
 | TK_LBRACK sep(TK_COMMA, datatype_member) TK_RBRACK
  { 0 }


datatype_member:
 | TK_IDENTIFIER TK_COLON type_name
  { 0 }




asm_declaration_specifier:
 | KW_ASM TK_LBRACK TK_STRING_LITERAL+ TK_RBRACK
  { decl ($startpos, $endpos) @@ AsmSpecifier $3 }


asm_statement:
 | KW_ASM boption(KW_VOLATILE) TK_LBRACK TK_STRING_LITERAL+ TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ AsmStatement ($2, $4, [], [], [], []) }
 | KW_ASM boption(KW_VOLATILE) TK_LBRACK TK_STRING_LITERAL+ TK_COLON asm_argument_list? TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ AsmStatement ($2, $4, list_from_opt $6, [], [], []) }
 | KW_ASM boption(KW_VOLATILE) TK_LBRACK TK_STRING_LITERAL+ TK_COLON asm_argument_list? TK_COLON asm_argument_list? TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ AsmStatement ($2, $4, list_from_opt $6, list_from_opt $8, [], []) }
 | KW_ASM boption(KW_VOLATILE) TK_LBRACK TK_STRING_LITERAL+ TK_COLON asm_argument_list? TK_COLON asm_argument_list? TK_COLON sep(TK_COMMA, asm_clobbered)? TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ AsmStatement ($2, $4, list_from_opt $6, list_from_opt $8, list_from_opt $10, []) }
 | KW_ASM boption(KW_VOLATILE) KW_GOTO TK_LBRACK TK_STRING_LITERAL+ TK_COLON TK_COLON asm_argument_list? TK_COLON sep(TK_COMMA, asm_clobbered)? TK_COLON sep(TK_COMMA, TK_IDENTIFIER) TK_RBRACK TK_SEMICOLON
  { stmt ($startpos, $endpos) @@ AsmStatement ($2, $5, [], list_from_opt $8, list_from_opt $10, $12) }

asm_argument_list:
 | sep(TK_COMMA, asm_argument)
  { $1 }


asm_argument:
 | TK_STRING_LITERAL+ TK_LBRACK assignment_expression TK_RBRACK
  { AsmArgument ($1, $3) }


asm_clobbered:
 | TK_STRING_LITERAL+
  { $1 }




sep(S,T): sep_rev(S,T) { List.rev $1 }
sep_rev(S,T):
 | T
  { [$1] }
 | sep_rev(S,T) S T
  { $3 :: $1 }

list(X): list_rev(X) { List.rev $1 }
list_rev(X):
 |
  { [] }
 | list_rev(X) X
  { $2 :: $1 }

nonempty_list(X): nonempty_list_rev(X) { List.rev $1 }
nonempty_list_rev(X):
 | X
  { [$1] }
 | nonempty_list_rev(X) X
  { $2 :: $1 }

option(X):
 |
  { None }
 | X
  { Some $1 }

boption(X):
 |
  { false }
 | X
  { true }




attribute:
 | KW_ATTRIBUTE TK_LBRACK TK_LBRACK sep(TK_COMMA, attrib)? TK_RBRACK TK_RBRACK
  { list_from_opt $4 }
 | KW_DECLSPEC TK_LBRACK sep(TK_COMMA, attrib) TK_RBRACK
  { $3 }
 | KW_FASTCALL
  { ["fastcall", []] }


paren(X):
 | TK_LBRACK X TK_RBRACK
  { $2 }
 | TK_LBRACK paren(X) TK_RBRACK
  { $2 }


attrib:
 | KW_ATTR_ALIAS paren(string_literal)
  { "alias", [$2] }
 | KW_ATTR_ALIGNED paren(numeric_literal)?
  { "aligned", singleton_list_from_opt $2 }
 | KW_ATTR_ALWAYS_INLINE
  { "always_inline", [] }
 | KW_ATTR_COMMON
  { "common", [] }
 | KW_ATTR_COLD
  { "cold", [] }
 | KW_ATTR_CONST
  { "const", [] }
 | KW_ATTR_DEPRECATED
  { "deprecated", [] }
 | KW_ATTR_DLLEXPORT
  { "dllexport", [] }
 | KW_ATTR_DLLIMPORT
  { "dllimport", [] }
 | KW_ATTR_EXTERNALLY_VISIBLE
  { "externally_visible", [] }
 | KW_ATTR_FORMAT TK_LBRACK identifier TK_COMMA numeric_literal TK_COMMA numeric_literal TK_RBRACK
  { "format", [$3; $5; $7] }
 | KW_ATTR_FORMAT_ARG paren(numeric_literal)
  { "format_arg", [$2] }
 | KW_ATTR_GNU_INLINE
  { "gnu_inline", [] }
 | KW_ATTR_HOT
  { "hot", [] }
 | KW_ATTR_INTERRUPT paren(string_literal)
  { "interrupt", [$2] }
 | KW_ATTR_MALLOC
  { "malloc", [] }
 | KW_ATTR_MAY_ALIAS
  { "may_alias", [] }
 | KW_ATTR_MIPS16
  { "mips16", [] }
 | KW_ATTR_MODE paren(identifier)
  { "mode", [$2] }
 | KW_ATTR_NOCLONE
  { "noclone", [] }
 | KW_ATTR_NOCOMMON
  { "nocommon", [] }
 | KW_ATTR_NOINLINE
  { "noinline", [] }
 | KW_ATTR_NO_INSTRUMENT_FUNCTION
  { "no_instrument_function", [] }
 | KW_ATTR_NOMIPS16
  { "nomips16", [] }
 | KW_ATTR_NONNULL paren(sep(TK_COMMA, numeric_literal))
  { "nonnull", $2 }
 | KW_ATTR_NORETURN
  { "noreturn", [] }
 | KW_ATTR_NO_SPLIT_STACK
  { "no_split_stack", [] }
 | KW_ATTR_NOTHROW
  { "nothrow", [] }
 | KW_ATTR_NOTSHARED
  { "notshared", [] }
 | KW_ATTR_PACKED
  { "packed", [] }
 | KW_ATTR_PURE
  { "pure", [] }
 | KW_ATTR_SECTION paren(string_literal)
  { "section", [$2] }
 | KW_ATTR_SENTINEL
  { "sentinel", [] }
 | KW_ATTR_TRANSPARENT_UNION
  { "transparent_union", [] }
 | KW_ATTR_UNUSED
  { "unused", [] }
 | KW_ATTR_USED
  { "used", [] }
 | KW_ATTR_VECTOR_SIZE TK_LBRACK assignment_expression TK_RBRACK
  { "vector_size", [$3] }
 | KW_ATTR_VISIBILITY paren(string_literal)
  { "visibility", [$2] }
 | KW_ATTR_WARN_UNUSED_RESULT
  { "warn_unused_result", [] }
 | KW_ATTR_WEAK
  { "weak", [] }

 | KW_ATTR_CDECL
  { "cdecl", [] }
 | KW_ATTR_STDCALL
  { "stdcall", [] }
 | KW_ATTR_THISCALL
  { "thiscall", [] }

 | KW_ATTR_ARTIFICIAL
  { "artificial", [] }
 | KW_ATTR_FORCE_ALIGN_ARG_POINTER
  { "force_align_arg_pointer", [] }
 | KW_ATTR_REGPARM paren(numeric_literal)
  { "regparm", [$2] }
 | KW_ATTR_CALLEE_POP_AGGREGATE_RETURN paren(numeric_literal)
  { "callee_pop_aggregate_return", [$2] }
 | KW_ATTR_SSEREGPARM
  { "sseregparm", [] }
 | KW_ATTR_CLEANUP paren(identifier)
  { "cleanup", [$2] }
 | KW_ATTR_FASTCALL
  { "fastcall", [] }
 | KW_ATTR_OPTIMIZE TK_LBRACK numeric_literal TK_COMMA string_literal TK_RBRACK
  { "optimize", [$3; $5] }
 | KW_ATTR_TARGET paren(string_literal)
  { "target", [$2] }
 | KW_ATTR_ALLOC_SIZE paren(numeric_literal)
  { "alloc_size", [$2] }

 | KW_ATTR_ALTIVEC
  { "altivec", [] }
 | KW_ATTR_BACK_SWITCH
  { "back_switch", [] }
 | KW_ATTR_BASED
  { "based", [] }
 | KW_ATTR_BELOW100
  { "below100", [] }
 | KW_ATTR_CB
  { "cb", [] }
 | KW_ATTR_CONSTRUCTOR
  { "constructor", [] }
 | KW_ATTR_DESTRUCTOR
  { "destructor", [] }
 | KW_ATTR_DISINTERRUPT
  { "disinterrupt", [] }
 | KW_ATTR_EIGHTBIT_DATA
  { "eightbit_data", [] }
 | KW_ATTR_EXCEPTION_HANDLER
  { "exception_handler", [] }
 | KW_ATTR_FAR
  { "far", [] }
 | KW_ATTR_FAST_INTERRUPT
  { "fast_interrupt", [] }
 | KW_ATTR_FLATTEN
  { "flatten", [] }
 | KW_ATTR_FUNCTION_VECTOR
  { "function_vector", [] }
 | KW_ATTR_GCC_STRUCT
  { "gcc_struct", [] }
 | KW_ATTR_IFUNC
  { "ifunc", [] }
 | KW_ATTR_INTERRUPT_HANDLER
  { "interrupt_handler", [] }
 | KW_ATTR_INTERRUPT_THREAD
  { "interrupt_thread", [] }
 | KW_ATTR_IO
  { "io", [] }
 | KW_ATTR_ISR
  { "isr", [] }
 | KW_ATTR_KSPISUSP
  { "kspisusp", [] }
 | KW_ATTR_L1_DATA_A
  { "l1_data_a", [] }
 | KW_ATTR_L1_DATA_B
  { "l1_data_b", [] }
 | KW_ATTR_L1_DATA
  { "l1_data", [] }
 | KW_ATTR_L1_TEXT
  { "l1_text", [] }
 | KW_ATTR_L2
  { "l2", [] }
 | KW_ATTR_LEAF
  { "leaf", [] }
 | KW_ATTR_LONG_CALL
  { "long_call", [] }
 | KW_ATTR_LONGCALL
  { "longcall", [] }
 | KW_ATTR_MODEL
  { "model", [] }
 | KW_ATTR_MS_ABI
  { "ms_abi", [] }
 | KW_ATTR_MS_HOOK_PROLOGUE
  { "ms_hook_prologue", [] }
 | KW_ATTR_MS_STRUCT
  { "ms_struct", [] }
 | KW_ATTR_NAKED
  { "naked", [] }
 | KW_ATTR_NEAR
  { "near", [] }
 | KW_ATTR_NESTING
  { "nesting", [] }
 | KW_ATTR_NMI_HANDLER
  { "nmi_handler", [] }
 | KW_ATTR_OS_MAIN
  { "os_main", [] }
 | KW_ATTR_OS_TASK
  { "os_task", [] }
 | KW_ATTR_PCS
  { "pcs", [] }
 | KW_ATTR_PROGMEM
  { "progmem", [] }
 | KW_ATTR_RESBANK
  { "resbank", [] }
 | KW_ATTR_RETURNS_TWICE
  { "returns_twice", [] }
 | KW_ATTR_SAVEALL
  { "saveall", [] }
 | KW_ATTR_SAVE_VOLATILES
  { "save_volatiles", [] }
 | KW_ATTR_SELECTANY
  { "selectany", [] }
 | KW_ATTR_SHARED
  { "shared", [] }
 | KW_ATTR_SHORT_CALL
  { "short_call", [] }
 | KW_ATTR_SHORTCALL
  { "shortcall", [] }
 | KW_ATTR_SIGNAL
  { "signal", [] }
 | KW_ATTR_SP_SWITCH
  { "sp_switch", [] }
 | KW_ATTR_SPU_VECTOR
  { "spu_vector", [] }
 | KW_ATTR_SYSCALL_LINKAGE
  { "syscall_linkage", [] }
 | KW_ATTR_SYSV_ABI
  { "sysv_abi", [] }
 | KW_ATTR_TINY_DATA
  { "tiny_data", [] }
 | KW_ATTR_TINY
  { "tiny", [] }
 | KW_ATTR_TLS_MODEL
  { "tls_model", [] }
 | KW_ATTR_TRAP_EXIT
  { "trap_exit", [] }
 | KW_ATTR_VERSION_ID
  { "version_id", [] }
 | KW_ATTR_VLIW
  { "vliw", [] }
 | KW_ATTR_WEAKREF
  { "weakref", [] }
