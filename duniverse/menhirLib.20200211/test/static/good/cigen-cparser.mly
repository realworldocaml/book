/* CIGEN
 *
 * C interface generator for Objective CAML.
 * (C) 1997 by Gerd Stolpmann
 *
 * Module: syntactic analysis of C text (only subset for *.h files)
 * Created: July, 12th 1997
 * Modified: -
 */


%{
  (* header *)
  open Ctypes
  open Cparseraux

  let check_for_typedefs d =
  ( if List.mem S_typedef (fst(d.d_specs)) then
    (  let n = (List.map name_of_declarator (d.d_decl)) in
          List.iter (fun x-> print_endline ("new typedef: "^x)) n;
          flush stdout;
          typedef_names := n @ !typedef_names
    )
  )

%}


/**** declarations ****/

/* keywords */

%token KW_auto
%token KW_break
%token KW_case
%token KW_char
%token KW_const
%token KW_continue
%token KW_default
%token KW_do
%token KW_double
%token KW_else
%token KW_enum
%token KW_extern
%token KW_float
%token KW_for
%token KW_goto
%token KW_if
%token KW_int
%token KW_long
%token KW_register
%token KW_return
%token KW_short
%token KW_signed
%token KW_sizeof
%token KW_static
%token KW_struct
%token KW_switch
%token KW_typedef
%token KW_union
%token KW_unsigned
%token KW_void
%token KW_volatile
%token KW_while

/* symbols */

%token KW_add
%token KW_sub
%token KW_times
%token KW_div
%token KW_mod
%token KW_point
%token KW_access
%token KW_test
%token KW_comma
%token KW_semicolon
%token KW_add_to
%token KW_sub_from
%token KW_complement
%token KW_not
%token KW_assign
%token KW_assign_times
%token KW_assign_div
%token KW_assign_mod
%token KW_assign_add
%token KW_assign_sub
%token KW_assign_left
%token KW_assign_right
%token KW_assign_and
%token KW_assign_exor
%token KW_assign_or
%token KW_lazy_or
%token KW_lazy_and
%token KW_or
%token KW_and
%token KW_exor
%token KW_equal
%token KW_unequal
%token KW_left
%token KW_right
%token KW_lower
%token KW_greater
%token KW_lowereq
%token KW_greatereq
%token KW_andsoon
%token KW_colon
%token KW_oparen
%token KW_cparen
%token KW_obracket
%token KW_cbracket
%token KW_obrace
%token KW_cbrace

/* other */

%token < int > Integer_unsuffixed
%token < int > Integer_long
%token < int > Integer_unsigned
%token < float > Double
%token < float > Double_long
%token < float > Float
%token < string > Chars_raw
%token < string > Chars_extended_raw
%token < string > String_raw
%token < string > String_extended_raw
%token < string > Ident
%token < string > Typedef_ident
  /* all idents which are known to denote types must be converted into
   * typedef_idents; the reason for this that the grammar would otherwise
   * be ambigous.
   */

%token Nulltoken Endofstream

%type < unit > nothing
%type < Ctypes.declaration list > translation_unit

%start nothing translation_unit

%%

/**** rules ****/

nothing :
  Endofstream { () }

translation_unit :
    declaration Endofstream
      { [$1] }
  | declaration translation_unit
      { $1 :: $2 }

declaration :
    declaration_specifiers init_declarator_list KW_semicolon
      { let d = {d_specs=$1; d_decl=$2} in
          check_for_typedefs d;
          d
      }
  | declaration_specifiers KW_semicolon
      { let d = {d_specs=$1; d_decl=[]} in
          check_for_typedefs d;
          d
      }

declaration_specifiers :
    storage_class_specifier declaration_specifiers
      { ($1::(fst $2), snd $2) }
  | storage_class_specifier
      { ([$1], ([],[])) }
  | type_specifier declaration_specifiers
      { (fst $2, ($1::fst(snd $2),snd (snd $2)) ) }
  | type_specifier
      { ([],([$1],[])) }
  | type_qualifier declaration_specifiers
      { (fst $2, (fst(snd $2), $1::snd(snd $2))) }
  | type_qualifier
      { ([],([],[$1])) }

storage_class_specifier :
    KW_auto     { S_auto }
  | KW_register { S_register }
  | KW_static   { S_static }
  | KW_extern   { S_extern }
  | KW_typedef  { S_typedef }

type_specifier :
    KW_void                     { T_void }
  | KW_char			{ T_char }
  | KW_short			{ T_short }
  | KW_int			{ T_int }
  | KW_long			{ T_long }
  | KW_float			{ T_float }
  | KW_double			{ T_double }
  | KW_signed			{ T_signed }
  | KW_unsigned			{ T_unsigned }
  | struct_or_union_specifier	{ T_struct $1 }
  | enum_specifier		{ T_enum $1 }
  | typedef_name		{ T_typedef $1 }

type_qualifier :
    KW_const    { Q_const }
  | KW_volatile { Q_volatile }

struct_or_union_specifier :
    struct_or_union Ident KW_obrace struct_declaration_list KW_cbrace
      { {struct_su = $1; struct_tag = Some $2; struct_lst = $4 }}
  | struct_or_union Typedef_ident KW_obrace struct_declaration_list KW_cbrace
      { {struct_su = $1; struct_tag = Some $2; struct_lst = $4 }}
  | struct_or_union KW_obrace struct_declaration_list KW_cbrace
      { {struct_su = $1; struct_tag = None; struct_lst = $3 }}
  | struct_or_union Ident
      { {struct_su = $1; struct_tag = Some $2; struct_lst = [] }}
  | struct_or_union Typedef_ident
      { {struct_su = $1; struct_tag = Some $2; struct_lst = [] }}

  /* note: typedef names and tag names are in different namespaces. So it
   * is permitted to use a name which has been recognized as typedef name
   * as a tag name, too.
   */


struct_or_union :
    KW_struct { Is_struct }
  | KW_union  { Is_union }

struct_declaration_list :
    struct_declaration
      { [$1] }
  | struct_declaration struct_declaration_list
      { $1 :: $2 }

init_declarator_list :
    declarator
      { [$1] }
  | declarator KW_comma init_declarator_list
      { $1 :: $3 }

struct_declaration :
    specifier_qualifier_list struct_declarator_list KW_semicolon
      { {s_specs = $1; s_decl = $2} }


specifier_qualifier_list :
    type_specifier
      { ([$1],[]) }
  | type_specifier specifier_qualifier_list
      { ($1::(fst $2),snd $2) }
  | type_qualifier
      { ([],[$1]) }
  | type_qualifier specifier_qualifier_list
     { (fst $2,$1::(snd $2)) }

struct_declarator_list :
    declarator
      { [$1] }
  | declarator  KW_comma  struct_declarator_list
      { $1 :: $3 }


enum_specifier :
    KW_enum Ident KW_obrace enumerator_list KW_cbrace
      { {enum_tag = Some $2; enum_labels =$4 }}
  | KW_enum Typedef_ident KW_obrace enumerator_list KW_cbrace
      { {enum_tag = Some $2; enum_labels =$4 }}
  | KW_enum KW_obrace enumerator_list KW_cbrace
      { {enum_tag = None; enum_labels = $3} }
  | KW_enum Ident
      { {enum_tag = Some $2; enum_labels = [] } }
  | KW_enum Typedef_ident
      { {enum_tag = Some $2; enum_labels = [] } }

  /* note: typedef names and tag names are in different namespaces. So it
   * is permitted to use a name which has been recognized as typedef name
   * as a tag name, too.
   */

enumerator_list :
    enumerator
      { [$1] }
  | enumerator KW_comma enumerator_list
      { $1 :: $3 }

enumerator :
    Ident
      { ($1,None) }
  | Ident KW_assign Integer_unsuffixed
      { ($1,Some $3) }
    /* subset */

declarator :
    direct_declarator
      { $1 }
  | pointer direct_declarator
      { D_pointer ($2,$1) }

direct_declarator :
    Ident
      { D_name $1 }
  | KW_oparen declarator KW_cparen
      { $2 }
  | direct_declarator KW_obracket KW_cbracket
      { D_openarray $1 }
  | direct_declarator KW_obracket Integer_unsuffixed KW_cbracket
      { D_array ($1,$3) }
    /* subset */
  | direct_declarator KW_oparen parameter_type_list KW_cparen
      { D_function ($1,$3) }
    /* old C functions not supported */

pointer :
    KW_times type_qualifier_list
      { [$2] }
  | KW_times
      { [[]] }
  | KW_times type_qualifier_list pointer
      { $2 :: $3 }
  | KW_times pointer
      { [] :: $2 }

type_qualifier_list :
    type_qualifier
      { [$1] }
  | type_qualifier type_qualifier_list
      { $1 :: $2 }


parameter_type_list :
    parameter_list
      { $1 }
  | parameter_list KW_comma KW_andsoon
      { $1 @ [P_var] }

parameter_list :
    parameter_declaration
      { [$1] }
  | parameter_list KW_comma  parameter_declaration
      { $1 @  [$3] }

parameter_declaration :
    declaration_specifiers declarator
      { P_decl ($1,$2) }
  | declaration_specifiers abstract_declarator
      { P_abs ($1,$2) }
  | declaration_specifiers
      { P_null $1 }

abstract_declarator :
    pointer
      { D_abspointer $1 }
  | pointer direct_abstract_declarator
      { D_pointer ($2,$1) }
  | direct_abstract_declarator
      { $1 }

direct_abstract_declarator :
    KW_oparen abstract_declarator KW_cparen
      { $2 }
  | direct_abstract_declarator KW_obracket KW_cbracket
      { D_openarray $1 }
  | direct_abstract_declarator KW_obracket Integer_unsuffixed KW_cbracket
      { D_array ($1,$3) }
    /* subset */
  | KW_obracket KW_cbracket
      { D_absopenarray }
  | KW_obracket Integer_unsuffixed KW_cbracket
      { D_absarray $2 }
    /* subset */
  | direct_abstract_declarator KW_oparen parameter_type_list KW_cparen
      { D_function ($1,$3) }
  | direct_abstract_declarator KW_oparen KW_cparen
      { D_function ($1,[]) }
  | KW_oparen parameter_type_list KW_cparen
      { D_absfunction $2 }
  | KW_oparen KW_cparen
      { D_absfunction [] }

typedef_name :
    Typedef_ident
       { $1 }


%%

(**** trailer ****)
