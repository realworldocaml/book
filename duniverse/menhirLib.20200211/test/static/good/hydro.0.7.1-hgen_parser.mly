(* Original file: hydro.0.7.1/hydro-0.7.1/src/hydrogen/hgen_parser.mly *)
%{ (* $Id: hgen_parser.mly 15582 2007-12-06 23:56:27Z gerd $ *)
  (* Header *)
  open Hgen_types
  open Hgen_types.AST
%}

/* Keywords */

%token<Hgen_types.loc> K_BOOL K_ENUM K_IMPLEMENTS K_MODULE K_STRUCT K_BYTE
%token<Hgen_types.loc> K_EXCEPTION K_THROWS K_CLASS K_EXTENDS
%token<Hgen_types.loc> K_INTERFACE K_OUT K_TRUE K_CONST K_FALSE K_LOCAL
%token<Hgen_types.loc> K_SEQUENCE K_VOID K_DICTIONARY K_SHORT
%token<Hgen_types.loc> K_DOUBLE K_IDEMPOTENT K_LONG K_STRING K_INT K_FLOAT
 
/* Other tokens */

%token<string * Hgen_types.loc> IDENT
%token<string * Hgen_types.loc * Hgen_types.loc> STRING_LITERAL
%token<int64 * Hgen_types.loc> INT_LITERAL
%token<float * Hgen_types.loc> FLOAT_LITERAL
%token<Hgen_types.loc> LBRACE RBRACE LANGLE RANGLE LPAREN RPAREN
%token<Hgen_types.loc> LBRACK RBRACK LDBRACK RDBRACK
%token<Hgen_types.loc> SEMI COMMA ASTERISK DCOLON EQUAL EOF

/* Entry points */

%type<Hgen_types.AST.def list> start
%start start

%%

start: defs EOF { $1 }

/**********************************************************************/
/* Global definitions                                                 */
/**********************************************************************/

defs: 
  global_meta_data defs
    { (`GMeta $1) :: $2 }
| def SEMI defs
    { $1 :: $3 }
| 
    { [] }

def:
  meta_data_opt K_MODULE IDENT LBRACE defs RBRACE
    { `Module
	( object
	    method name = fst $3
	    method meta = $1
	    method term = $5
	    method loc = snd $3
	  end : module_def
	)
    }
| meta_data_opt local_flag K_CLASS IDENT class_def_opt
    { `Class
	( object
	    method name = fst $4
	    method meta = $1
	    method local = $2
	    method term = $5
	    method loc = snd $4
	  end : class_def
	)
    }
| meta_data_opt local_flag K_INTERFACE IDENT intf_def_opt
   { `Intf 
       ( object
	   method name = fst $4
	   method meta = $1
	   method local = $2
	   method term = $5
	   method loc = snd $4
	 end : intf_def
       )
   }
| meta_data_opt local_flag K_EXCEPTION IDENT exn_def_opt
   { `Exn
       ( object
	   method name = fst $4
	   method meta = $1
	   method local = $2
	   method term = $5
	   method loc = snd $4
	 end : exn_def
       )
   }
| meta_data_opt local_flag K_STRUCT IDENT struct_def_opt
   { `Struct
       ( object
	   method name = fst $4
	   method meta = $1
	   method local = $2
	   method term = $5
	   method loc = snd $4
	 end : struct_def
       )
   }
| meta_data_opt local_flag K_SEQUENCE LANGLE typ RANGLE IDENT
   { let (arg_typ, arg_meta) = $5 in
     `Seq 
       ( object
	   method name = fst $7
	   method arg_typ = arg_typ
	   method arg_meta = arg_meta
	   method meta = $1
	   method local = $2
	   method loc = snd $7
	 end : seq_def
       )
   }
| meta_data_opt local_flag K_DICTIONARY LANGLE typ COMMA typ RANGLE IDENT
  { let (arg_typ1, arg_meta1) = $5 in
    let (arg_typ2, arg_meta2) = $7 in
    `Dict 
       ( object
	   method name = fst $9
	   method arg_typ1 = arg_typ1
	   method arg_typ2 = arg_typ2
	   method arg_meta1 = arg_meta1
	   method arg_meta2 = arg_meta2
	   method meta = $1
	   method local = $2
	   method loc = snd $9
	 end : dict_def
       )
  }
| meta_data_opt local_flag K_ENUM IDENT LBRACE enum_list RBRACE
   { `Enum 
       ( object
	   method name = fst $4
	   method meta = $1
	   method local = $2
	   method term = $6
	   method loc = snd $4
	 end : enum_def
       )
   }
| meta_data_opt K_CONST typ IDENT EQUAL const_value
   { let (arg_typ, arg_meta) = $3 in
     `Const
       ( object
	   method name = fst $4
	   method arg_typ = arg_typ
	   method arg_meta = arg_meta
	   method arg_value = $6
	   method meta = $1
	   method loc = snd $4
	 end : const_def
       )
   }

/**********************************************************************/
/* Class definitions                                                  */
/**********************************************************************/

class_def_opt:
  single_extends_clause_opt implements_clause_opt LBRACE class_members RBRACE
   { let members = $4 in
     let data_members =
       List.flatten
	 (List.map
	    (function `Data_member m -> [m] | _ -> [])
	    members) in
     let operations =
       List.flatten
	 (List.map
	    (function `Operation m -> [m] | _ -> [])
	    members) in
     Some 
       ( object
	   method extends = $1
	   method implements = $2
	   method data_members = data_members
	   method operations = operations
	 end : class_term
       )
   }
|
   { None }

single_extends_clause_opt:
  K_EXTENDS name
     { Some $2 }
|
     { None }

implements_clause_opt:
  K_IMPLEMENTS name_list
     { $2 }
|
     { [] }

class_members:
  class_member class_members
    { $1 :: $2 }
| 
    { [] }
  
class_member:
  data_or_operation_member
    { $1 }

/* does not work for class_member:
	  data_member
	    { `Data_member $1 }
	| operation_member
	    { `Operation $1 }
*/

/**********************************************************************/
/* Interface definitions                                              */
/**********************************************************************/

intf_def_opt:
  intf_extends_clause_opt  LBRACE intf_members RBRACE
    { Some
	( object
	    method extends = $1
	    method operations = $3
	  end : intf_term
	)
    }
|
    { None }

intf_extends_clause_opt:
  K_EXTENDS name_list
    { $2 }
|
    { [] }

intf_members:
  operation_member intf_members
    { $1 :: $2 }
| 
    { [] }

/**********************************************************************/
/* Exception definitions                                              */
/**********************************************************************/

exn_def_opt:
  single_extends_clause_opt LBRACE exn_members RBRACE
    { Some
	( object
	    method extends = $1
	    method data_members = $3
	  end : exn_term
	)
    }
|
    { None }

exn_members:
  data_member exn_members
    { $1 :: $2 }
| 
    { [] }

/**********************************************************************/
/* Structure definitions                                              */
/**********************************************************************/

struct_def_opt:
  LBRACE struct_members RBRACE
    { Some
	( object
	    method data_members = $2
	  end : struct_term
	)
    }
|
    { None }

struct_members:
  data_member struct_members
    { $1 :: $2 }
| 
    { [] }

/**********************************************************************/
/* Enumeration definitions                                            */
/**********************************************************************/

enum_list:
  IDENT
    { [ fst $1 ] }
| IDENT COMMA enum_list
    { fst $1 :: $3 }

/**********************************************************************/
/* Members                                                            */
/**********************************************************************/

data_member:
  typ IDENT SEMI
    { let (typ, meta) = $1 in
      ( object
	  method name = fst $2
	  method typ = typ
	  method meta = meta
	  method loc = snd $2
	end : data_member
      ) 
    }

operation_member:
  op_typ IDENT LPAREN params RPAREN throws_clause_opt SEMI
    { let (typ, meta, idempotent) = $1 in
      ( object
	  method name = fst $2
	  method typ = typ
	  method meta = meta
	  method params = $4
	  method throws = $6
	  method idempotent = idempotent
	  method loc = snd $2
	end : operation
      ) 
    }

data_or_operation_member:
  op_typ IDENT data_or_operation_cont
    { let (typ, meta, idempotent) = $1 in
      match $3 with
	| None ->
	    if idempotent then raise Parsing.Parse_error;
	    `Data_member(object
			   method name = fst $2
			   method typ = typ
			   method meta = meta
			   method loc = snd $2
			 end : data_member
			) 
	| Some (params, throws) ->
	    `Operation( object
			  method name = fst $2
			  method typ = typ
			  method meta = meta
			  method params = params
			  method throws = throws
			  method idempotent = idempotent
			  method loc = snd $2
			end : operation
		      ) 
    }

data_or_operation_cont:
  SEMI
    { None }
| LPAREN params RPAREN throws_clause_opt SEMI
    { Some($2, $4) }


params:
  params1
    { $1 }
|
    { [] }

params1:
  param COMMA params1
    { $1 :: $3 }
| param
    { [ $1 ] }

param:
  out_flag typ IDENT
    { let (typ, meta) = $2 in
      ( object
	  method name = fst $3
	  method typ = typ
	  method meta = meta
	  method out = $1
	  method loc = snd $3
	end : param
      )
    }

out_flag:
  K_OUT
    { true }
|
    { false }

throws_clause_opt:
  K_THROWS name_list
    { $2 }
|
    { [] }

/**********************************************************************/
/* Types                                                              */
/**********************************************************************/

typ:
  meta_data_opt typ_name
    { ( $2, $1 ) }

op_typ:
  meta_data_opt idempotent_flag typ_name
    { ( $3, $1, $2 ) }

typ_name:
  K_BYTE
    { `Byte }
| K_BOOL
    { `Bool }
| K_SHORT
    { `Short }
| K_INT
    { `Int }
| K_LONG
    { `Long }
| K_FLOAT
    { `Float }
| K_DOUBLE
    { `Double }
| K_STRING
    { `String }
| K_VOID
    { `Void }
| name
    { `Name $1 }
| name ASTERISK
    { `Proxy $1 }

idempotent_flag:
  K_IDEMPOTENT
    { true }
|
    { false }

/**********************************************************************/
/* Qualifiers                                                         */
/**********************************************************************/

meta_data_opt:
  LBRACK string_list RBRACK
    { List.map Hgen_parser_util.parse_meta_def $2 }
| 
    { [] }

global_meta_data:
  LDBRACK string_list RDBRACK
    { List.map Hgen_parser_util.parse_meta_def $2 }

string_list:
  string_lit COMMA string_list
    { $1 :: $3 }
| string_lit
    { [ $1 ] }

string_lit:
  STRING_LITERAL string_lit
    { let (s, _, _) = $1 in s ^ $2 }
| STRING_LITERAL 
    { let (s, _, _) = $1 in s }

local_flag:
  K_LOCAL
    { true }
|
    { false }


/**********************************************************************/
/* Constants                                                          */
/**********************************************************************/

const_value:
  INT_LITERAL
    { `Int (fst $1) }
| FLOAT_LITERAL
    { `Float (fst $1) }
| STRING_LITERAL
    { let (s, _, _) = $1 in `String s }
| K_FALSE
    { `Bool false }
| K_TRUE
    { `Bool true }
| name
    { `Name $1 }

/**********************************************************************/
/* Names                                                              */
/**********************************************************************/

name:
  DCOLON rel_name
    { `Absolute $2 }
| rel_name
    { `Relative $1 }

rel_name:
  IDENT
    { [ fst $1 ] }
| IDENT DCOLON rel_name
    { ( fst $1) :: $3 }

name_list:
  name
    { [ $1 ] }
| name COMMA name_list
    { $1 :: $3 }

%%
  (* Trailer *)
