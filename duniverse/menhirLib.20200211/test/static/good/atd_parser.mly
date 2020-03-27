/*
   ATD Parser

   requires menhir.
*/
%{
  open Printf
  open Atd_ast

  let syntax_error s pos1 pos2 =
    let msg = sprintf "%s:\n%s" (string_of_loc (pos1, pos2)) s in
    error msg
%}

%token TYPE EQ OP_PAREN CL_PAREN OP_BRACK CL_BRACK OP_CURL CL_CURL
       SEMICOLON COMMA COLON STAR OF EOF BAR LT GT INHERIT
       QUESTION TILDE
%token < string > STRING LIDENT UIDENT TIDENT

%start full_module
%type < Atd_ast.full_module > full_module
%%

full_module:
| x = annot  y = module_body { ((($startpos(x), $endpos(x)), x), y) }
;

module_body:
| module_item module_body   { $1 :: $2 }
| EOF                       { [] }
| _e=error     { syntax_error "Syntax error" $startpos(_e) $endpos(_e) }
;

annot:
| x = asection l = annot { x :: l }
|                        { ([] : annot) }
;

asection:
| LT x = LIDENT l = afield_list GT  { (x, (($startpos, $endpos), l)) }
| LT LIDENT afield_list _e=error    { syntax_error
                                        "Expecting '>'"
                                        $startpos(_e) $endpos(_e) }
| LT _e=error                       { syntax_error
                                        "Expecting lowercase identifier"
                                        $startpos(_e) $endpos(_e) }
;

afield_list:
| x = afield l = afield_list  { x :: l }
|                             { [] }
;

afield:
| LIDENT EQ STRING  { ($1, (($startpos, $endpos), Some $3)) }
| LIDENT            { ($1, (($startpos, $endpos), None)) }
;

module_item:

| TYPE p = type_param s = LIDENT a = annot EQ t = type_expr
                               { `Type (($startpos, $endpos), (s, p, a), t) }

| TYPE type_param LIDENT annot EQ _e=error
    { syntax_error "Expecting type expression" $startpos(_e) $endpos(_e) }
| TYPE type_param LIDENT annot _e=error
    { syntax_error "Expecting '='" $startpos(_e) $endpos(_e) }
| TYPE _e=error
    { syntax_error "Expecting type name" $startpos(_e) $endpos(_e) }
;

type_param:
| TIDENT                            { [ $1 ] }
| OP_PAREN type_var_list CL_PAREN   { $2 }
|                                   { [] }
| OP_PAREN type_var_list _e=error
    { syntax_error "Expecting ')'" $startpos(_e) $endpos(_e) }
;

type_var_list:
| TIDENT COMMA type_var_list   { $1 :: $3 }
| TIDENT                       { [ $1 ] }
;

type_expr:
| OP_BRACK l = variant_list CL_BRACK a = annot
     { `Sum (($startpos, $endpos), l, a) }
| OP_BRACK CL_BRACK a = annot
     { `Sum (($startpos, $endpos), [], a) }

| OP_CURL l = field_list CL_CURL a = annot
     { `Record (($startpos, $endpos), l, a) }
| OP_CURL CL_CURL a = annot
     { `Record (($startpos, $endpos), [], a) }

| OP_PAREN x = annot_expr CL_PAREN a = annot
     { `Tuple (($startpos, $endpos), [x], a) }

| OP_PAREN l = cartesian_product CL_PAREN a = annot
     { `Tuple (($startpos, $endpos), l, a) }

| x = type_inst a = annot
     { let pos1 = $startpos in
       let pos2 = $endpos in
       let loc = (pos1, pos2) in
       let loc2, name, args = x in
       match name, args with
           "list", [x] -> `List (loc, x, a)
         | "option", [x] -> `Option (loc, x, a)
         | "nullable", [x] -> `Nullable (loc, x, a)
         | "shared", [x] ->
             let a =
               if Atd_annot.has_field ["share"] "id" a then
                 (* may cause ID clashes if not used properly *)
                 a
               else
                 Atd_annot.set_field loc
                   "share" "id" (Some (Atd_annot.create_id ())) a
             in
             `Shared (loc, x, a)
         | "wrap", [x] -> `Wrap (loc, x, a)

         | ("list"|"option"|"nullable"|"shared"|"wrap"), _ ->
             syntax_error (sprintf "%s expects one argument" name) pos1 pos2

         | _ -> (`Name (loc, x, a) : type_expr) }

| x = TIDENT
     { `Tvar (($startpos, $endpos), x) }
| OP_BRACK variant_list _e=error
     { syntax_error "Expecting ']'" $startpos(_e) $endpos(_e) }
| OP_CURL field_list _e=error
     { syntax_error "Expecting '}'" $startpos(_e) $endpos(_e) }
| OP_PAREN cartesian_product _e=error
     { syntax_error "Expecting ')'" $startpos(_e) $endpos(_e) }
;

cartesian_product:
| x = annot_expr STAR l = cartesian_product   { x :: l }
| x = annot_expr STAR y = annot_expr          { [ x; y ] }
|                                             { [] }
;

annot_expr:
| a = annot COLON x = type_expr    { (($startpos, $endpos), x, a) }
| x = type_expr                    { (($startpos, $endpos), x, []) }
;

type_inst:
| l = type_args s = LIDENT  { (($startpos, $endpos), s, l) }
;

type_args:
| type_expr                         { [ $1 ] }
| OP_PAREN type_arg_list CL_PAREN   { $2 }
|                                   { [] }
| OP_PAREN type_arg_list _e=error
     { syntax_error "Expecting ')'" $startpos(_e) $endpos(_e) }
;

type_arg_list:
| type_expr COMMA type_arg_list  { $1 :: $3 }
| type_expr COMMA type_expr      { [ $1; $3 ] }
;

variant_list:
| BAR variant_list0 { $2 }
| variant_list0     { $1 }
;

variant_list0:
| variant BAR variant_list0  { $1 :: $3 }
| variant                    { ([ $1 ] : variant list) }
;

variant:
| x = UIDENT a = annot OF t = type_expr
     { `Variant (($startpos, $endpos), (x, a), Some t) }
| x = UIDENT a = annot
     { `Variant (($startpos, $endpos), (x, a), None) }
| INHERIT t = type_expr
     { `Inherit (($startpos, $endpos), t) }
| UIDENT annot OF _e=error
     { syntax_error "Expecting type expression after 'of'"
         $startpos(_e) $endpos(_e) }
;

field_list:
| x = field SEMICOLON l = field_list   { x :: l }
| x = field SEMICOLON                  { [ x ] }
| x = field                            { [ x ] }
;

field:
| fn = field_name a = annot COLON t = type_expr
    { let k, fk = fn in
      `Field (($startpos, $endpos), (k, fk, a), t) }
| INHERIT t = type_expr
    { `Inherit (($startpos, $endpos), t) }
| field_name annot COLON _e=error
    { syntax_error "Expecting type expression after ':'"
        $startpos(_e) $endpos(_e) }
| field_name annot _e=error
    { syntax_error "Expecting ':'" $startpos(_e) $endpos(_e) }
;

field_name:
| k = LIDENT             { (k, `Required) }
| QUESTION k = LIDENT    { (k, `Optional) }
| TILDE k = LIDENT       { (k, `With_default) }
;
